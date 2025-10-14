-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Streaming AEAD Implementation
-- Permanent streaming engine for all file sizes
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- File I/O not provable

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Directories;
with Interfaces; use Interfaces;
with Anubis_Types.Classical;
with Anubis_Types.PQC;
with Anubis_Types.Header_AAD;
with Anubis_Types.Finalize;
with Anubis_Trust;
with Anubis_Constants; use Anubis_Constants;
with Anubis_Entropy;
with Sodium_Common;
with Interfaces.C;

package body Anubis_Types.Streaming is

   -------------------------------------------------------------------------
   -- Helpers: Big-Endian U64 Conversion
   -------------------------------------------------------------------------

   type U64 is new Interfaces.Unsigned_64;

   procedure U64_To_BE_Bytes (Value : U64; Output : out Byte_Array) with
      Pre => Output'Length = 8
   is
      V : U64 := Value;
   begin
      for I in reverse Output'Range loop
         Output (I) := Byte (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;
   end U64_To_BE_Bytes;

   function BE_Bytes_To_U64 (Input : Byte_Array) return U64 with
      Pre => Input'Length = 8
   is
      Result : U64 := 0;
   begin
      for I in Input'Range loop
         Result := Shift_Left (Result, 8) or U64 (Input (I));
      end loop;
      return Result;
   end BE_Bytes_To_U64;

   -------------------------------------------------------------------------
   -- Helper: Construct Nonce (file_nonce16 || chunk_index_u64_be)
   -------------------------------------------------------------------------

   procedure Make_Chunk_Nonce (
      File_Nonce16 : Byte_Array;
      Chunk_Index  : Natural;
      Nonce24      : out XChaCha20_Nonce
   ) with
      Pre => File_Nonce16'Length = 16
   is
      Index_Bytes : Byte_Array (1 .. 8);
   begin
      -- First 16 bytes: file nonce
      Nonce24.Data (1 .. 16) := File_Nonce16;

      -- Last 8 bytes: chunk index as big-endian u64
      U64_To_BE_Bytes (U64 (Chunk_Index), Index_Bytes);
      Nonce24.Data (17 .. 24) := Index_Bytes;
   end Make_Chunk_Nonce;

   -------------------------------------------------------------------------
   -- Streaming Encrypt
   -------------------------------------------------------------------------

   procedure Encrypt_File_Streaming (
      Input_Path      : in     String;
      Output_Path     : in     String;
      X25519_PK       : in     X25519_Public_Key;
      ML_KEM_PK       : in     ML_KEM_Public_Key;
      Ed25519_SK      : in     Ed25519_Secret_Key;
      ML_DSA_SK       : in     ML_DSA_Secret_Key;
      Signer_Label_Data    : in     Signer_Label;
      Signer_Timestamp     : in     Unsigned_64;
      Signer_Fingerprint_Data : in  Signer_Fingerprint;
      Result          : out    Result_Code;
      Chunk_Size      : in     Natural := 67_108_864
   ) is
      use Interfaces.C;
      use Ada.Directories;

      -- File handles
      Input_File  : File_Type;
      Output_File : File_Type;

      -- Crypto keys
      Ephemeral_X25519_PK : X25519_Public_Key;
      Ephemeral_X25519_SK : X25519_Secret_Key;
      ML_KEM_CT           : ML_KEM_Ciphertext;
      Hybrid_Secret       : PQC.Hybrid_Shared_Secret;
      Encryption_Key      : XChaCha20_Key;

      -- File metadata
      File_Nonce16   : Byte_Array (1 .. 16);
      Total_Size     : File_Size;
      Bytes_Processed : File_Size := 0;
      Chunk_Index    : Natural := 0;

      -- Chunk buffers (heap allocated)
      type Chunk_Buffer_Access is access Stream_Element_Array;
      type Byte_Buffer_Access is access Byte_Array;

      Stream_Chunk  : Chunk_Buffer_Access;
      Plain_Chunk   : Byte_Buffer_Access;
      Cipher_Chunk  : Byte_Buffer_Access;

      Op_Success : Boolean;
      File_Stream : Stream_Access;
      Output_Stream : Stream_Access;
      Computed_AAD : Byte_Array (1 .. 32);

   begin
      -- Step 1: Open input file and get size
      begin
         Open (Input_File, In_File, Input_Path);
      exception
         when others =>
            Result := IO_Error;
            return;
      end;

      -- Allocate chunk buffers on heap (after file is opened)
      begin
         Stream_Chunk := new Stream_Element_Array (1 .. Stream_Element_Offset (Chunk_Size));
         Plain_Chunk := new Byte_Array (1 .. Chunk_Size);
         Cipher_Chunk := new Byte_Array (1 .. Chunk_Size);
      exception
         when others =>
            Close (Input_File);
            Result := IO_Error;
            return;
      end;

      Total_Size := File_Size (Size (Input_File));  -- Convert Stream_IO.Count to File_Size
      File_Stream := Stream (Input_File);

      -- Step 2: Perform hybrid key exchange
      PQC.Hybrid_Encapsulate (
         X25519_Public           => X25519_PK,
         ML_KEM_Public           => ML_KEM_PK,
         X25519_Ephemeral_Public => Ephemeral_X25519_PK,
         X25519_Ephemeral_Secret => Ephemeral_X25519_SK,
         Ciphertext              => ML_KEM_CT,
         Hybrid_Secret           => Hybrid_Secret,
         Success                 => Op_Success
      );

      if not Op_Success then
         Close (Input_File);
         Result := Crypto_Error;
         return;
      end if;

      -- Step 3: Derive encryption key
      PQC.Derive_Encryption_Key (
         Hybrid_Secret  => Hybrid_Secret,
         Encryption_Key => Encryption_Key,
         Success        => Op_Success
      );

      if not Op_Success then
         Close (Input_File);
         Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
         Result := Crypto_Error;
         return;
      end if;

      -- Step 4: Generate 16-byte file nonce
      Sodium_Common.randombytes_buf (
         buf  => File_Nonce16 (File_Nonce16'First)'Address,
         size => size_t (File_Nonce16'Length)
      );

      -- Step 4.5: Compute header AAD (binds all chunks to header)
      declare
         Computed_AAD : Byte_Array (1 .. 32);
         Partial_Path : constant String := Finalize.Partial_Name (Output_Path);
      begin

      -- Step 5: Create output file with .partial extension (for crash safety)
      begin
         Create (Output_File, Out_File, Finalize.Partial_Name (Output_Path));
      exception
         when others =>
            Close (Input_File);
            Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
            Classical.Zeroize_XChaCha20_Key (Encryption_Key);
            Result := IO_Error;
            return;
      end;

      Output_Stream := Stream (Output_File);

      -- Write header: ANUB3 (5) | version (1) | file_nonce (16) | chunk_size (8) | total_size (8) |
      --               ephemeral_x25519_pk (32) | ml_kem_ct (1568) | signer_label (64) | signer_ts (8)
      --               | signer_fingerprint (32) | ed25519_sig (64) | ml_dsa_sig (4627)
      declare
         Magic             : constant Byte_Array := (65, 78, 85, 66, 51);  -- "ANUB3"
         Version           : constant Byte := 3;
         CS_Bytes          : Byte_Array (1 .. 8);
         TS_Bytes          : Byte_Array (1 .. 8);
         Time_Bytes        : Byte_Array (1 .. 8);
         Header_To_Sign    : Byte_Array (1 .. 1_742);
         Offset            : Natural := Header_To_Sign'First;
         Hybrid_Sig        : PQC.Hybrid_Signature;
         Ed_Sig            : Ed25519_Signature;
         ML_Sig            : ML_DSA_Signature;
         Sign_Success      : Boolean;
         Zero_Ed_Sig       : Ed25519_Signature := (Data => (others => 0));
         Zero_ML_Sig       : ML_DSA_Signature  := (Data => (others => 0));
      begin
         -- Populate header buffer (without signatures)
         pragma Assert (Header_To_Sign'Length = 1_742);
         Header_To_Sign (Offset .. Offset + Magic'Length - 1) := Magic;
         Offset := Offset + Magic'Length;

         Header_To_Sign (Offset) := Version;
         Offset := Offset + 1;

         Header_To_Sign (Offset .. Offset + File_Nonce16'Length - 1) := File_Nonce16;
         Offset := Offset + File_Nonce16'Length;

         U64_To_BE_Bytes (U64 (Chunk_Size), CS_Bytes);
         Header_To_Sign (Offset .. Offset + CS_Bytes'Length - 1) := CS_Bytes;
         Offset := Offset + CS_Bytes'Length;

         U64_To_BE_Bytes (U64 (Total_Size), TS_Bytes);
         Header_To_Sign (Offset .. Offset + TS_Bytes'Length - 1) := TS_Bytes;
         Offset := Offset + TS_Bytes'Length;

         Header_To_Sign (Offset .. Offset + Ephemeral_X25519_PK.Data'Length - 1) :=
           Ephemeral_X25519_PK.Data;
         Offset := Offset + Ephemeral_X25519_PK.Data'Length;

         Header_To_Sign (Offset .. Offset + ML_KEM_CT.Data'Length - 1) := ML_KEM_CT.Data;
         Offset := Offset + ML_KEM_CT.Data'Length;

         Header_To_Sign (Offset .. Offset + Signer_Label_Data'Length - 1) := Signer_Label_Data;
         Offset := Offset + Signer_Label_Data'Length;

         U64_To_BE_Bytes (U64 (Signer_Timestamp), Time_Bytes);
         Header_To_Sign (Offset .. Offset + Time_Bytes'Length - 1) := Time_Bytes;
         Offset := Offset + Time_Bytes'Length;

         Header_To_Sign (Offset .. Offset + Signer_Fingerprint_Data'Length - 1) :=
           Signer_Fingerprint_Data;

         -- Produce hybrid signature (mandatory authenticity)
         PQC.Hybrid_Sign (
            Message     => Header_To_Sign,
            Ed25519_SK  => Ed25519_SK,
            ML_DSA_SK   => ML_DSA_SK,
            Signature   => Hybrid_Sig,
            Success     => Sign_Success
         );

         if not Sign_Success then
            Close (Input_File);
            Close (Output_File);
            Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
            Classical.Zeroize_XChaCha20_Key (Encryption_Key);
            PQC.Set_Signature_Components (Hybrid_Sig, Zero_Ed_Sig, Zero_ML_Sig);
            Result := Crypto_Error;
            return;
         end if;

         Computed_AAD := Header_AAD.Compute_Header_AAD (Header_To_Sign);

         PQC.Get_Signature_Components (
            Sig         => Hybrid_Sig,
            Ed25519_Sig => Ed_Sig,
            ML_DSA_Sig  => ML_Sig
         );

         -- Persist header and signatures
         for B of Header_To_Sign loop
            Stream_Element'Write (Output_Stream, Stream_Element (B));
         end loop;

         for B of Ed_Sig.Data loop
            Stream_Element'Write (Output_Stream, Stream_Element (B));
         end loop;

         for B of ML_Sig.Data loop
            Stream_Element'Write (Output_Stream, Stream_Element (B));
         end loop;

         -- Zeroize temporary signature buffers
         PQC.Set_Signature_Components (Hybrid_Sig, Zero_Ed_Sig, Zero_ML_Sig);
         Ed_Sig := Zero_Ed_Sig;
         ML_Sig := Zero_ML_Sig;
      end;

      -- Step 6: Process file in chunks
      pragma Assert (Computed_AAD'Length = 32);
      -- PLATINUM PROOF: Loop invariants (would be proven in pure model)
      -- Invariant 1: Chunk_Index <= (Total_Size / Chunk_Size) + 1
      -- Invariant 2: Bytes_Processed <= Total_Size
      -- Invariant 3: All prior chunks authenticated with same Computed_AAD
      -- Invariant 4: Nonce uniqueness: each chunk uses File_Nonce16 || Chunk_Index
      -- Invariant 5: Encryption_Key remains valid throughout loop
      loop
         exit when Bytes_Processed >= Total_Size;

         -- Read chunk
         declare
            Bytes_To_Read : constant Stream_Element_Count :=
              Stream_Element_Count'Min (
                Stream_Element_Count (Chunk_Size),
                Stream_Element_Count (Total_Size - Bytes_Processed)
              );
            Last : Stream_Element_Offset;
            Nonce : XChaCha20_Nonce;
            Auth_Tag : Poly1305_Tag;
         begin
            Read (File_Stream.all, Stream_Chunk (1 .. Bytes_To_Read), Last);

            if Last = 0 then
               exit;
            end if;

            -- Convert to byte array
            for I in 1 .. Natural (Last) loop
               Plain_Chunk (I) := Byte (Stream_Chunk (Stream_Element_Offset (I)));
            end loop;

            -- Construct nonce for this chunk
            Make_Chunk_Nonce (File_Nonce16, Chunk_Index, Nonce);

            -- Encrypt chunk with AAD binding
            Classical.XChaCha20_Encrypt (
               Plaintext  => Plain_Chunk (1 .. Natural (Last)),
               Key        => Encryption_Key,
               Nonce      => Nonce,
               AAD        => Computed_AAD,
               Ciphertext => Cipher_Chunk (1 .. Natural (Last)),
               Auth_Tag   => Auth_Tag,
               Success    => Op_Success
            );

            if not Op_Success then
               Close (Input_File);
               Close (Output_File);
               Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
               Classical.Zeroize_XChaCha20_Key (Encryption_Key);
               Result := Crypto_Error;
               return;
            end if;

            -- Write chunk: length (8) || tag (16) || ciphertext
            declare
               Len_Bytes : Byte_Array (1 .. 8);
            begin
               U64_To_BE_Bytes (U64 (Last), Len_Bytes);
               for B of Len_Bytes loop
                  Stream_Element'Write (Output_Stream, Stream_Element (B));
               end loop;
            end;

            for B of Auth_Tag.Data loop
               Stream_Element'Write (Output_Stream, Stream_Element (B));
            end loop;

            for I in 1 .. Natural (Last) loop
               Stream_Element'Write (Output_Stream, Stream_Element (Cipher_Chunk (I)));
            end loop;

            -- Update counters
            Bytes_Processed := Bytes_Processed + File_Size (Last);
            Chunk_Index := Chunk_Index + 1;
         end;
      end loop;

      -- Step 7: Write finalization marker and atomically rename
      if not Finalize.Write_Final_Marker (Output_File) then
         Close (Input_File);
         Close (Output_File);
         Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
         Classical.Zeroize_XChaCha20_Key (Encryption_Key);
         Result := IO_Error;
         return;
      end if;

      -- Cleanup
      Close (Input_File);
      Close (Output_File);

      -- Atomic rename from .partial to final path
      if not Finalize.Atomic_Rename (Partial_Path, Output_Path) then
         Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
         Classical.Zeroize_XChaCha20_Key (Encryption_Key);
         Result := IO_Error;
         return;
      end if;

      Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
      Classical.Zeroize_XChaCha20_Key (Encryption_Key);

      Result := Success;

      end;  -- Close declare block for Header_AAD

   exception
      when others =>
         if Is_Open (Input_File) then Close (Input_File); end if;
         if Is_Open (Output_File) then Close (Output_File); end if;
         Result := IO_Error;
   end Encrypt_File_Streaming;

   -------------------------------------------------------------------------
   -- Streaming Decrypt
   -------------------------------------------------------------------------

   procedure Decrypt_File_Streaming (
      Input_Path      : in     String;
      Output_Path     : in     String;
      X25519_SK       : in     X25519_Secret_Key;
      ML_KEM_SK       : in     ML_KEM_Secret_Key;
      Ed25519_PK      : in     Ed25519_Public_Key;
      ML_DSA_PK       : in     ML_DSA_Public_Key;
      Signer_Label_Data    : out    Signer_Label;
      Signer_Timestamp     : out   Unsigned_64;
      Signer_Fingerprint_Data : out Signer_Fingerprint;
      Result          : out    Result_Code
   ) is
      -- File handles
      Input_File  : File_Type;
      Output_File : File_Type;

      -- Crypto keys
      Ephemeral_X25519_PK : X25519_Public_Key;
      ML_KEM_CT           : ML_KEM_Ciphertext;
      Hybrid_Secret       : PQC.Hybrid_Shared_Secret;
      Decryption_Key      : XChaCha20_Key;

      -- File metadata
      File_Nonce16   : Byte_Array (1 .. 16);
      Chunk_Size_U64 : U64;
      Total_Size_U64 : U64;
      Chunk_Size     : Natural;
      Total_Size     : Natural;
      Bytes_Processed : Natural := 0;
      Chunk_Index    : Natural := 0;

      -- Buffers
      type Byte_Buffer_Access is access Byte_Array;
      Cipher_Chunk  : Byte_Buffer_Access;
      Plain_Chunk   : Byte_Buffer_Access;

      Op_Success : Boolean;
      File_Stream : Stream_Access;
      Output_Stream : Stream_Access;
      Partial_Path  : constant String := Finalize.Partial_Name (Output_Path);

   begin
      Signer_Label_Data := (others => 0);
      Signer_Fingerprint_Data := (others => 0);
      Signer_Timestamp := 0;

      -- Open input file
      begin
         Open (Input_File, In_File, Input_Path);
      exception
         when others =>
            Result := IO_Error;
            return;
      end;

      File_Stream := Stream (Input_File);

      declare
         Computed_AAD : Byte_Array (1 .. 32);
      begin
         -- Read and verify header
         declare
            Magic            : Byte_Array (1 .. 5);
            Version          : Byte;
            CS_Bytes         : Byte_Array (1 .. 8);
            TS_Bytes         : Byte_Array (1 .. 8);
            Time_Bytes       : Byte_Array (1 .. 8);
            Header_To_Verify : Byte_Array (1 .. 1_742);
            Offset           : Natural := Header_To_Verify'First;
            Ed_Sig           : Ed25519_Signature;
            ML_Sig           : ML_DSA_Signature;
            Hybrid_Sig       : PQC.Hybrid_Signature;
            Zero_Ed_Sig      : Ed25519_Signature := (Data => (others => 0));
            Zero_ML_Sig      : ML_DSA_Signature  := (Data => (others => 0));
            Verified         : Boolean;
         begin
            -- Read magic
            for I in Magic'Range loop
               Magic (I) := Byte (Stream_Element'Input (File_Stream));
               Header_To_Verify (Offset) := Magic (I);
               Offset := Offset + 1;
            end loop;

            if Magic = (65, 78, 85, 66, 50) then  -- "ANUB2"
               Close (Input_File);
               Result := Legacy_Format;
               return;
            elsif Magic /= (65, 78, 85, 66, 51) then  -- "ANUB3"
               Close (Input_File);
               Result := Invalid_Format;
               return;
            end if;

            -- Read version
            Version := Byte (Stream_Element'Input (File_Stream));
            Header_To_Verify (Offset) := Version;
            Offset := Offset + 1;
            if Version /= 3 then
               Close (Input_File);
               Result := Invalid_Format;
               return;
            end if;

            -- Read file nonce
            for I in File_Nonce16'Range loop
               File_Nonce16 (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;
            Header_To_Verify (Offset .. Offset + File_Nonce16'Length - 1) := File_Nonce16;
            Offset := Offset + File_Nonce16'Length;

            -- Read chunk size
            for I in CS_Bytes'Range loop
               CS_Bytes (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;
            Chunk_Size_U64 := BE_Bytes_To_U64 (CS_Bytes);
            Header_To_Verify (Offset .. Offset + CS_Bytes'Length - 1) := CS_Bytes;
            Offset := Offset + CS_Bytes'Length;

            -- SECURITY: Strict chunk size validation (prevents DoS via pathological allocations)
            -- Reject if not representable as Natural
            if Chunk_Size_U64 > U64 (Natural'Last) then
               Close (Input_File);
               Result := Invalid_Format;
               return;
            end if;

            Chunk_Size := Natural (Chunk_Size_U64);

            -- SECURITY: Enforce sane chunk size range (4 KiB to 1 GiB)
            -- Prevents DoS via pathological allocations (too large or too small)
            if Chunk_Size < MIN_CHUNK_SIZE or else Chunk_Size > MAX_CHUNK_SIZE then
               Close (Input_File);
               Result := Invalid_Format;
               return;
            end if;

            -- Read total size
            for I in TS_Bytes'Range loop
               TS_Bytes (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;
            Total_Size_U64 := BE_Bytes_To_U64 (TS_Bytes);
            Header_To_Verify (Offset .. Offset + TS_Bytes'Length - 1) := TS_Bytes;
            Offset := Offset + TS_Bytes'Length;

            -- SECURITY: Validate total size is representable as Natural
            if Total_Size_U64 > U64 (Natural'Last) then
               Close (Input_File);
               Result := Invalid_Format;
               return;
            end if;

            Total_Size := Natural (Total_Size_U64);

            -- Read ephemeral X25519 public key
            for I in Ephemeral_X25519_PK.Data'Range loop
               Ephemeral_X25519_PK.Data (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;
            Header_To_Verify (Offset .. Offset + Ephemeral_X25519_PK.Data'Length - 1) :=
              Ephemeral_X25519_PK.Data;
            Offset := Offset + Ephemeral_X25519_PK.Data'Length;

            -- Read ML-KEM ciphertext
            for I in ML_KEM_CT.Data'Range loop
               ML_KEM_CT.Data (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;
            Header_To_Verify (Offset .. Offset + ML_KEM_CT.Data'Length - 1) := ML_KEM_CT.Data;
            Offset := Offset + ML_KEM_CT.Data'Length;

            for I in Signer_Label_Data'Range loop
               Signer_Label_Data (I) := Byte (Stream_Element'Input (File_Stream));
               Header_To_Verify (Offset) := Signer_Label_Data (I);
               Offset := Offset + 1;
            end loop;

            for I in Time_Bytes'Range loop
               Time_Bytes (I) := Byte (Stream_Element'Input (File_Stream));
               Header_To_Verify (Offset) := Time_Bytes (I);
               Offset := Offset + 1;
            end loop;

            Signer_Timestamp := Unsigned_64 (BE_Bytes_To_U64 (Time_Bytes));

            for I in Signer_Fingerprint_Data'Range loop
               Signer_Fingerprint_Data (I) := Byte (Stream_Element'Input (File_Stream));
               Header_To_Verify (Offset) := Signer_Fingerprint_Data (I);
               Offset := Offset + 1;
            end loop;

            -- Read mandatory signatures
            for I in Ed_Sig.Data'Range loop
               Ed_Sig.Data (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;

            for I in ML_Sig.Data'Range loop
               ML_Sig.Data (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;

            PQC.Set_Signature_Components (
               Sig         => Hybrid_Sig,
               Ed25519_Sig => Ed_Sig,
               ML_DSA_Sig  => ML_Sig
            );

            -- Sanity: header preamble size invariant
            pragma Assert (Header_To_Verify'Length = 1_742);
            Verified := PQC.Hybrid_Verify (
               Message     => Header_To_Verify,
               Signature   => Hybrid_Sig,
               Ed25519_PK  => Ed25519_PK,
               ML_DSA_PK   => ML_DSA_PK
            );

            -- Zeroize hybrid signature container
            PQC.Set_Signature_Components (Hybrid_Sig, Zero_Ed_Sig, Zero_ML_Sig);
            Ed_Sig := Zero_Ed_Sig;
            ML_Sig := Zero_ML_Sig;

            if not Verified then
               Close (Input_File);
               Result := Auth_Failed;
               return;
            end if;

            Computed_AAD := Header_AAD.Compute_Header_AAD (Header_To_Verify);
         end;

         declare
            Status : constant Anubis_Trust.Trust_Status :=
              Anubis_Trust.Verify (Signer_Fingerprint_Data, Signer_Label_Data, Signer_Timestamp);
         begin
            case Status is
               when Anubis_Trust.Approved =>
                  null;
               when Anubis_Trust.Pending =>
                  Close (Input_File);
                  Result := Trust_Pending;
                  return;
               when Anubis_Trust.Denied =>
                  Close (Input_File);
                  Result := Trust_Denied;
                  return;
               when Anubis_Trust.Error =>
                  Close (Input_File);
                  Result := Trust_Error;
                  return;
            end case;
         end;

         -- Allocate buffers
         Cipher_Chunk := new Byte_Array (1 .. Chunk_Size);
         Plain_Chunk := new Byte_Array (1 .. Chunk_Size);

      -- Perform hybrid key decapsulation
      PQC.Hybrid_Decapsulate (
         X25519_Secret       => X25519_SK,
         ML_KEM_Secret       => ML_KEM_SK,
         X25519_Ephemeral    => Ephemeral_X25519_PK,
         ML_KEM_CT           => ML_KEM_CT,
         Hybrid_Secret       => Hybrid_Secret,
         Success             => Op_Success
      );

      if not Op_Success then
         Close (Input_File);
         Result := Crypto_Error;
         return;
      end if;

      -- Derive decryption key
      PQC.Derive_Encryption_Key (
         Hybrid_Secret  => Hybrid_Secret,
         Encryption_Key => Decryption_Key,
         Success        => Op_Success
      );

      if not Op_Success then
         Close (Input_File);
         Result := Crypto_Error;
         return;
      end if;

      -- SECURITY: Verify derived key has sufficient entropy
      -- Detects key derivation failures before attempting decryption
      if not Decryption_Key.Valid or else
         Anubis_Entropy.Is_All_Zeros (Decryption_Key.Data)
      then
         Close (Input_File);
         Classical.Zeroize_XChaCha20_Key (Decryption_Key);
         Result := Crypto_Error;
         return;
      end if;

      -- Create output file as a partial, then atomically rename on success
      begin
         Create (Output_File, Out_File, Partial_Path);
      exception
         when others =>
            Close (Input_File);
            Result := IO_Error;
            return;
      end;

      Output_Stream := Stream (Output_File);

      -- Process chunks
      pragma Assert (Computed_AAD'Length = 32);
      -- PLATINUM PROOF: Loop invariants (would be proven in pure model)
      -- Invariant 1: Chunk_Index <= (Total_Size / Chunk_Size) + 1
      -- Invariant 2: Bytes_Processed <= Total_Size
      -- Invariant 3: All prior chunks verified with same Computed_AAD
      -- Invariant 4: Nonce uniqueness: each chunk uses File_Nonce16 || Chunk_Index
      -- Invariant 5: Decryption_Key remains valid throughout loop
      -- Invariant 6: Any auth failure → immediate exit with Auth_Failed
      loop
         exit when Bytes_Processed >= Total_Size;

         declare
            Len_Bytes : Byte_Array (1 .. 8);
            Chunk_Len_U64 : U64;
            Chunk_Len : Natural;
            Auth_Tag : Poly1305_Tag;
            Nonce : XChaCha20_Nonce;
         begin
            -- Read chunk length
            for I in Len_Bytes'Range loop
               Len_Bytes (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;
            Chunk_Len_U64 := BE_Bytes_To_U64 (Len_Bytes);
            Chunk_Len := Natural (Chunk_Len_U64);

            -- SECURITY: Validate chunk length is within bounds
            if Chunk_Len = 0 or Chunk_Len > Chunk_Size then
               Close (Input_File);
               Close (Output_File);
               begin
                  Ada.Directories.Delete_File (Finalize.Partial_Name (Output_Path));
               exception
                  when others => null;
               end;
               Result := Invalid_Format;
               return;
            end if;

            -- SECURITY: Validate chunk doesn't exceed remaining data
            -- Detects tampering where chunk metadata is inconsistent with total size
            if Bytes_Processed + Chunk_Len > Total_Size then
               Close (Input_File);
               Close (Output_File);
               begin
                  Ada.Directories.Delete_File (Finalize.Partial_Name (Output_Path));
               exception
                  when others => null;
               end;
               Result := Invalid_Format;
               return;
            end if;

            -- Read auth tag
            for I in Auth_Tag.Data'Range loop
               Auth_Tag.Data (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;

            -- Read ciphertext
            for I in 1 .. Chunk_Len loop
               Cipher_Chunk (I) := Byte (Stream_Element'Input (File_Stream));
            end loop;

            -- Construct nonce
            Make_Chunk_Nonce (File_Nonce16, Chunk_Index, Nonce);

            -- Decrypt and verify with AAD
            Classical.XChaCha20_Decrypt (
               Ciphertext => Cipher_Chunk (1 .. Chunk_Len),
               Auth_Tag   => Auth_Tag,
               Key        => Decryption_Key,
               Nonce      => Nonce,
               AAD        => Computed_AAD,
               Plaintext  => Plain_Chunk (1 .. Chunk_Len),
               Success    => Op_Success
            );

            if not Op_Success then
               Close (Input_File);
               Close (Output_File);
               begin
                  Ada.Directories.Delete_File (Finalize.Partial_Name (Output_Path));
               exception
                  when others => null;
               end;
               Classical.Zeroize_XChaCha20_Key (Decryption_Key);
               Result := Auth_Failed;
               return;
            end if;

            -- Write plaintext
            for I in 1 .. Chunk_Len loop
               Stream_Element'Write (Output_Stream, Stream_Element (Plain_Chunk (I)));
            end loop;

            -- Update counters
            Bytes_Processed := Bytes_Processed + Chunk_Len;
            Chunk_Index := Chunk_Index + 1;
         end;
      end loop;

      -- SECURITY: Strict tampering detection
      -- Verify that we processed exactly the number of bytes specified in header
      -- If file has extra data appended, this will catch it
      if Bytes_Processed /= Total_Size then
         Close (Input_File);
         Close (Output_File);
         begin
            Ada.Directories.Delete_File (Finalize.Partial_Name (Output_Path));
         exception
            when others => null;
         end;
         Classical.Zeroize_XChaCha20_Key (Decryption_Key);
         Result := Invalid_Format;  -- File size mismatch = tampering
         return;
      end if;

      -- Verify finalization marker (crash safety check)
      declare
         Final_Marker : Byte_Array (1 .. 11);  -- "ANUB3:FINAL"
         Expected_Marker : constant Byte_Array := (65, 78, 85, 66, 51, 58, 70, 73, 78, 65, 76);
      begin
         for I in Final_Marker'Range loop
            Final_Marker (I) := Byte (Stream_Element'Input (File_Stream));
         end loop;

         if Final_Marker /= Expected_Marker then
            Close (Input_File);
            Close (Output_File);
            begin
               Ada.Directories.Delete_File (Finalize.Partial_Name (Output_Path));
            exception
               when others => null;
            end;
            Classical.Zeroize_XChaCha20_Key (Decryption_Key);
            Result := Invalid_Format;  -- Missing or corrupt finalization marker
            return;
         end if;
      exception
         when others =>
            -- No finalization marker = file was not properly finalized
            Close (Input_File);
            Close (Output_File);
            begin
               Ada.Directories.Delete_File (Finalize.Partial_Name (Output_Path));
            exception
               when others => null;
            end;
            Classical.Zeroize_XChaCha20_Key (Decryption_Key);
            Result := Invalid_Format;
            return;
      end;

      -- SECURITY: Verify EOF (no trailing data after finalization marker)
      declare
         Extra_Byte : Stream_Element;
      begin
         -- Try to read one more byte; if it succeeds, trailing junk exists
         Extra_Byte := Stream_Element'Input (File_Stream);
         -- If we got here, there's trailing data = tampering
         Close (Input_File);
         Close (Output_File);
         begin
            Ada.Directories.Delete_File (Finalize.Partial_Name (Output_Path));
         exception
            when others => null;
         end;
         Classical.Zeroize_XChaCha20_Key (Decryption_Key);
         Result := Invalid_Format;  -- Trailing data detected
         return;
      exception
         when others =>
            -- Expected: end of file reached; continue to cleanup
            null;
      end;

      -- Cleanup
      Close (Input_File);
      Close (Output_File);
      -- Atomically rename partial to final destination
      declare
         Renamed : constant Boolean := Finalize.Atomic_Rename (Finalize.Partial_Name (Output_Path), Output_Path);
      begin
         if not Renamed then
            -- Best-effort cleanup if rename fails
            begin
               Ada.Directories.Delete_File (Finalize.Partial_Name (Output_Path));
            exception
               when others => null;
            end;
            Classical.Zeroize_XChaCha20_Key (Decryption_Key);
            Result := IO_Error;
            return;
         end if;
      end;
      Classical.Zeroize_XChaCha20_Key (Decryption_Key);

      Result := Success;

      end;  -- Close declare block for Header_AAD

   exception
      when others =>
         if Is_Open (Input_File) then Close (Input_File); end if;
         if Is_Open (Output_File) then
            Close (Output_File);
            -- Attempt to delete any partial file (best-effort)
            declare
               -- This exception handler may run outside the scope of Partial_Path; recompute
               -- based on Output_Path to avoid leaking partials
               Fallback_Partial : constant String := Finalize.Partial_Name (Output_Path);
            begin
               begin
                  Ada.Directories.Delete_File (Fallback_Partial);
               exception
                  when others => null;
               end;
            end;
         end if;
         Result := IO_Error;
   end Decrypt_File_Streaming;

end Anubis_Types.Streaming;
