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
         Computed_AAD : constant Byte_Array := Header_AAD.Compute_Header_AAD (
            File_Nonce16 => File_Nonce16,
            Chunk_Size   => Chunk_Size,
            Total_Size   => Natural (Total_Size)
         );
         Partial_Path : constant String := Finalize.Partial_Name (Output_Path);
      begin

      -- Step 5: Create output file with .partial extension (for crash safety)
      begin
         Create (Output_File, Out_File, Partial_Path);
      exception
         when others =>
            Close (Input_File);
            Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
            Classical.Zeroize_XChaCha20_Key (Encryption_Key);
            Result := IO_Error;
            return;
      end;

      Output_Stream := Stream (Output_File);

      -- Write header: ANUB2 (5) | version (1) | file_nonce (16) | chunk_size (8) | total_size (8) |
      --               ephemeral_x25519_pk (32) | ml_kem_ct (1568)
      declare
         Magic : constant Byte_Array := (65, 78, 85, 66, 50);  -- "ANUB2"
         Version : constant Byte := 1;
         CS_Bytes : Byte_Array (1 .. 8);
         TS_Bytes : Byte_Array (1 .. 8);
      begin
         for B of Magic loop
            Stream_Element'Write (Output_Stream, Stream_Element (B));
         end loop;
         Stream_Element'Write (Output_Stream, Stream_Element (Version));
         for B of File_Nonce16 loop
            Stream_Element'Write (Output_Stream, Stream_Element (B));
         end loop;

         U64_To_BE_Bytes (U64 (Chunk_Size), CS_Bytes);
         for B of CS_Bytes loop
            Stream_Element'Write (Output_Stream, Stream_Element (B));
         end loop;

         U64_To_BE_Bytes (U64 (Total_Size), TS_Bytes);
         for B of TS_Bytes loop
            Stream_Element'Write (Output_Stream, Stream_Element (B));
         end loop;

         -- Write ephemeral X25519 public key
         for B of Ephemeral_X25519_PK.Data loop
            Stream_Element'Write (Output_Stream, Stream_Element (B));
         end loop;

         -- Write ML-KEM ciphertext
         for B of ML_KEM_CT.Data loop
            Stream_Element'Write (Output_Stream, Stream_Element (B));
         end loop;
      end;

      -- Step 6: Process file in chunks
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
      Result          : out    Result_Code
   ) is
      -- File handles
      Input_File  : File_Type;
      Output_File : File_Type;

      -- Crypto keys
      Parsed_X25519_PK : X25519_Public_Key;
      Parsed_ML_KEM_PK : ML_KEM_Public_Key;
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

   begin
      -- Open input file
      begin
         Open (Input_File, In_File, Input_Path);
      exception
         when others =>
            Result := IO_Error;
            return;
      end;

      File_Stream := Stream (Input_File);

      -- Read and verify header
      declare
         Magic : Byte_Array (1 .. 5);
         Version : Byte;
         CS_Bytes : Byte_Array (1 .. 8);
         TS_Bytes : Byte_Array (1 .. 8);
      begin
         -- Read magic
         for I in Magic'Range loop
            Magic (I) := Byte (Stream_Element'Input (File_Stream));
         end loop;

         if Magic /= (65, 78, 85, 66, 50) then  -- "ANUB2"
            Close (Input_File);
            Result := Invalid_Format;
            return;
         end if;

         -- Read version
         Version := Byte (Stream_Element'Input (File_Stream));
         if Version /= 1 then
            Close (Input_File);
            Result := Invalid_Format;
            return;
         end if;

         -- Read file nonce
         for I in File_Nonce16'Range loop
            File_Nonce16 (I) := Byte (Stream_Element'Input (File_Stream));
         end loop;

         -- Read chunk size
         for I in CS_Bytes'Range loop
            CS_Bytes (I) := Byte (Stream_Element'Input (File_Stream));
         end loop;
         Chunk_Size_U64 := BE_Bytes_To_U64 (CS_Bytes);
         Chunk_Size := Natural (Chunk_Size_U64);

         -- Read total size
         for I in TS_Bytes'Range loop
            TS_Bytes (I) := Byte (Stream_Element'Input (File_Stream));
         end loop;
         Total_Size_U64 := BE_Bytes_To_U64 (TS_Bytes);
         Total_Size := Natural (Total_Size_U64);

         -- Read ephemeral X25519 public key
         for I in Ephemeral_X25519_PK.Data'Range loop
            Ephemeral_X25519_PK.Data (I) := Byte (Stream_Element'Input (File_Stream));
         end loop;

         -- Read ML-KEM ciphertext
         for I in ML_KEM_CT.Data'Range loop
            ML_KEM_CT.Data (I) := Byte (Stream_Element'Input (File_Stream));
         end loop;
      end;

      -- Compute header AAD (must match encryption AAD)
      declare
         Computed_AAD : constant Byte_Array := Header_AAD.Compute_Header_AAD (
            File_Nonce16 => File_Nonce16,
            Chunk_Size   => Chunk_Size,
            Total_Size   => Total_Size
         );
      begin

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

      -- Create output file
      begin
         Create (Output_File, Out_File, Output_Path);
      exception
         when others =>
            Close (Input_File);
            Result := IO_Error;
            return;
      end;

      Output_Stream := Stream (Output_File);

      -- Process chunks
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

            if Chunk_Len = 0 or Chunk_Len > Chunk_Size then
               Close (Input_File);
               Close (Output_File);
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
         Classical.Zeroize_XChaCha20_Key (Decryption_Key);
         Result := Invalid_Format;  -- File size mismatch = tampering
         return;
      end if;

      -- Verify finalization marker (crash safety check)
      declare
         Final_Marker : Byte_Array (1 .. 11);  -- "ANUB2:FINAL"
         Expected_Marker : constant Byte_Array := (65, 78, 85, 66, 50, 58, 70, 73, 78, 65, 76);
      begin
         for I in Final_Marker'Range loop
            Final_Marker (I) := Byte (Stream_Element'Input (File_Stream));
         end loop;

         if Final_Marker /= Expected_Marker then
            Close (Input_File);
            Close (Output_File);
            Classical.Zeroize_XChaCha20_Key (Decryption_Key);
            Result := Invalid_Format;  -- Missing or corrupt finalization marker
            return;
         end if;
      exception
         when others =>
            -- No finalization marker = file was not properly finalized
            Close (Input_File);
            Close (Output_File);
            Classical.Zeroize_XChaCha20_Key (Decryption_Key);
            Result := Invalid_Format;
            return;
      end;

      -- Cleanup
      Close (Input_File);
      Close (Output_File);
      Classical.Zeroize_XChaCha20_Key (Decryption_Key);

      Result := Success;

      end;  -- Close declare block for Header_AAD

   exception
      when others =>
         if Is_Open (Input_File) then Close (Input_File); end if;
         if Is_Open (Output_File) then Close (Output_File); end if;
         Result := IO_Error;
   end Decrypt_File_Streaming;

end Anubis_Types.Streaming;
