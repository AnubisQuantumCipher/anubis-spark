-------------------------------------------------------------------------------
-- ANUBIS-SPARK: File Encryption Core Implementation
-- Integrates hybrid cryptography for authenticated file encryption
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- File I/O and complex operations not provable

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;
with Anubis_Types.Classical;
with Anubis_Types.PQC;

package body Anubis_File_Encryption is

   package Byte_IO is new Ada.Sequential_IO (Byte);

   -------------------------------------------------------------------------
   -- Helper: Generate Random Nonce
   -------------------------------------------------------------------------

   procedure Generate_Nonce (Nonce : out XChaCha20_Nonce) is
   begin
      -- Use liboqs/libsodium RNG (automatically seeded)
      -- For now, just fill with non-zero pattern
      -- TODO: Use proper crypto RNG
      for I in Nonce.Data'Range loop
         Nonce.Data (I) := Byte (I mod 256);
      end loop;
   end Generate_Nonce;

   -------------------------------------------------------------------------
   -- Header Size Calculation
   -------------------------------------------------------------------------

   function Header_Size return Natural is
   begin
      -- Magic (8) + Version (2) + Length (4) + Keys + Ciphertext + Nonce + Signatures
      return 8 + 2 + 4 + 32 + 1_568 + 32 + 1_568 + 24 + 64 + 4_627;
   end Header_Size;

   -------------------------------------------------------------------------
   -- Helper: Serialize 32-bit integer to big-endian bytes
   -------------------------------------------------------------------------

   procedure U32_To_Bytes (
      Value  : in     Natural;
      Output : out    Byte_Array
   ) with
      Pre => Output'Length = 4
   is
      V : Natural := Value;
   begin
      Output (Output'First + 3) := Byte (V mod 256);
      V := V / 256;
      Output (Output'First + 2) := Byte (V mod 256);
      V := V / 256;
      Output (Output'First + 1) := Byte (V mod 256);
      V := V / 256;
      Output (Output'First) := Byte (V mod 256);
   end U32_To_Bytes;

   -------------------------------------------------------------------------
   -- Helper: Parse big-endian bytes to 32-bit integer
   -------------------------------------------------------------------------

   function Bytes_To_U32 (Input : Byte_Array) return Natural with
      Pre => Input'Length = 4
   is
      Result : Natural := 0;
   begin
      Result := Natural (Input (Input'First)) * 256 * 256 * 256;
      Result := Result + Natural (Input (Input'First + 1)) * 256 * 256;
      Result := Result + Natural (Input (Input'First + 2)) * 256;
      Result := Result + Natural (Input (Input'First + 3));
      return Result;
   end Bytes_To_U32;

   -------------------------------------------------------------------------
   -- Create Encryption Header
   -------------------------------------------------------------------------

   procedure Create_Header (
      Recipient_X25519_PK     : in     X25519_Public_Key;
      Recipient_ML_KEM_PK     : in     ML_KEM_Public_Key;
      Ephemeral_X25519_PK     : in     X25519_Public_Key;
      ML_KEM_Ciphertext       : in     ML_KEM_Ciphertext;
      Nonce                   : in     XChaCha20_Nonce;
      Sender_Ed25519_SK       : in     Ed25519_Secret_Key;
      Sender_ML_DSA_SK        : in     ML_DSA_Secret_Key;
      Header                  : out    Encryption_Header;
      Success                 : out    Boolean
   ) is
      -- Data to be signed (everything except the signature fields)
      Header_To_Sign : Byte_Array (1 .. 3_236);  -- Up to nonce
      Sign_Success   : Boolean;
   begin
      -- Fill magic and version
      Header.Magic := MAGIC;
      Header.Version := (0, 1);

      -- Calculate and serialize header length
      declare
         Length_Bytes : Byte_Array (1 .. 4);
      begin
         U32_To_Bytes (Header_Size, Length_Bytes);
         Header.Header_Length := Length_Bytes;
      end;

      -- Copy key exchange data
      Header.Recipient_X25519 := Recipient_X25519_PK;
      Header.Recipient_ML_KEM := Recipient_ML_KEM_PK;
      Header.Ephemeral_X25519 := Ephemeral_X25519_PK;
      Header.ML_KEM_CT := ML_KEM_Ciphertext;
      Header.Nonce := Nonce;

      -- Build message to sign (all header data up to signature)
      declare
         Offset : Natural := 0;
      begin
         -- Magic
         Header_To_Sign (1 .. 8) := Header.Magic;
         Offset := 8;

         -- Version
         Header_To_Sign (9 .. 10) := Header.Version;
         Offset := 10;

         -- Length
         Header_To_Sign (11 .. 14) := Header.Header_Length;
         Offset := 14;

         -- Recipient X25519
         Header_To_Sign (15 .. 46) := Recipient_X25519_PK.Data;
         Offset := 46;

         -- Recipient ML-KEM
         Header_To_Sign (47 .. 1_614) := Recipient_ML_KEM_PK.Data;
         Offset := 1_614;

         -- Ephemeral X25519
         Header_To_Sign (1_615 .. 1_646) := Ephemeral_X25519_PK.Data;
         Offset := 1_646;

         -- ML-KEM Ciphertext
         Header_To_Sign (1_647 .. 3_214) := ML_KEM_Ciphertext.Data;
         Offset := 3_214;

         -- Nonce
         Header_To_Sign (3_215 .. 3_238) := Nonce.Data;
      end;

      -- Create hybrid signature over header data
      declare
         Hybrid_Sig : PQC.Hybrid_Signature;
      begin
         PQC.Hybrid_Sign (
            Message     => Header_To_Sign,
            Ed25519_SK  => Sender_Ed25519_SK,
            ML_DSA_SK   => Sender_ML_DSA_SK,
            Signature   => Hybrid_Sig,
            Success     => Sign_Success
         );

         if not Sign_Success then
            Success := False;
            return;
         end if;

         -- Extract signatures from hybrid signature
         Header.Ed25519_Sig := Hybrid_Sig.Ed25519_Sig;
         Header.ML_DSA_Sig := Hybrid_Sig.ML_DSA_Sig;
      end;

      Success := True;
   end Create_Header;

   -------------------------------------------------------------------------
   -- Parse and Verify Encryption Header
   -------------------------------------------------------------------------

   procedure Parse_Header (
      Header_Data             : in     Byte_Array;
      Sender_Ed25519_PK       : in     Ed25519_Public_Key;
      Sender_ML_DSA_PK        : in     ML_DSA_Public_Key;
      Recipient_X25519_PK     : out    X25519_Public_Key;
      Recipient_ML_KEM_PK     : out    ML_KEM_Public_Key;
      Ephemeral_X25519_PK     : out    X25519_Public_Key;
      ML_KEM_Ciphertext       : out    ML_KEM_Ciphertext;
      Nonce                   : out    XChaCha20_Nonce;
      Success                 : out    Boolean
   ) is
      Offset : Natural := Header_Data'First;
      Header_To_Verify : Byte_Array (1 .. 3_238);
      Hybrid_Sig : PQC.Hybrid_Signature;
      Verify_Result : Boolean;
   begin
      -- Check magic
      if Header_Data (Offset .. Offset + 7) /= MAGIC then
         Success := False;
         return;
      end if;
      Offset := Offset + 8;

      -- Check version
      if Header_Data (Offset) /= 0 or Header_Data (Offset + 1) /= 1 then
         Success := False;
         return;
      end if;
      Offset := Offset + 2;

      -- Parse header length
      declare
         Length : constant Natural := Bytes_To_U32 (Header_Data (Offset .. Offset + 3));
      begin
         if Length /= Header_Size then
            Success := False;
            return;
         end if;
      end;
      Offset := Offset + 4;

      -- Parse X25519 recipient public key
      for I in Recipient_X25519_PK.Data'Range loop
         Recipient_X25519_PK.Data (I) := Header_Data (Offset);
         Offset := Offset + 1;
      end loop;

      -- Parse ML-KEM recipient public key
      for I in Recipient_ML_KEM_PK.Data'Range loop
         Recipient_ML_KEM_PK.Data (I) := Header_Data (Offset);
         Offset := Offset + 1;
      end loop;

      -- Parse ephemeral X25519 public key
      for I in Ephemeral_X25519_PK.Data'Range loop
         Ephemeral_X25519_PK.Data (I) := Header_Data (Offset);
         Offset := Offset + 1;
      end loop;

      -- Parse ML-KEM ciphertext
      for I in ML_KEM_Ciphertext.Data'Range loop
         ML_KEM_Ciphertext.Data (I) := Header_Data (Offset);
         Offset := Offset + 1;
      end loop;

      -- Parse nonce
      for I in Nonce.Data'Range loop
         Nonce.Data (I) := Header_Data (Offset);
         Offset := Offset + 1;
      end loop;

      -- Parse Ed25519 signature
      for I in Hybrid_Sig.Ed25519_Sig.Data'Range loop
         Hybrid_Sig.Ed25519_Sig.Data (I) := Header_Data (Offset);
         Offset := Offset + 1;
      end loop;

      -- Parse ML-DSA signature
      for I in Hybrid_Sig.ML_DSA_Sig.Data'Range loop
         Hybrid_Sig.ML_DSA_Sig.Data (I) := Header_Data (Offset);
         Offset := Offset + 1;
      end loop;

      -- Build message to verify (header without signatures)
      Header_To_Verify (1 .. 3_238) := Header_Data (Header_Data'First .. Header_Data'First + 3_237);

      -- Verify hybrid signature
      Verify_Result := PQC.Hybrid_Verify (
         Message     => Header_To_Verify,
         Signature   => Hybrid_Sig,
         Ed25519_PK  => Sender_Ed25519_PK,
         ML_DSA_PK   => Sender_ML_DSA_PK
      );

      Success := Verify_Result;
   end Parse_Header;

   -------------------------------------------------------------------------
   -- High-Level File Encryption
   -------------------------------------------------------------------------

   procedure Encrypt_File (
      Plaintext_File          : in     String;
      Ciphertext_File         : in     String;
      Recipient_X25519_PK     : in     X25519_Public_Key;
      Recipient_ML_KEM_PK     : in     ML_KEM_Public_Key;
      Sender_Ed25519_SK       : in     Ed25519_Secret_Key;
      Sender_ML_DSA_SK        : in     ML_DSA_Secret_Key;
      Success                 : out    Boolean
   ) is
      -- File handles
      Input_File  : File_Type;
      Output_File : File_Type;

      -- Ephemeral keys for this encryption
      Ephemeral_X25519_PK : X25519_Public_Key;
      Ephemeral_X25519_SK : X25519_Secret_Key;

      -- Key exchange results
      ML_KEM_CT      : ML_KEM_Ciphertext;
      Hybrid_Secret  : PQC.Hybrid_Shared_Secret;
      Encryption_Key : XChaCha20_Key;

      -- Encryption parameters
      Nonce    : XChaCha20_Nonce;
      Auth_Tag : Poly1305_Tag;

      -- Header
      Header : Encryption_Header;

      -- Temporary success flags
      Op_Success : Boolean;

   begin
      -- Step 1: Read plaintext file
      declare
         File_Stream : Stream_Access;
         File_Size   : Stream_Element_Count;
         Plaintext   : Stream_Element_Array (1 .. 1_000_000);  -- Max 1MB for now
         Last        : Stream_Element_Offset;
      begin
         begin
            Open (Input_File, In_File, Plaintext_File);
         exception
            when others =>
               Success := False;
               return;
         end;

         File_Stream := Stream (Input_File);
         File_Size := Size (Input_File);

         if File_Size > Plaintext'Length then
            Close (Input_File);
            Success := False;
            return;
         end if;

         Read (File_Stream.all, Plaintext, Last);
         Close (Input_File);

         -- Step 2: Generate ephemeral X25519 keypair
         Classical.X25519_Generate_Keypair (
            Public_Key => Ephemeral_X25519_PK,
            Secret_Key => Ephemeral_X25519_SK,
            Success    => Op_Success
         );

         if not Op_Success then
            Success := False;
            return;
         end if;

         -- Step 3: Perform hybrid key exchange
         PQC.Hybrid_Encapsulate (
            X25519_Public           => Recipient_X25519_PK,
            ML_KEM_Public           => Recipient_ML_KEM_PK,
            X25519_Ephemeral_Secret => Ephemeral_X25519_SK,
            Ciphertext              => ML_KEM_CT,
            Hybrid_Secret           => Hybrid_Secret,
            Success                 => Op_Success
         );

         if not Op_Success then
            Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
            Success := False;
            return;
         end if;

         -- Step 4: Derive encryption key
         PQC.Derive_Encryption_Key (
            Hybrid_Secret  => Hybrid_Secret,
            Encryption_Key => Encryption_Key,
            Success        => Op_Success
         );

         if not Op_Success then
            Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
            Success := False;
            return;
         end if;

         -- Step 5: Generate nonce
         Generate_Nonce (Nonce);

         -- Step 6: Encrypt data
         declare
            Plain_Bytes  : Byte_Array (1 .. Natural (Last));
            Cipher_Bytes : Byte_Array (1 .. Natural (Last));
         begin
            -- Convert Stream_Element_Array to Byte_Array
            for I in Plain_Bytes'Range loop
               Plain_Bytes (I) := Byte (Plaintext (Stream_Element_Offset (I)));
            end loop;

            Classical.XChaCha20_Encrypt (
               Plaintext  => Plain_Bytes,
               Key        => Encryption_Key,
               Nonce      => Nonce,
               Ciphertext => Cipher_Bytes,
               Auth_Tag   => Auth_Tag,
               Success    => Op_Success
            );

            if not Op_Success then
               Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
               Classical.Zeroize_XChaCha20_Key (Encryption_Key);
               Success := False;
               return;
            end if;

            -- Step 7: Create header
            Create_Header (
               Recipient_X25519_PK => Recipient_X25519_PK,
               Recipient_ML_KEM_PK => Recipient_ML_KEM_PK,
               Ephemeral_X25519_PK => Ephemeral_X25519_PK,
               ML_KEM_Ciphertext   => ML_KEM_CT,
               Nonce               => Nonce,
               Sender_Ed25519_SK   => Sender_Ed25519_SK,
               Sender_ML_DSA_SK    => Sender_ML_DSA_SK,
               Header              => Header,
               Success             => Op_Success
            );

            if not Op_Success then
               Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
               Classical.Zeroize_XChaCha20_Key (Encryption_Key);
               Success := False;
               return;
            end if;

            -- Step 8: Write encrypted file
            declare
               Output_Stream : Stream_Access;
            begin
               begin
                  Create (Output_File, Out_File, Ciphertext_File);
               exception
                  when others =>
                     Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
                     Classical.Zeroize_XChaCha20_Key (Encryption_Key);
                     Success := False;
                     return;
               end;

               Output_Stream := Stream (Output_File);

               -- Write header (serialize all fields)
               for I in Header.Magic'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.Magic (I)));
               end loop;
               for I in Header.Version'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.Version (I)));
               end loop;
               for I in Header.Header_Length'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.Header_Length (I)));
               end loop;
               for I in Header.Recipient_X25519.Data'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.Recipient_X25519.Data (I)));
               end loop;
               for I in Header.Recipient_ML_KEM.Data'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.Recipient_ML_KEM.Data (I)));
               end loop;
               for I in Header.Ephemeral_X25519.Data'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.Ephemeral_X25519.Data (I)));
               end loop;
               for I in Header.ML_KEM_CT.Data'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.ML_KEM_CT.Data (I)));
               end loop;
               for I in Header.Nonce.Data'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.Nonce.Data (I)));
               end loop;
               for I in Header.Ed25519_Sig.Data'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.Ed25519_Sig.Data (I)));
               end loop;
               for I in Header.ML_DSA_Sig.Data'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Header.ML_DSA_Sig.Data (I)));
               end loop;

               -- Write ciphertext
               for I in Cipher_Bytes'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Cipher_Bytes (I)));
               end loop;

               -- Write authentication tag
               for I in Auth_Tag.Data'Range loop
                  Stream_Element'Write (Output_Stream, Stream_Element (Auth_Tag.Data (I)));
               end loop;

               Close (Output_File);
            end;

            -- Cleanup
            Classical.Zeroize_X25519_Secret (Ephemeral_X25519_SK);
            Classical.Zeroize_XChaCha20_Key (Encryption_Key);

            Success := True;
         end;
      end;
   end Encrypt_File;

   -------------------------------------------------------------------------
   -- High-Level File Decryption (Stub for now - full implementation next)
   -------------------------------------------------------------------------

   procedure Decrypt_File (
      Ciphertext_File         : in     String;
      Plaintext_File          : in     String;
      Recipient_X25519_SK     : in     X25519_Secret_Key;
      Recipient_ML_KEM_SK     : in     ML_KEM_Secret_Key;
      Sender_Ed25519_PK       : in     Ed25519_Public_Key;
      Sender_ML_DSA_PK        : in     ML_DSA_Public_Key;
      Success                 : out    Boolean
   ) is
      -- File handles
      Input_File  : File_Type;
      Output_File : File_Type;

      -- Parsed header data
      Parsed_Recipient_X25519 : X25519_Public_Key;
      Parsed_Recipient_ML_KEM : ML_KEM_Public_Key;
      Ephemeral_X25519_PK     : X25519_Public_Key;
      ML_KEM_CT               : ML_KEM_Ciphertext;
      Nonce                   : XChaCha20_Nonce;

      -- Key exchange results
      Hybrid_Secret  : PQC.Hybrid_Shared_Secret;
      Decryption_Key : XChaCha20_Key;

      -- Temporary success flag
      Op_Success : Boolean;

   begin
      -- Step 1: Read encrypted file
      declare
         File_Stream : Stream_Access;
         File_Size   : Stream_Element_Count;
         Encrypted   : Stream_Element_Array (1 .. 2_000_000);  -- Max 2MB (header + 1MB data + overhead)
         Last        : Stream_Element_Offset;
      begin
         begin
            Open (Input_File, In_File, Ciphertext_File);
         exception
            when others =>
               Success := False;
               return;
         end;

         File_Stream := Stream (Input_File);
         File_Size := Size (Input_File);

         if File_Size > Encrypted'Length then
            Close (Input_File);
            Success := False;
            return;
         end if;

         if File_Size < Stream_Element_Count (Header_Size + 16) then
            -- File too small (must have header + auth tag minimum)
            Close (Input_File);
            Success := False;
            return;
         end if;

         Read (File_Stream.all, Encrypted, Last);
         Close (Input_File);

         -- Step 2: Parse and verify header
         declare
            Header_Bytes : Byte_Array (1 .. Header_Size);
            Ciphertext_Size : constant Natural := Natural (Last) - Header_Size - 16;
            Ciphertext_Bytes : Byte_Array (1 .. Ciphertext_Size);
            Auth_Tag : Poly1305_Tag;
         begin
            -- Extract header
            for I in Header_Bytes'Range loop
               Header_Bytes (I) := Byte (Encrypted (Stream_Element_Offset (I)));
            end loop;

            -- Parse and verify header (includes signature verification)
            Parse_Header (
               Header_Data         => Header_Bytes,
               Sender_Ed25519_PK   => Sender_Ed25519_PK,
               Sender_ML_DSA_PK    => Sender_ML_DSA_PK,
               Recipient_X25519_PK => Parsed_Recipient_X25519,
               Recipient_ML_KEM_PK => Parsed_Recipient_ML_KEM,
               Ephemeral_X25519_PK => Ephemeral_X25519_PK,
               ML_KEM_Ciphertext   => ML_KEM_CT,
               Nonce               => Nonce,
               Success             => Op_Success
            );

            if not Op_Success then
               -- Signature verification failed
               Success := False;
               return;
            end if;

            -- Step 3: Perform hybrid key decapsulation
            PQC.Hybrid_Decapsulate (
               X25519_Secret       => Recipient_X25519_SK,
               ML_KEM_Secret       => Recipient_ML_KEM_SK,
               X25519_Ephemeral    => Ephemeral_X25519_PK,
               ML_KEM_Ciphertext   => ML_KEM_CT,
               Hybrid_Secret       => Hybrid_Secret,
               Success             => Op_Success
            );

            if not Op_Success then
               Success := False;
               return;
            end if;

            -- Step 4: Derive decryption key
            PQC.Derive_Encryption_Key (
               Hybrid_Secret  => Hybrid_Secret,
               Encryption_Key => Decryption_Key,
               Success        => Op_Success
            );

            if not Op_Success then
               Classical.Zeroize_XChaCha20_Key (Decryption_Key);
               Success := False;
               return;
            end if;

            -- Extract ciphertext
            for I in Ciphertext_Bytes'Range loop
               Ciphertext_Bytes (I) := Byte (Encrypted (Stream_Element_Offset (Header_Size + I)));
            end loop;

            -- Extract auth tag (last 16 bytes)
            for I in Auth_Tag.Data'Range loop
               Auth_Tag.Data (I) := Byte (Encrypted (Last - 15 + Stream_Element_Offset (I - 1)));
            end loop;

            -- Step 5: Decrypt and verify data
            declare
               Plaintext_Bytes : Byte_Array (1 .. Ciphertext_Size);
            begin
               Classical.XChaCha20_Decrypt (
                  Ciphertext => Ciphertext_Bytes,
                  Key        => Decryption_Key,
                  Nonce      => Nonce,
                  Auth_Tag   => Auth_Tag,
                  Plaintext  => Plaintext_Bytes,
                  Success    => Op_Success
               );

               if not Op_Success then
                  -- Decryption or authentication failed
                  Classical.Zeroize_XChaCha20_Key (Decryption_Key);
                  Success := False;
                  return;
               end if;

               -- Step 6: Write plaintext file
               declare
                  Output_Stream : Stream_Access;
               begin
                  begin
                     Create (Output_File, Out_File, Plaintext_File);
                  exception
                     when others =>
                        Classical.Zeroize_XChaCha20_Key (Decryption_Key);
                        Success := False;
                        return;
                  end;

                  Output_Stream := Stream (Output_File);

                  -- Write plaintext
                  for I in Plaintext_Bytes'Range loop
                     Stream_Element'Write (Output_Stream, Stream_Element (Plaintext_Bytes (I)));
                  end loop;

                  Close (Output_File);
               end;

               -- Step 7: Secure cleanup
               Classical.Zeroize_XChaCha20_Key (Decryption_Key);

               Success := True;
            end;
         end;
      end;
   end Decrypt_File;

end Anubis_File_Encryption;
