-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Streaming AEAD File Encryption
-- Universal engine for all file sizes (small = 1 chunk, large = many chunks)
-- Construction: XChaCha20-Poly1305 per chunk (encrypt-then-MAC)
-- Nonce: nonce24 = file_nonce16 || u64_be(chunk_idx) (fresh by construction)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package Anubis_Types.Streaming is

   -------------------------------------------------------------------------
   -- Result Codes
   -------------------------------------------------------------------------

   type Result_Code is (
      Success,           -- Operation completed successfully
      IO_Error,          -- File I/O error
      Crypto_Error,      -- Cryptographic operation failed
      Invalid_Format,    -- Invalid file format
      Auth_Failed        -- Authentication tag verification failed
   );

   -------------------------------------------------------------------------
   -- Streaming Encryption (64 MB chunks by default)
   -------------------------------------------------------------------------

   -- Encrypt file using streaming AEAD
   -- Small files become single chunk; large files use multiple chunks
   procedure Encrypt_File_Streaming (
      Input_Path      : in     String;
      Output_Path     : in     String;
      X25519_PK       : in     X25519_Public_Key;
      ML_KEM_PK       : in     ML_KEM_Public_Key;
      Ed25519_SK      : in     Ed25519_Secret_Key;
      ML_DSA_SK       : in     ML_DSA_Secret_Key;
      Result          : out    Result_Code;
      Chunk_Size      : in     Natural := 67_108_864  -- 64 MB default
   ) with
      Pre    => Input_Path'Length > 0 and
                Output_Path'Length > 0 and
                Chunk_Size > 0 and
                Is_Valid (Ed25519_SK) and
                Is_Valid (ML_DSA_SK),
      Global => null;

   -- Decrypt file using streaming AEAD
   -- Verifies every chunk's authentication tag
   procedure Decrypt_File_Streaming (
      Input_Path      : in     String;
      Output_Path     : in     String;
      X25519_SK       : in     X25519_Secret_Key;
      ML_KEM_SK       : in     ML_KEM_Secret_Key;
      Ed25519_PK      : in     Ed25519_Public_Key;
      ML_DSA_PK       : in     ML_DSA_Public_Key;
      Result          : out    Result_Code
   ) with
      Pre    => Input_Path'Length > 0 and
                Output_Path'Length > 0 and
                Is_Valid (X25519_SK) and
                Is_Valid (ML_KEM_SK),
      Global => null;

end Anubis_Types.Streaming;
