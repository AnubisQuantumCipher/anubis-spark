-------------------------------------------------------------------------------
-- ANUBIS-SPARK: File Encryption Core
-- PLATINUM LEVEL: Authenticated encryption with hybrid post-quantum protection
--
-- SECURITY ARCHITECTURE:
--   1. Ephemeral key exchange (X25519 + ML-KEM-1024)
--   2. HKDF key derivation with domain separation
--   3. XChaCha20-Poly1305 AEAD for data encryption
--   4. Hybrid signatures (Ed25519 + ML-DSA-87) for authenticity
--
-- FILE FORMAT:
--   [Magic: 8 bytes] "ANUBIS\x01\x00"
--   [Version: 2 bytes] 0x0001
--   [Header Length: 4 bytes] (big-endian)
--   [Header: variable]
--     - Recipient Public Keys (X25519 + ML-KEM)
--     - Ephemeral Public Key (X25519)
--     - ML-KEM Ciphertext
--     - Nonce (XChaCha20)
--     - Hybrid Signature (Ed25519 + ML-DSA-87)
--   [Encrypted Data: variable]
--   [Auth Tag: 16 bytes] Poly1305 MAC
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package Anubis_Types.File_Encryption is

   -------------------------------------------------------------------------
   -- File Format Constants
   -------------------------------------------------------------------------

   MAGIC : constant Byte_Array (1 .. 8) := (
      65, 78, 85, 66, 73, 83, 1, 0  -- "ANUBIS" + version bytes
   );

   FILE_VERSION : constant := 1;

   -------------------------------------------------------------------------
   -- Encryption Header (stores key exchange data)
   -------------------------------------------------------------------------

   type Encryption_Header is private;

   -- Get header size (for buffer allocation)
   function Header_Size return Natural with
      Global => null,
      Post   => Header_Size'Result > 0;

   -- Create encryption header (during encryption)
   procedure Create_Header (
      Recipient_X25519_PK     : in     X25519_Public_Key;
      Recipient_ML_KEM_PK     : in     ML_KEM_Public_Key;
      Ephemeral_X25519_PK     : in     X25519_Public_Key;
      ML_KEM_CT               : in     ML_KEM_Ciphertext;
      Nonce                   : in     XChaCha20_Nonce;
      Sender_Ed25519_SK       : in     Ed25519_Secret_Key;
      Sender_ML_DSA_SK        : in     ML_DSA_Secret_Key;
      Header                  : out    Encryption_Header;
      Success                 : out    Boolean
   ) with
      Pre    => Is_Valid (Sender_Ed25519_SK) and
                Is_Valid (Sender_ML_DSA_SK),
      Global => null;

   -- Parse and verify encryption header (during decryption)
   procedure Parse_Header (
      Header_Data             : in     Byte_Array;
      Sender_Ed25519_PK       : in     Ed25519_Public_Key;
      Sender_ML_DSA_PK        : in     ML_DSA_Public_Key;
      Recipient_X25519_PK     : out    X25519_Public_Key;
      Recipient_ML_KEM_PK     : out    ML_KEM_Public_Key;
      Ephemeral_X25519_PK     : out    X25519_Public_Key;
      ML_KEM_CT               : out    ML_KEM_Ciphertext;
      Nonce                   : out    XChaCha20_Nonce;
      Success                 : out    Boolean
   ) with
      Pre    => Header_Data'Length >= Header_Size,
      Global => null;

   -------------------------------------------------------------------------
   -- High-Level Encryption Operations
   -------------------------------------------------------------------------

   -- Encrypt file with hybrid post-quantum protection
   procedure Encrypt_File (
      Plaintext_File          : in     String;
      Ciphertext_File         : in     String;
      Recipient_X25519_PK     : in     X25519_Public_Key;
      Recipient_ML_KEM_PK     : in     ML_KEM_Public_Key;
      Sender_Ed25519_SK       : in     Ed25519_Secret_Key;
      Sender_ML_DSA_SK        : in     ML_DSA_Secret_Key;
      Success                 : out    Boolean
   ) with
      Pre    => Plaintext_File'Length > 0 and
                Ciphertext_File'Length > 0 and
                Is_Valid (Sender_Ed25519_SK) and
                Is_Valid (Sender_ML_DSA_SK),
      Global => null;

   -- Decrypt file and verify signatures
   procedure Decrypt_File (
      Ciphertext_File         : in     String;
      Plaintext_File          : in     String;
      Recipient_X25519_SK     : in     X25519_Secret_Key;
      Recipient_ML_KEM_SK     : in     ML_KEM_Secret_Key;
      Sender_Ed25519_PK       : in     Ed25519_Public_Key;
      Sender_ML_DSA_PK        : in     ML_DSA_Public_Key;
      Success                 : out    Boolean
   ) with
      Pre    => Ciphertext_File'Length > 0 and
                Plaintext_File'Length > 0 and
                Is_Valid (Recipient_X25519_SK) and
                Is_Valid (Recipient_ML_KEM_SK),
      Global => null;

private

   -------------------------------------------------------------------------
   -- Private Implementation Details
   -------------------------------------------------------------------------

   -- Encryption header layout (optimized for alignment)
   type Encryption_Header is record
      Magic                : Byte_Array (1 .. 8);      -- "ANUBIS\x01\x00"
      Version              : Byte_Array (1 .. 2);      -- 0x0001
      Header_Length        : Byte_Array (1 .. 4);      -- Big-endian uint32
      Recipient_X25519     : X25519_Public_Key;        -- 32 bytes
      Recipient_ML_KEM     : ML_KEM_Public_Key;        -- 1,568 bytes
      Ephemeral_X25519     : X25519_Public_Key;        -- 32 bytes
      ML_KEM_CT            : ML_KEM_Ciphertext;        -- 1,568 bytes
      Nonce                : XChaCha20_Nonce;          -- 24 bytes
      -- Signature fields (filled during signing)
      Ed25519_Sig          : Ed25519_Signature;        -- 64 bytes
      ML_DSA_Sig           : ML_DSA_Signature;         -- 4,627 bytes
   end record;

   -- Total header size: 8 + 2 + 4 + 32 + 1568 + 32 + 1568 + 24 + 64 + 4627 = 7,929 bytes

end Anubis_Types.File_Encryption;
