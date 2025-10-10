-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Key Storage Module
-- Secure serialization and persistence of hybrid keypairs
--
-- FILE FORMAT:
--   [Magic: 8 bytes] "ANUBISK\x01\x00"
--   [Version: 2 bytes] 0x0001
--   [Key Type: 1 byte] 0x01 = Identity (full keypair)
--   [X25519 Public Key: 32 bytes]
--   [X25519 Secret Key: 32 bytes]
--   [ML-KEM Public Key: 1568 bytes]
--   [ML-KEM Secret Key: 3168 bytes]
--   [Ed25519 Public Key: 32 bytes]
--   [Ed25519 Secret Key: 64 bytes]
--   [ML-DSA Public Key: 2592 bytes]
--   [ML-DSA Secret Key: 4864 bytes]
--
-- TODO: Add passphrase-based encryption using Argon2id + XChaCha20-Poly1305
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- File I/O not provable

package Anubis_Types.Storage is

   -------------------------------------------------------------------------
   -- Identity Keypair (all keys for one identity)
   -------------------------------------------------------------------------

   type Identity_Keypair is private;

   -- Generate new identity with all hybrid keys
   procedure Generate_Identity (
      Identity : out    Identity_Keypair;
      Success  : out    Boolean
   );

   -- Save identity to file
   procedure Save_Identity (
      Identity : in     Identity_Keypair;
      Filename : in     String;
      Success  : out    Boolean
   );

   -- Load identity from file
   procedure Load_Identity (
      Filename : in     String;
      Identity : out    Identity_Keypair;
      Success  : out    Boolean
   );

   -- Accessors for keys (to use in encryption/signing)
   function Get_X25519_Public (Identity : Identity_Keypair) return X25519_Public_Key;
   function Get_X25519_Secret (Identity : Identity_Keypair) return X25519_Secret_Key;
   function Get_ML_KEM_Public (Identity : Identity_Keypair) return ML_KEM_Public_Key;
   function Get_ML_KEM_Secret (Identity : Identity_Keypair) return ML_KEM_Secret_Key;
   function Get_Ed25519_Public (Identity : Identity_Keypair) return Ed25519_Public_Key;
   function Get_Ed25519_Secret (Identity : Identity_Keypair) return Ed25519_Secret_Key;
   function Get_ML_DSA_Public (Identity : Identity_Keypair) return ML_DSA_Public_Key;
   function Get_ML_DSA_Secret (Identity : Identity_Keypair) return ML_DSA_Secret_Key;

   -- Secure cleanup
   procedure Zeroize_Identity (Identity : in out Identity_Keypair);

private

   type Identity_Keypair is record
      -- Classical keys
      X25519_PK  : X25519_Public_Key;
      X25519_SK  : X25519_Secret_Key;
      Ed25519_PK : Ed25519_Public_Key;
      Ed25519_SK : Ed25519_Secret_Key;

      -- Post-quantum keys
      ML_KEM_PK  : ML_KEM_Public_Key;
      ML_KEM_SK  : ML_KEM_Secret_Key;
      ML_DSA_PK  : ML_DSA_Public_Key;
      ML_DSA_SK  : ML_DSA_Secret_Key;

      -- Validity flag
      Valid : Boolean := False;
   end record;

end Anubis_Types.Storage;
