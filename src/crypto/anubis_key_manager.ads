-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Key Management System
-- Comprehensive key lifecycle management with SPARK verification
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Ada.Calendar; use Ada.Calendar;

package Anubis_Key_Manager is

   -------------------------------------------------------------------------
   -- Key Hierarchy Design:
   --
   -- Master Key (derived from passphrase via Argon2id)
   --    ├─> Encryption Master Key (first 32 bytes)
   --    │      ├─> File Encryption Keys (ephemeral, per-file)
   --    │      └─> Vault Encryption Key (persistent)
   --    │
   --    └─> Authentication Master Key (last 32 bytes)
   --           ├─> HMAC Keys (for integrity)
   --           └─> Key Encryption Keys (for wrapping)
   --
   -- Hybrid Identity Keys (separate key pairs):
   --    ├─> Classical Keys
   --    │      ├─> X25519 (key exchange)
   --    │      └─> Ed25519 (signatures)
   --    │
   --    └─> Post-Quantum Keys
   --           ├─> ML-KEM-1024 (key encapsulation)
   --           └─> ML-DSA-87 (digital signatures)
   -------------------------------------------------------------------------

   -- Key metadata for lifecycle management
   type Key_Metadata is record
      Key_ID          : String (1 .. 64);  -- SHA-256 fingerprint (hex)
      Created_At      : Time;
      Last_Used       : Time;
      Expiry_Date     : Time;
      Rotation_Count  : Natural := 0;
      Status          : Key_Status := Uninitialized;
      Purpose         : Key_Purpose;
   end record;

   type Key_Purpose is (
      Master_Key_Purpose,
      File_Encryption,
      Vault_Encryption,
      Key_Wrapping,
      Digital_Signature,
      Key_Exchange,
      Backup_Recovery
   );

   -- Hybrid identity containing both classical and PQ keys
   type Hybrid_Identity is record
      -- Classical cryptography
      X25519_Public   : X25519_Public_Key;
      X25519_Secret   : X25519_Secret_Key;
      Ed25519_Public  : Ed25519_Public_Key;
      Ed25519_Secret  : Ed25519_Secret_Key;

      -- Post-quantum cryptography
      ML_KEM_Public   : ML_KEM_Public_Key;
      ML_KEM_Secret   : ML_KEM_Secret_Key;
      ML_DSA_Public   : ML_DSA_Public_Key;
      ML_DSA_Secret   : ML_DSA_Secret_Key;

      -- Metadata
      Metadata        : Key_Metadata;
   end record;

   -- Encrypted keystore format (for persistent storage)
   type Encrypted_Keystore is private;

   -------------------------------------------------------------------------
   -- Key Generation: Uses cryptographically secure entropy
   -------------------------------------------------------------------------

   -- Generate new hybrid identity (both classical and PQ keys)
   procedure Generate_Hybrid_Identity (
      Identity : out Hybrid_Identity;
      Success  : out Boolean
   ) with
      Post => (if Success then
                  Is_Valid (Identity.X25519_Secret) and
                  Is_Valid (Identity.Ed25519_Secret) and
                  Is_Valid (Identity.ML_KEM_Secret) and
                  Is_Valid (Identity.ML_DSA_Secret));

   -- Derive master key from passphrase using Argon2id
   procedure Derive_Master_Key (
      Passphrase  : in     String;
      Salt        : in     Argon2_Salt;
      Master      : out    Master_Key;
      Success     : out    Boolean
   ) with
      Pre  => Passphrase'Length >= 12,  -- Minimum 12 characters
      Post => (if Success then Is_Valid (Master));

   -- Generate random salt for key derivation
   procedure Generate_Salt (
      Salt    : out Argon2_Salt;
      Success : out Boolean
   );

   -------------------------------------------------------------------------
   -- Key Storage: Encrypted at rest
   -------------------------------------------------------------------------

   -- Save identity to encrypted keystore
   procedure Save_Identity (
      Identity   : in     Hybrid_Identity;
      Master     : in     Master_Key;
      Keystore   : out    Encrypted_Keystore;
      Success    : out    Boolean
   ) with
      Pre => Is_Valid (Master);

   -- Load identity from encrypted keystore
   procedure Load_Identity (
      Keystore   : in     Encrypted_Keystore;
      Master     : in     Master_Key;
      Identity   : out    Hybrid_Identity;
      Success    : out    Boolean
   ) with
      Pre  => Is_Valid (Master),
      Post => (if Success then
                  Is_Valid (Identity.X25519_Secret) and
                  Is_Valid (Identity.ML_KEM_Secret));

   -- Export identity to file (encrypted)
   procedure Export_Identity_To_File (
      Identity   : in     Hybrid_Identity;
      Master     : in     Master_Key;
      File_Path  : in     String;
      Success    : out    Boolean
   ) with
      Pre => Is_Valid (Master);

   -- Import identity from file
   procedure Import_Identity_From_File (
      File_Path  : in     String;
      Master     : in     Master_Key;
      Identity   : out    Hybrid_Identity;
      Success    : out    Boolean
   ) with
      Pre  => Is_Valid (Master),
      Post => (if Success then Is_Valid (Identity.X25519_Secret));

   -------------------------------------------------------------------------
   -- Key Rotation: Periodic key updates for forward secrecy
   -------------------------------------------------------------------------

   -- Check if keys need rotation based on policy
   function Needs_Rotation (
      Metadata        : Key_Metadata;
      Max_Age_Days    : Positive := 90;
      Max_Operations  : Positive := 1_000_000
   ) return Boolean;

   -- Rotate hybrid identity (generates new keys, preserves old for decryption)
   procedure Rotate_Hybrid_Identity (
      Old_Identity : in     Hybrid_Identity;
      New_Identity : out    Hybrid_Identity;
      Success      : out    Boolean
   ) with
      Pre  => Is_Valid (Old_Identity.X25519_Secret),
      Post => (if Success then
                  Is_Valid (New_Identity.X25519_Secret) and
                  New_Identity.Metadata.Rotation_Count =
                     Old_Identity.Metadata.Rotation_Count + 1);

   -------------------------------------------------------------------------
   -- Key Revocation and Destruction
   -------------------------------------------------------------------------

   -- Revoke a key (marks as revoked, cannot be used)
   procedure Revoke_Identity (
      Identity : in out Hybrid_Identity
   ) with
      Post => Identity.Metadata.Status = Revoked;

   -- Securely destroy an identity (zeroizes all secret keys)
   procedure Destroy_Identity (
      Identity : in out Hybrid_Identity
   ) with
      Post => not Is_Valid (Identity.X25519_Secret) and
              not Is_Valid (Identity.Ed25519_Secret) and
              not Is_Valid (Identity.ML_KEM_Secret) and
              not Is_Valid (Identity.ML_DSA_Secret) and
              Identity.Metadata.Status = Destroyed;

   -------------------------------------------------------------------------
   -- Key Backup and Recovery (Shamir Secret Sharing)
   -------------------------------------------------------------------------

   type Recovery_Share is private;
   type Share_Array is array (Positive range <>) of Recovery_Share;

   -- Split master key into N shares (require K to reconstruct)
   procedure Create_Recovery_Shares (
      Master      : in     Master_Key;
      N_Shares    : in     Positive;  -- Total shares
      K_Threshold : in     Positive;  -- Required to recover
      Shares      : out    Share_Array;
      Success     : out    Boolean
   ) with
      Pre  => Is_Valid (Master) and
              K_Threshold <= N_Shares and
              Shares'Length = N_Shares and
              K_Threshold >= 2;  -- Minimum 2-of-N

   -- Recover master key from K or more shares
   procedure Recover_Master_Key (
      Shares  : in     Share_Array;
      Master  : out    Master_Key;
      Success : out    Boolean
   ) with
      Pre  => Shares'Length >= 2,
      Post => (if Success then Is_Valid (Master));

   -------------------------------------------------------------------------
   -- Key Export/Import for Secure Transfer
   -------------------------------------------------------------------------

   -- Export public keys only (safe to share)
   type Public_Identity is record
      X25519_Public  : X25519_Public_Key;
      Ed25519_Public : Ed25519_Public_Key;
      ML_KEM_Public  : ML_KEM_Public_Key;
      ML_DSA_Public  : ML_DSA_Public_Key;
      Fingerprint    : String (1 .. 64);  -- SHA-256 of public keys
   end record;

   function Export_Public_Identity (
      Identity : Hybrid_Identity
   ) return Public_Identity;

   -- Compute fingerprint for key verification
   function Compute_Fingerprint (
      Identity : Hybrid_Identity
   ) return String with
      Post => Compute_Fingerprint'Result'Length = 64;

private

   -- Encrypted keystore structure (CBOR-encoded, encrypted with Master Key)
   type Encrypted_Keystore is record
      Version         : Positive := 1;
      Salt            : Argon2_Salt;
      Nonce           : XChaCha20_Nonce;
      Ciphertext      : Byte_Array (1 .. 16_384);  -- Encrypted identity data
      Ciphertext_Len  : Natural := 0;
      Auth_Tag        : Poly1305_Tag;
   end record;

   -- Recovery share structure (Shamir Secret Sharing)
   type Recovery_Share is record
      Share_Index     : Positive;
      Share_Data      : Byte_Array (1 .. 64);
      Checksum        : Byte_Array (1 .. 32);  -- SHA-256 for integrity
   end record;

end Anubis_Key_Manager;
