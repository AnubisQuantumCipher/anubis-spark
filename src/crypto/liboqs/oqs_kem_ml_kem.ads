-------------------------------------------------------------------------------
-- ANUBIS-SPARK: liboqs ML-KEM C Bindings
-- Low-level FFI to ML-KEM-1024 (NIST FIPS 203)
-------------------------------------------------------------------------------

pragma Ada_2012;

with System;
with OQS_Common; use OQS_Common;

package OQS_KEM_ML_KEM is
   pragma Preelaborate;

   -------------------------------------------------------------------------
   -- ML-KEM-1024: NIST Level 5 Key Encapsulation Mechanism
   -- Security: 256-bit equivalent (quantum-resistant)
   -------------------------------------------------------------------------

   -- Key sizes (bytes)
   ML_KEM_1024_LENGTH_PUBLIC_KEY      : constant := 1_568;
   ML_KEM_1024_LENGTH_SECRET_KEY      : constant := 3_168;
   ML_KEM_1024_LENGTH_CIPHERTEXT      : constant := 1_568;
   ML_KEM_1024_LENGTH_SHARED_SECRET   : constant := 32;

   -------------------------------------------------------------------------
   -- ML-KEM-1024 Operations
   -------------------------------------------------------------------------

   -- Generate a keypair
   -- public_key must point to 1,568 bytes
   -- secret_key must point to 3,168 bytes
   function OQS_KEM_ml_kem_1024_keypair (
      public_key : System.Address;
      secret_key : System.Address
   ) return OQS_STATUS with
      Import, Convention => C, External_Name => "OQS_KEM_ml_kem_1024_keypair";

   -- Encapsulate: Generate shared secret and ciphertext
   -- ciphertext must point to 1,568 bytes
   -- shared_secret must point to 32 bytes
   -- public_key must point to 1,568 bytes
   function OQS_KEM_ml_kem_1024_encaps (
      ciphertext    : System.Address;
      shared_secret : System.Address;
      public_key    : System.Address
   ) return OQS_STATUS with
      Import, Convention => C, External_Name => "OQS_KEM_ml_kem_1024_encaps";

   -- Decapsulate: Recover shared secret from ciphertext
   -- shared_secret must point to 32 bytes
   -- ciphertext must point to 1,568 bytes
   -- secret_key must point to 3,168 bytes
   function OQS_KEM_ml_kem_1024_decaps (
      shared_secret : System.Address;
      ciphertext    : System.Address;
      secret_key    : System.Address
   ) return OQS_STATUS with
      Import, Convention => C, External_Name => "OQS_KEM_ml_kem_1024_decaps";

end OQS_KEM_ML_KEM;
