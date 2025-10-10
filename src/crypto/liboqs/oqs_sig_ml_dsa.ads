-------------------------------------------------------------------------------
-- ANUBIS-SPARK: liboqs ML-DSA C Bindings
-- Low-level FFI to ML-DSA-87 (NIST FIPS 204)
-------------------------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C;
with System;
with OQS_Common; use OQS_Common;

package OQS_SIG_ML_DSA is
   pragma Preelaborate;

   -------------------------------------------------------------------------
   -- ML-DSA-87: NIST Level 5 Digital Signature Algorithm
   -- Security: 256-bit equivalent (quantum-resistant)
   -- Based on CRYSTALS-Dilithium
   -------------------------------------------------------------------------

   -- Key and signature sizes (bytes)
   ML_DSA_87_LENGTH_PUBLIC_KEY  : constant := 2_592;
   ML_DSA_87_LENGTH_SECRET_KEY  : constant := 4_896;
   ML_DSA_87_LENGTH_SIGNATURE   : constant := 4_627;

   -------------------------------------------------------------------------
   -- ML-DSA-87 Operations
   -------------------------------------------------------------------------

   -- Generate a keypair
   -- public_key must point to 2,592 bytes
   -- secret_key must point to 4,896 bytes
   function OQS_SIG_ml_dsa_87_keypair (
      public_key : System.Address;
      secret_key : System.Address
   ) return OQS_STATUS with
      Import, Convention => C, External_Name => "OQS_SIG_ml_dsa_87_keypair";

   -- Sign a message
   -- signature must point to at least 4,627 bytes
   -- signature_len will be set to actual signature length (typically 4,627)
   -- message points to the message to sign
   -- message_len is the length of the message
   -- secret_key must point to 4,896 bytes
   function OQS_SIG_ml_dsa_87_sign (
      signature     : System.Address;
      signature_len : access Interfaces.C.size_t;
      message       : System.Address;
      message_len   : Interfaces.C.size_t;
      secret_key    : System.Address
   ) return OQS_STATUS with
      Import, Convention => C, External_Name => "OQS_SIG_ml_dsa_87_sign";

   -- Verify a signature
   -- message points to the message that was signed
   -- message_len is the length of the message
   -- signature points to the signature (4,627 bytes)
   -- signature_len is the length of the signature
   -- public_key must point to 2,592 bytes
   -- Returns OQS_SUCCESS if signature is valid, OQS_ERROR otherwise
   function OQS_SIG_ml_dsa_87_verify (
      message       : System.Address;
      message_len   : Interfaces.C.size_t;
      signature     : System.Address;
      signature_len : Interfaces.C.size_t;
      public_key    : System.Address
   ) return OQS_STATUS with
      Import, Convention => C, External_Name => "OQS_SIG_ml_dsa_87_verify";

   -------------------------------------------------------------------------
   -- ML-DSA-87 with Context String (Advanced)
   -- Allows signing/verifying with additional context data
   -------------------------------------------------------------------------

   -- Sign with context string
   function OQS_SIG_ml_dsa_87_sign_with_ctx_str (
      signature     : System.Address;
      signature_len : access Interfaces.C.size_t;
      message       : System.Address;
      message_len   : Interfaces.C.size_t;
      ctx           : System.Address;
      ctx_len       : Interfaces.C.size_t;
      secret_key    : System.Address
   ) return OQS_STATUS with
      Import, Convention => C,
      External_Name => "OQS_SIG_ml_dsa_87_sign_with_ctx_str";

   -- Verify with context string
   function OQS_SIG_ml_dsa_87_verify_with_ctx_str (
      message       : System.Address;
      message_len   : Interfaces.C.size_t;
      signature     : System.Address;
      signature_len : Interfaces.C.size_t;
      ctx           : System.Address;
      ctx_len       : Interfaces.C.size_t;
      public_key    : System.Address
   ) return OQS_STATUS with
      Import, Convention => C,
      External_Name => "OQS_SIG_ml_dsa_87_verify_with_ctx_str";

end OQS_SIG_ML_DSA;
