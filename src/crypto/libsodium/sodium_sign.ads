-------------------------------------------------------------------------------
-- ANUBIS-SPARK: libsodium Ed25519 C Bindings
-- Low-level FFI to Ed25519 Digital Signatures
-------------------------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C;
with System;

package Sodium_Sign is
   pragma Preelaborate;

   -------------------------------------------------------------------------
   -- Ed25519 - Edwards-curve Digital Signature Algorithm
   -- Classical signatures, quantum-vulnerable but fast and proven
   -------------------------------------------------------------------------

   -- Key and signature sizes (bytes)
   crypto_sign_ed25519_PUBLICKEYBYTES : constant := 32;
   crypto_sign_ed25519_SECRETKEYBYTES : constant := 64;  -- Note: 64 bytes (includes public key)
   crypto_sign_ed25519_BYTES          : constant := 64;
   crypto_sign_ed25519_SEEDBYTES      : constant := 32;

   -- Generic aliases
   crypto_sign_PUBLICKEYBYTES : constant := 32;
   crypto_sign_SECRETKEYBYTES : constant := 64;
   crypto_sign_BYTES          : constant := 64;
   crypto_sign_SEEDBYTES      : constant := 32;

   -------------------------------------------------------------------------
   -- Ed25519 Key Generation
   -------------------------------------------------------------------------

   -- Generate Ed25519 keypair
   -- public_key: 32 bytes output
   -- secret_key: 64 bytes output (contains both secret and public key)
   -- Returns 0 on success
   function crypto_sign_ed25519_keypair (
      public_key : System.Address;
      secret_key : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_ed25519_keypair";

   -- Alternative name (same as above)
   function crypto_sign_keypair (
      public_key : System.Address;
      secret_key : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_keypair";

   -- Derive keypair from seed (deterministic)
   -- public_key: 32 bytes output
   -- secret_key: 64 bytes output
   -- seed: 32 bytes input
   -- Returns 0 on success
   function crypto_sign_ed25519_seed_keypair (
      public_key : System.Address;
      secret_key : System.Address;
      seed       : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_ed25519_seed_keypair";

   function crypto_sign_seed_keypair (
      public_key : System.Address;
      secret_key : System.Address;
      seed       : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_seed_keypair";

   -------------------------------------------------------------------------
   -- Ed25519 Signing (Detached Signatures)
   -------------------------------------------------------------------------

   -- Sign a message (detached signature)
   -- signature: 64 bytes output
   -- signature_len: actual signature length (always 64)
   -- message: message to sign
   -- message_len: length of message
   -- secret_key: 64 bytes input
   -- Returns 0 on success
   function crypto_sign_ed25519_detached (
      signature     : System.Address;
      signature_len : access Interfaces.C.unsigned_long;
      message       : System.Address;
      message_len   : Interfaces.C.unsigned_long;
      secret_key    : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_ed25519_detached";

   function crypto_sign_detached (
      signature     : System.Address;
      signature_len : access Interfaces.C.unsigned_long;
      message       : System.Address;
      message_len   : Interfaces.C.unsigned_long;
      secret_key    : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_detached";

   -------------------------------------------------------------------------
   -- Ed25519 Verification
   -------------------------------------------------------------------------

   -- Verify a detached signature
   -- signature: 64 bytes input
   -- message: message that was signed
   -- message_len: length of message
   -- public_key: 32 bytes input
   -- Returns 0 if signature is valid, -1 if invalid
   function crypto_sign_ed25519_verify_detached (
      signature   : System.Address;
      message     : System.Address;
      message_len : Interfaces.C.unsigned_long;
      public_key  : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_ed25519_verify_detached";

   function crypto_sign_verify_detached (
      signature   : System.Address;
      message     : System.Address;
      message_len : Interfaces.C.unsigned_long;
      public_key  : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_verify_detached";

   -------------------------------------------------------------------------
   -- Ed25519 Signing (Combined Message + Signature)
   -------------------------------------------------------------------------

   -- Sign a message (signature + message combined)
   -- signed_message: (signature_len + message_len) bytes output
   -- signed_message_len: actual length of output
   -- message: message to sign
   -- message_len: length of message
   -- secret_key: 64 bytes input
   -- Returns 0 on success
   function crypto_sign_ed25519 (
      signed_message     : System.Address;
      signed_message_len : access Interfaces.C.unsigned_long;
      message            : System.Address;
      message_len        : Interfaces.C.unsigned_long;
      secret_key         : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_ed25519";

   function crypto_sign (
      signed_message     : System.Address;
      signed_message_len : access Interfaces.C.unsigned_long;
      message            : System.Address;
      message_len        : Interfaces.C.unsigned_long;
      secret_key         : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign";

   -- Verify and extract message from signed message
   -- message: message output
   -- message_len: length of extracted message
   -- signed_message: (signature + message) input
   -- signed_message_len: length of signed message
   -- public_key: 32 bytes input
   -- Returns 0 if valid and message extracted, -1 if invalid
   function crypto_sign_ed25519_open (
      message            : System.Address;
      message_len        : access Interfaces.C.unsigned_long;
      signed_message     : System.Address;
      signed_message_len : Interfaces.C.unsigned_long;
      public_key         : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_ed25519_open";

   function crypto_sign_open (
      message            : System.Address;
      message_len        : access Interfaces.C.unsigned_long;
      signed_message     : System.Address;
      signed_message_len : Interfaces.C.unsigned_long;
      public_key         : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_open";

   -------------------------------------------------------------------------
   -- Ed25519 Key Extraction
   -------------------------------------------------------------------------

   -- Extract public key from secret key
   -- public_key: 32 bytes output
   -- secret_key: 64 bytes input
   -- Returns 0 on success
   function crypto_sign_ed25519_sk_to_pk (
      public_key : System.Address;
      secret_key : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_ed25519_sk_to_pk";

   -- Extract seed from secret key
   -- seed: 32 bytes output
   -- secret_key: 64 bytes input
   -- Returns 0 on success
   function crypto_sign_ed25519_sk_to_seed (
      seed       : System.Address;
      secret_key : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_sign_ed25519_sk_to_seed";

end Sodium_Sign;
