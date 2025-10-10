-------------------------------------------------------------------------------
-- ANUBIS-SPARK: libsodium X25519 C Bindings
-- Low-level FFI to X25519 Elliptic Curve Diffie-Hellman
-------------------------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C;
with System;

package Sodium_Scalarmult is
   pragma Preelaborate;

   -------------------------------------------------------------------------
   -- X25519 (Curve25519) - Elliptic Curve Diffie-Hellman
   -- Classical ECDH, quantum-vulnerable but fast and proven
   -------------------------------------------------------------------------

   -- Key sizes (bytes)
   crypto_scalarmult_curve25519_BYTES       : constant := 32;
   crypto_scalarmult_curve25519_SCALARBYTES : constant := 32;

   crypto_scalarmult_BYTES       : constant := 32;
   crypto_scalarmult_SCALARBYTES : constant := 32;

   -------------------------------------------------------------------------
   -- X25519 Key Generation
   -------------------------------------------------------------------------

   -- Generate X25519 keypair
   -- public_key: 32 bytes output (can be shared)
   -- secret_key: 32 bytes output (must be kept secret)
   -- Returns 0 on success
   function crypto_box_keypair (
      public_key : System.Address;
      secret_key : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_box_keypair";

   -- Derive public key from secret key
   -- public_key: 32 bytes output
   -- secret_key: 32 bytes input
   -- Returns 0 on success, -1 on failure
   function crypto_scalarmult_curve25519_base (
      public_key : System.Address;
      secret_key : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_scalarmult_curve25519_base";

   -- Alternative name (same as above)
   function crypto_scalarmult_base (
      public_key : System.Address;
      secret_key : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_scalarmult_base";

   -------------------------------------------------------------------------
   -- X25519 Shared Secret Computation
   -------------------------------------------------------------------------

   -- Compute shared secret from our secret key and peer's public key
   -- shared_secret: 32 bytes output
   -- our_secret_key: 32 bytes input (our secret)
   -- their_public_key: 32 bytes input (peer's public)
   -- Returns 0 on success, -1 on failure (e.g., weak public key)
   function crypto_scalarmult_curve25519 (
      shared_secret    : System.Address;
      our_secret_key   : System.Address;
      their_public_key : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_scalarmult_curve25519";

   -- Alternative name (same as above)
   function crypto_scalarmult (
      shared_secret    : System.Address;
      our_secret_key   : System.Address;
      their_public_key : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_scalarmult";

   -------------------------------------------------------------------------
   -- High-Level crypto_box (Authenticated Encryption with X25519)
   -------------------------------------------------------------------------

   -- Sizes for crypto_box (NaCl box construction)
   crypto_box_PUBLICKEYBYTES  : constant := 32;
   crypto_box_SECRETKEYBYTES  : constant := 32;
   crypto_box_NONCEBYTES      : constant := 24;
   crypto_box_MACBYTES        : constant := 16;
   crypto_box_BEFORENMBYTES   : constant := 32;

   -- Precompute shared secret for multiple messages
   -- This does X25519 ECDH + HSalsa20
   -- shared_secret: 32 bytes output
   -- their_public_key: 32 bytes input
   -- our_secret_key: 32 bytes input
   -- Returns 0 on success
   function crypto_box_beforenm (
      shared_secret    : System.Address;
      their_public_key : System.Address;
      our_secret_key   : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_box_beforenm";

end Sodium_Scalarmult;
