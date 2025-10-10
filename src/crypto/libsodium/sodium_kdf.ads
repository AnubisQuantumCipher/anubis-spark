-------------------------------------------------------------------------------
-- ANUBIS-SPARK: libsodium KDF (HKDF-SHA256/SHA512) C Bindings
-- Low-level FFI to HKDF key derivation functions
-------------------------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C;
with System;

package Sodium_KDF is
   pragma Preelaborate;

   -------------------------------------------------------------------------
   -- HKDF-SHA256 - HMAC-based Extract-and-Expand Key Derivation Function
   -- RFC 5869 - Standard key derivation from shared secrets
   -------------------------------------------------------------------------

   -- Key sizes
   crypto_kdf_hkdf_sha256_KEYBYTES   : constant := 32;
   crypto_kdf_hkdf_sha256_BYTES_MIN  : constant := 0;
   crypto_kdf_hkdf_sha256_BYTES_MAX  : constant := 8160;  -- 255 * 32

   -------------------------------------------------------------------------
   -- HKDF-SHA256 Extract
   -------------------------------------------------------------------------

   -- Extract: Convert input keying material into pseudorandom key
   -- prk: pseudorandom key output (32 bytes)
   -- salt: optional salt (can be NULL)
   -- salt_len: length of salt (0 if NULL)
   -- ikm: input keying material
   -- ikm_len: length of input keying material
   -- Returns 0 on success
   function crypto_kdf_hkdf_sha256_extract (
      prk      : System.Address;
      salt     : System.Address;
      salt_len : Interfaces.C.size_t;
      ikm      : System.Address;
      ikm_len  : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_kdf_hkdf_sha256_extract";

   -------------------------------------------------------------------------
   -- HKDF-SHA256 Expand
   -------------------------------------------------------------------------

   -- Expand: Derive multiple keys from pseudorandom key
   -- out_key: derived key output (out_len bytes)
   -- out_len: desired key length (0 to 8160 bytes)
   -- ctx: context/info string (application-specific)
   -- ctx_len: length of context
   -- prk: pseudorandom key from extract (32 bytes)
   -- Returns 0 on success, -1 on failure
   function crypto_kdf_hkdf_sha256_expand (
      out_key : System.Address;
      out_len : Interfaces.C.size_t;
      ctx     : System.Address;
      ctx_len : Interfaces.C.size_t;
      prk     : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_kdf_hkdf_sha256_expand";

   -------------------------------------------------------------------------
   -- HKDF-SHA256 Combined (Extract + Expand)
   -------------------------------------------------------------------------

   -- Derive key directly from input keying material
   -- out_key: derived key output (out_len bytes)
   -- out_len: desired key length (0 to 8160 bytes)
   -- ctx: context/info string (application-specific)
   -- ctx_len: length of context
   -- ikm: input keying material
   -- ikm_len: length of input keying material
   -- salt: optional salt (can be NULL)
   -- salt_len: length of salt (0 if NULL)
   -- Returns 0 on success, -1 on failure
   function crypto_kdf_hkdf_sha256_derive_from_key (
      out_key  : System.Address;
      out_len  : Interfaces.C.size_t;
      ctx      : System.Address;
      ctx_len  : Interfaces.C.size_t;
      ikm      : System.Address;
      ikm_len  : Interfaces.C.size_t;
      salt     : System.Address;
      salt_len : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_kdf_hkdf_sha256_derive_from_key";

   -------------------------------------------------------------------------
   -- HKDF-SHA512 - Larger output variant
   -------------------------------------------------------------------------

   crypto_kdf_hkdf_sha512_KEYBYTES  : constant := 64;
   crypto_kdf_hkdf_sha512_BYTES_MIN : constant := 0;
   crypto_kdf_hkdf_sha512_BYTES_MAX : constant := 16_320;  -- 255 * 64

   -- HKDF-SHA512 Extract
   function crypto_kdf_hkdf_sha512_extract (
      prk      : System.Address;
      salt     : System.Address;
      salt_len : Interfaces.C.size_t;
      ikm      : System.Address;
      ikm_len  : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_kdf_hkdf_sha512_extract";

   -- HKDF-SHA512 Expand
   function crypto_kdf_hkdf_sha512_expand (
      out_key : System.Address;
      out_len : Interfaces.C.size_t;
      ctx     : System.Address;
      ctx_len : Interfaces.C.size_t;
      prk     : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_kdf_hkdf_sha512_expand";

   -- HKDF-SHA512 Combined
   function crypto_kdf_hkdf_sha512_derive_from_key (
      out_key  : System.Address;
      out_len  : Interfaces.C.size_t;
      ctx      : System.Address;
      ctx_len  : Interfaces.C.size_t;
      ikm      : System.Address;
      ikm_len  : Interfaces.C.size_t;
      salt     : System.Address;
      salt_len : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_kdf_hkdf_sha512_derive_from_key";

   -------------------------------------------------------------------------
   -- Simple KDF (Blake2b-based, for deriving subkeys)
   -------------------------------------------------------------------------

   crypto_kdf_BYTES_MIN       : constant := 16;
   crypto_kdf_BYTES_MAX       : constant := 64;
   crypto_kdf_CONTEXTBYTES    : constant := 8;
   crypto_kdf_KEYBYTES        : constant := 32;

   -- Derive subkey from master key
   -- subkey: derived subkey output (subkey_len bytes)
   -- subkey_len: desired subkey length (16 to 64 bytes)
   -- subkey_id: subkey ID (0 to 2^64-1)
   -- ctx: 8-byte context (application-specific)
   -- key: 32-byte master key
   -- Returns 0 on success
   function crypto_kdf_derive_from_key (
      subkey     : System.Address;
      subkey_len : Interfaces.C.size_t;
      subkey_id  : Interfaces.C.unsigned_long;
      ctx        : System.Address;
      key        : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_kdf_derive_from_key";

   -- Generate random master key
   procedure crypto_kdf_keygen (
      key : System.Address
   ) with
      Import, Convention => C,
      External_Name => "crypto_kdf_keygen";

end Sodium_KDF;
