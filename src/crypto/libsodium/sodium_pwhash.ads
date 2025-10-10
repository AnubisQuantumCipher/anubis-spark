-------------------------------------------------------------------------------
-- ANUBIS-SPARK: libsodium Argon2id C Bindings
-- Low-level FFI to Argon2id password hashing / key derivation
-------------------------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C;
with System;

package Sodium_Pwhash is
   pragma Preelaborate;

   -------------------------------------------------------------------------
   -- Argon2id - Memory-Hard Password Hashing / Key Derivation
   -- Winner of Password Hashing Competition (2015)
   -- Resistant to GPU/ASIC attacks via memory-hardness
   -------------------------------------------------------------------------

   -- Sizes
   crypto_pwhash_argon2id_SALTBYTES : constant := 16;
   crypto_pwhash_SALTBYTES          : constant := 16;

   -- String output size (for password verification)
   crypto_pwhash_argon2id_STRBYTES : constant := 128;
   crypto_pwhash_STRBYTES          : constant := 128;

   -------------------------------------------------------------------------
   -- Argon2id Parameters
   -------------------------------------------------------------------------

   -- Algorithm identifier
   crypto_pwhash_argon2id_ALG_ARGON2ID13 : constant := 2;
   crypto_pwhash_ALG_ARGON2ID13          : constant := 2;
   crypto_pwhash_ALG_DEFAULT             : constant := 2;

   -- Memory limit (bytes)
   -- INTERACTIVE: 64 MiB (fast, for interactive logins)
   -- MODERATE: 256 MiB (balanced)
   -- SENSITIVE: 1 GiB (very secure, slow)
   crypto_pwhash_argon2id_MEMLIMIT_INTERACTIVE : constant := 67_108_864;    -- 64 MiB
   crypto_pwhash_argon2id_MEMLIMIT_MODERATE    : constant := 268_435_456;   -- 256 MiB
   crypto_pwhash_argon2id_MEMLIMIT_SENSITIVE   : constant := 1_073_741_824; -- 1 GiB

   crypto_pwhash_MEMLIMIT_INTERACTIVE : constant := 67_108_864;
   crypto_pwhash_MEMLIMIT_MODERATE    : constant := 268_435_456;
   crypto_pwhash_MEMLIMIT_SENSITIVE   : constant := 1_073_741_824;

   -- Iteration count
   -- INTERACTIVE: 2 iterations (fast)
   -- MODERATE: 3 iterations (balanced)
   -- SENSITIVE: 4 iterations (very secure, slow)
   crypto_pwhash_argon2id_OPSLIMIT_INTERACTIVE : constant := 2;
   crypto_pwhash_argon2id_OPSLIMIT_MODERATE    : constant := 3;
   crypto_pwhash_argon2id_OPSLIMIT_SENSITIVE   : constant := 4;

   crypto_pwhash_OPSLIMIT_INTERACTIVE : constant := 2;
   crypto_pwhash_OPSLIMIT_MODERATE    : constant := 3;
   crypto_pwhash_OPSLIMIT_SENSITIVE   : constant := 4;

   -- Minimum/maximum values
   crypto_pwhash_argon2id_PASSWD_MIN : constant := 0;
   crypto_pwhash_argon2id_PASSWD_MAX : constant := 4_294_967_295;  -- 4 GiB
   crypto_pwhash_argon2id_BYTES_MIN  : constant := 16;
   crypto_pwhash_argon2id_BYTES_MAX  : constant := 4_294_967_295;  -- 4 GiB

   -------------------------------------------------------------------------
   -- Argon2id Key Derivation
   -------------------------------------------------------------------------

   -- Derive key from password (recommended API)
   -- out_key: derived key output (outlen bytes)
   -- outlen: desired key length (16 to 4 GiB)
   -- password: password/passphrase to derive from
   -- password_len: length of password
   -- salt: 16 bytes random salt
   -- opslimit: iteration count (use constants above)
   -- memlimit: memory limit in bytes (use constants above)
   -- alg: algorithm (use crypto_pwhash_ALG_ARGON2ID13)
   -- Returns 0 on success, -1 on failure (e.g., out of memory)
   function crypto_pwhash (
      out_key      : System.Address;
      outlen       : Interfaces.C.unsigned_long;
      password     : System.Address;
      password_len : Interfaces.C.unsigned_long;
      salt         : System.Address;
      opslimit     : Interfaces.C.unsigned_long;
      memlimit     : Interfaces.C.size_t;
      alg          : Interfaces.C.int
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_pwhash";

   -- Argon2id-specific version (same as above)
   function crypto_pwhash_argon2id (
      out_key      : System.Address;
      outlen       : Interfaces.C.unsigned_long;
      password     : System.Address;
      password_len : Interfaces.C.unsigned_long;
      salt         : System.Address;
      opslimit     : Interfaces.C.unsigned_long;
      memlimit     : Interfaces.C.size_t;
      alg          : Interfaces.C.int
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_pwhash_argon2id";

   -------------------------------------------------------------------------
   -- Password Storage (String Format)
   -------------------------------------------------------------------------

   -- Hash password into string format (for storage/verification)
   -- out_str: 128 bytes output buffer (null-terminated string)
   -- password: password to hash
   -- password_len: length of password
   -- opslimit: iteration count
   -- memlimit: memory limit
   -- Returns 0 on success, -1 on failure
   function crypto_pwhash_str (
      out_str      : System.Address;
      password     : System.Address;
      password_len : Interfaces.C.unsigned_long;
      opslimit     : Interfaces.C.unsigned_long;
      memlimit     : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_pwhash_str";

   function crypto_pwhash_argon2id_str (
      out_str      : System.Address;
      password     : System.Address;
      password_len : Interfaces.C.unsigned_long;
      opslimit     : Interfaces.C.unsigned_long;
      memlimit     : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_pwhash_argon2id_str";

   -- Verify password against stored hash string
   -- str: 128 bytes hash string (from crypto_pwhash_str)
   -- password: password to verify
   -- password_len: length of password
   -- Returns 0 if password matches, -1 if doesn't match
   function crypto_pwhash_str_verify (
      str          : System.Address;
      password     : System.Address;
      password_len : Interfaces.C.unsigned_long
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_pwhash_str_verify";

   function crypto_pwhash_argon2id_str_verify (
      str          : System.Address;
      password     : System.Address;
      password_len : Interfaces.C.unsigned_long
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_pwhash_argon2id_str_verify";

   -- Check if stored hash needs rehashing (parameters outdated)
   -- str: hash string to check
   -- opslimit: current desired iteration count
   -- memlimit: current desired memory limit
   -- Returns 0 if parameters are up-to-date, -1 if rehashing needed
   function crypto_pwhash_str_needs_rehash (
      str      : System.Address;
      opslimit : Interfaces.C.unsigned_long;
      memlimit : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_pwhash_str_needs_rehash";

   function crypto_pwhash_argon2id_str_needs_rehash (
      str      : System.Address;
      opslimit : Interfaces.C.unsigned_long;
      memlimit : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "crypto_pwhash_argon2id_str_needs_rehash";

end Sodium_Pwhash;
