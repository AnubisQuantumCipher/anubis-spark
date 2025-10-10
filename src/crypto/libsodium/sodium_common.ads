-------------------------------------------------------------------------------
-- ANUBIS-SPARK: libsodium Common C Bindings
-- Low-level FFI to libsodium common functions
-------------------------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package Sodium_Common is
   pragma Preelaborate;

   -------------------------------------------------------------------------
   -- Return Values
   -------------------------------------------------------------------------

   -- Most libsodium functions return 0 on success, -1 on failure
   SODIUM_SUCCESS : constant := 0;
   SODIUM_ERROR   : constant := -1;

   -------------------------------------------------------------------------
   -- Library Initialization
   -------------------------------------------------------------------------

   -- Initialize libsodium (must be called before any crypto operations)
   -- Returns 0 on success, 1 if already initialized, -1 on failure
   function sodium_init return Interfaces.C.int with
      Import, Convention => C, External_Name => "sodium_init";

   -------------------------------------------------------------------------
   -- Version Information
   -------------------------------------------------------------------------

   function sodium_version_string return Interfaces.C.Strings.chars_ptr with
      Import, Convention => C, External_Name => "sodium_version_string";

   -------------------------------------------------------------------------
   -- Memory Utilities
   -------------------------------------------------------------------------

   -- Zeros out memory (protected against compiler optimization)
   -- CRITICAL FOR SECURITY: Use this to zeroize secret keys
   procedure sodium_memzero (
      pnt : System.Address;
      len : Interfaces.C.size_t
   ) with
      Import, Convention => C, External_Name => "sodium_memzero";

   -- Lock memory to prevent swapping to disk
   function sodium_mlock (
      addr : System.Address;
      len  : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "sodium_mlock";

   -- Unlock previously locked memory
   function sodium_munlock (
      addr : System.Address;
      len  : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "sodium_munlock";

   -- Constant-time memory comparison
   -- Returns 0 if equal, non-zero otherwise
   function sodium_memcmp (
      b1  : System.Address;
      b2  : System.Address;
      len : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "sodium_memcmp";

   -- Constant-time test for all-zero buffer
   -- Returns 1 if all bytes are 0, 0 otherwise
   function sodium_is_zero (
      n   : System.Address;
      len : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "sodium_is_zero";

   -------------------------------------------------------------------------
   -- Random Number Generation
   -------------------------------------------------------------------------

   -- Generate random bytes (cryptographically secure)
   procedure randombytes_buf (
      buf  : System.Address;
      size : Interfaces.C.size_t
   ) with
      Import, Convention => C, External_Name => "randombytes_buf";

   -- Generate random 32-bit integer
   function randombytes_random return Interfaces.C.unsigned with
      Import, Convention => C, External_Name => "randombytes_random";

   -- Generate random 32-bit integer in range [0, upper_bound)
   function randombytes_uniform (
      upper_bound : Interfaces.C.unsigned
   ) return Interfaces.C.unsigned with
      Import, Convention => C, External_Name => "randombytes_uniform";

   -------------------------------------------------------------------------
   -- Utility Functions
   -------------------------------------------------------------------------

   -- Convert binary to hex string
   function sodium_bin2hex (
      hex     : System.Address;
      hex_maxlen : Interfaces.C.size_t;
      bin     : System.Address;
      bin_len : Interfaces.C.size_t
   ) return Interfaces.C.Strings.chars_ptr with
      Import, Convention => C, External_Name => "sodium_bin2hex";

   -- Convert hex string to binary
   function sodium_hex2bin (
      bin        : System.Address;
      bin_maxlen : Interfaces.C.size_t;
      hex        : Interfaces.C.Strings.chars_ptr;
      hex_len    : Interfaces.C.size_t;
      ignore     : Interfaces.C.Strings.chars_ptr;
      bin_len    : access Interfaces.C.size_t;
      hex_end    : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "sodium_hex2bin";

end Sodium_Common;
