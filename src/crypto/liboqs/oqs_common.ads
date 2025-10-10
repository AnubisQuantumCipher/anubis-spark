-------------------------------------------------------------------------------
-- ANUBIS-SPARK: liboqs Common C Bindings
-- Low-level FFI to liboqs common functions
-------------------------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package OQS_Common is
   pragma Preelaborate;

   -------------------------------------------------------------------------
   -- OQS_STATUS: Return values from liboqs functions
   -------------------------------------------------------------------------

   type OQS_STATUS is new Interfaces.C.int;

   OQS_SUCCESS : constant OQS_STATUS := 0;
   OQS_ERROR   : constant OQS_STATUS := -1;
   OQS_EXTERNAL_LIB_ERROR_OPENSSL : constant OQS_STATUS := 50;

   -------------------------------------------------------------------------
   -- Initialization and cleanup
   -------------------------------------------------------------------------

   -- Initialize liboqs library (prefetch OpenSSL objects if needed)
   procedure OQS_init with
      Import, Convention => C, External_Name => "OQS_init";

   -- Stop OpenSSL threads (call before thread termination)
   procedure OQS_thread_stop with
      Import, Convention => C, External_Name => "OQS_thread_stop";

   -- Cleanup and free prefetched OpenSSL objects
   procedure OQS_destroy with
      Import, Convention => C, External_Name => "OQS_destroy";

   -- Get library version string
   function OQS_version return Interfaces.C.Strings.chars_ptr with
      Import, Convention => C, External_Name => "OQS_version";

   -------------------------------------------------------------------------
   -- Memory management
   -------------------------------------------------------------------------

   -- Zeros out memory (protected against compiler optimization)
   -- CRITICAL FOR SECURITY: Use this to zeroize secret keys
   procedure OQS_MEM_cleanse (
      ptr : System.Address;
      len : Interfaces.C.size_t
   ) with
      Import, Convention => C, External_Name => "OQS_MEM_cleanse";

   -- Zeros out and frees memory (secure deallocation)
   procedure OQS_MEM_secure_free (
      ptr : System.Address;
      len : Interfaces.C.size_t
   ) with
      Import, Convention => C, External_Name => "OQS_MEM_secure_free";

   -- Constant-time memory comparison
   -- Returns 0 if equal, 1 if different
   function OQS_MEM_secure_bcmp (
      a   : System.Address;
      b   : System.Address;
      len : Interfaces.C.size_t
   ) return Interfaces.C.int with
      Import, Convention => C, External_Name => "OQS_MEM_secure_bcmp";

end OQS_Common;
