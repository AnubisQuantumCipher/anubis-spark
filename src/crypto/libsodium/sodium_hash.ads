-------------------------------------------------------------------------------
-- Libsodium Ada FFI: crypto_generichash (BLAKE2b)
-- Provides cryptographic hashing for AAD binding
-------------------------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C; use Interfaces.C;
with System;

package Sodium_Hash is
   pragma SPARK_Mode (Off);  -- FFI bindings

   -------------------------------------------------------------------------
   -- Constants
   -------------------------------------------------------------------------

   CRYPTO_GENERICHASH_BYTES : constant := 32;  -- BLAKE2b-256 output
   CRYPTO_GENERICHASH_KEYBYTES : constant := 32;

   -------------------------------------------------------------------------
   -- crypto_generichash (BLAKE2b)
   -------------------------------------------------------------------------

   -- Simple one-shot hash (keyless)
   function crypto_generichash
     (Output     : System.Address;
      Output_Len : size_t;
      Input      : System.Address;
      Input_Len  : unsigned_long_long;
      Key        : System.Address;
      Key_Len    : size_t)
      return int
   with
      Import        => True,
      Convention    => C,
      External_Name => "crypto_generichash";

end Sodium_Hash;
