-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Header AAD Binding (Implementation)
-- Uses BLAKE2b-256 for header hashing
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- FFI to libsodium

with Sodium_Hash;
with Interfaces.C; use Interfaces.C;
with System;

package body Anubis_Types.Header_AAD is

   -------------------------------------------------------------------------
   -- Compute Header AAD
   -------------------------------------------------------------------------

   function Compute_Header_AAD (
      Header_Preamble : Byte_Array
   ) return Byte_Array
   is
      AAD         : Byte_Array (1 .. 32);
      Rc          : Interfaces.C.int;
   begin

      -- Hash header data with BLAKE2b-256 (keyless)
      Rc := Sodium_Hash.crypto_generichash (
         Output     => AAD (AAD'First)'Address,
         Output_Len => 32,
         Input      => Header_Preamble (Header_Preamble'First)'Address,
         Input_Len  => Interfaces.C.unsigned_long_long (Header_Preamble'Length),
         Key        => System.Null_Address,  -- Keyless hash
         Key_Len    => 0
      );

      if Rc /= 0 then
         -- Hash failed (should never happen with libsodium)
         AAD := (others => 0);
      end if;

      return AAD;
   end Compute_Header_AAD;

end Anubis_Types.Header_AAD;
