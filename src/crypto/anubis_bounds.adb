-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Bounds-Safe Array Access Implementation
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- Body uses direct array access

with Anubis_Types; use Anubis_Types;

package body Anubis_Bounds is

   -------------------------------------------------------------------------
   -- Safe Slice Implementation
   -------------------------------------------------------------------------

   function Safe_Slice
     (A      : Byte_Array;
      Offset : Natural;
      Count  : Natural) return Byte_Array
   is
   begin
      -- Precondition guarantees this is safe
      return A (A'First + Offset .. A'First + Offset + Count - 1);
   end Safe_Slice;

   -------------------------------------------------------------------------
   -- Safe Element Access Implementation
   -------------------------------------------------------------------------

   function Safe_At (A : Byte_Array; I : Natural) return Byte is
   begin
      -- Precondition guarantees this is safe
      return A (A'First + I);
   end Safe_At;

end Anubis_Bounds;
