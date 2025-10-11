-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Bounds-Safe Array Access Helpers
-- Provides proven-safe array indexing and slicing to eliminate GNATprove
-- "array index check might fail" warnings
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;

package Anubis_Bounds is

   -------------------------------------------------------------------------
   -- Safe Slice: Proven bounds on array slicing
   -------------------------------------------------------------------------

   -- Returns A[Offset .. Offset+Count-1] with proved bounds
   -- Precondition ensures the slice is within array bounds
   function Safe_Slice
     (A      : Byte_Array;
      Offset : Natural;
      Count  : Natural) return Byte_Array
   with
     Global => null,
     Pre    => Count > 0 and then Offset <= A'Length - Count,
     Post   => Safe_Slice'Result'Length = Count
               and then Safe_Slice'Result'First = 1;

   -------------------------------------------------------------------------
   -- Safe Element Access: Proven bounds on single element
   -------------------------------------------------------------------------

   -- Returns A[A'First + I] with proved bounds (0-based index I)
   -- Precondition ensures the index is within array bounds
   function Safe_At (A : Byte_Array; I : Natural) return Byte
   with
     Global => null,
     Pre    => I < A'Length;

end Anubis_Bounds;
