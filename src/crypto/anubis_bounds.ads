-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Bounds-Safe Array Access Helpers (Enhanced)
-- Provides proven-safe array indexing and slicing to eliminate GNATprove
-- "array index check might fail" warnings
-- Proves: Value preservation through slicing operations
-- Proves: No out-of-bounds access possible
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;

package Anubis_Bounds is

   -------------------------------------------------------------------------
   -- Safe Slice: Proven bounds on array slicing (Enhanced)
   -------------------------------------------------------------------------

   -- Returns A[Offset .. Offset+Count-1] with proved bounds
   -- Precondition ensures the slice is within array bounds
   -- Proves: Result contains exact copy of source bytes
   -- Proves: Result array is properly indexed from 1
   function Safe_Slice
     (A      : Byte_Array;
      Offset : Natural;
      Count  : Natural) return Byte_Array
   with
     Global => null,
     Pre    =>
       Count > 0 and then
       Offset < A'Length and then
       Offset <= A'Length - Count and then
       Count <= A'Length,
     Post   =>
       -- Length is exactly as requested
       Safe_Slice'Result'Length = Count and then

       -- Result is 1-indexed (canonical form)
       Safe_Slice'Result'First = 1 and then
       Safe_Slice'Result'Last = Count and then

       -- Values are preserved byte-for-byte
       (for all I in 0 .. Count - 1 =>
          Safe_Slice'Result (Safe_Slice'Result'First + I) =
          A (A'First + Offset + I)),

     Contract_Cases =>
       -- Case 1: Valid slice within bounds
       (Count > 0 and Offset + Count <= A'Length =>
          Safe_Slice'Result'Length = Count and
          Safe_Slice'Result'First = 1);

   -------------------------------------------------------------------------
   -- Safe Element Access: Proven bounds on single element (Enhanced)
   -------------------------------------------------------------------------

   -- Returns A[A'First + I] with proved bounds (0-based index I)
   -- Precondition ensures the index is within array bounds
   -- Proves: Returned value equals source value at that index
   function Safe_At (A : Byte_Array; I : Natural) return Byte
   with
     Global => null,
     Pre    =>
       I < A'Length and then
       A'First + I <= A'Last,
     Post   =>
       -- Value is preserved
       Safe_At'Result = A (A'First + I),

     Contract_Cases =>
       -- Case 1: Valid index
       (I < A'Length =>
          Safe_At'Result = A (A'First + I));

   -------------------------------------------------------------------------
   -- Safe Write: Proven bounds on single element write (Enhanced)
   -------------------------------------------------------------------------

   -- Writes Value to A[A'First + I] with proved bounds
   -- Proves: Only the target index is modified
   -- Proves: All other elements unchanged
   procedure Safe_Write
     (A     : in out Byte_Array;
      I     : Natural;
      Value : Byte)
   with
     Global  => null,
     Depends => (A => (A, I, Value)),
     Pre     =>
       I < A'Length and then
       A'First + I <= A'Last,
     Post    =>
       -- Target index has new value
       A (A'First + I) = Value and then

       -- All other indices unchanged
       (for all J in A'Range =>
          (if J /= A'First + I then A (J) = A'Old (J)));

   -------------------------------------------------------------------------
   -- Lemma: Slice Preserves Values (Ghost)
   -------------------------------------------------------------------------

   -- Proves that slicing preserves byte values exactly
   procedure Lemma_Slice_Preserves_Values
     (A      : Byte_Array;
      Offset : Natural;
      Count  : Natural) with
     Ghost,
     Global => null,
     Pre    =>
       Count > 0 and then
       Offset < A'Length and then
       Offset + Count <= A'Length,
     Post   =>
       (declare
          S : constant Byte_Array := Safe_Slice (A, Offset, Count);
        begin
          (for all I in 0 .. Count - 1 =>
             S (S'First + I) = A (A'First + Offset + I)));

   -------------------------------------------------------------------------
   -- Lemma: No Out of Bounds Access (Ghost)
   -------------------------------------------------------------------------

   -- Proves that Safe_At never accesses beyond array bounds
   procedure Lemma_No_OOB_Access
     (A : Byte_Array;
      I : Natural) with
     Ghost,
     Global => null,
     Pre    => I < A'Length,
     Post   =>
       A'First + I >= A'First and then
       A'First + I <= A'Last;

end Anubis_Bounds;
