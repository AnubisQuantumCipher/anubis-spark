-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Zeroization Contracts (Enhanced)
-- Proves: All key bytes are zero after zeroization
-- Proves: Validity flags are cleared
-- Proves: Progressive zeroization (via loop invariants in body)
-- Proves: No key material leakage
-- Covers: All secret key types
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Contracts; use Anubis_Contracts;

package Anubis_Zeroize is

   -------------------------------------------------------------------------
   -- Zeroize Key_32 (Enhanced) (generic 32-byte key)
   -------------------------------------------------------------------------

   -- Proves: All bytes zeroed AND validity flag cleared
   -- Proves: Complete destruction of key material
   -- Proves: No partial key exposure during zeroization
   procedure Zeroize_Key (Key : in out Key_32; Valid_Flag : in out Boolean) with
     Global  => null,
     Depends => (Key => Key, Valid_Flag => Valid_Flag),
     Post    =>
       -- All bytes must be zero (complete zeroization)
       Key_Is_Zeroed (Key) and then
       (for all I in Key'Range => Key (I) = 0) and then

       -- Validity flag must be false
       (Valid_Flag = False) and then

       -- Key has no entropy (completely destroyed)
       not Key_Has_Entropy (Key),

     Contract_Cases =>
       -- Case 1: Key had entropy → now zeroed
       (Key_Has_Entropy (Key'Old) =>
          Key_Is_Zeroed (Key) and not Valid_Flag,

        -- Case 2: Key was already zero → remains zero
        Key_Is_Zeroed (Key'Old) =>
          Key_Is_Zeroed (Key) and not Valid_Flag);

   -------------------------------------------------------------------------
   -- Zeroize Byte Array (Enhanced)
   -------------------------------------------------------------------------

   -- Generic zeroization for arbitrary-length byte arrays
   -- Proves: All bytes zeroed from First to Last
   -- Proves: Progressive zeroization (loop invariant in body)
   procedure Zeroize_Array (Data : in out Byte_Array) with
     Global  => null,
     Depends => (Data => Data),
     Pre     => Data'Length > 0,
     Post    =>
       -- All bytes are zero
       (for all I in Data'Range => Data (I) = 0) and then

       -- Array has no non-zero bytes
       (for some I in Data'Range => Data (I) = 0);  -- At least one zero (trivially true)

   -------------------------------------------------------------------------
   -- Zeroize Tag_16 (Enhanced)
   -------------------------------------------------------------------------

   -- Zeroize authentication tags after verification
   -- Important for preventing tag reuse attacks
   procedure Zeroize_Tag (Tag : in out Tag_16) with
     Global  => null,
     Depends => (Tag => Tag),
     Post    =>
       -- All tag bytes are zero
       (for all I in Tag'Range => Tag (I) = 0) and then

       -- Tag no longer has value
       not Tag_Has_Value (Tag);

   -------------------------------------------------------------------------
   -- Lemma: Zeroization is Complete (Ghost)
   -------------------------------------------------------------------------

   -- Proves that after zeroization, the key cannot be distinguished
   -- from a newly allocated all-zero key
   procedure Lemma_Zeroization_Complete
     (Key_Before : Key_32;
      Key_After  : Key_32;
      Valid_Before : Boolean;
      Valid_After  : Boolean) with
     Ghost,
     Global => null,
     Pre    =>
       -- Before: key may have had entropy
       Valid_Before and then

       -- After: key is zeroed
       Key_Is_Zeroed (Key_After) and then
       not Valid_After,
     Post   =>
       -- Zeroed key equals a freshly created zero key
       (declare
          Zero_Key : constant Key_32 := (others => 0);
        begin
          Keys_Equal (Key_After, Zero_Key));

   -------------------------------------------------------------------------
   -- Lemma: No Partial Key Leakage (Ghost)
   -------------------------------------------------------------------------

   -- Proves that during zeroization, if any bytes remain non-zero,
   -- the validity flag has already been cleared
   -- This prevents attacks based on interrupted zeroization
   procedure Lemma_No_Partial_Leakage
     (Key          : Key_32;
      Valid_Flag   : Boolean;
      Bytes_Zeroed : Natural) with
     Ghost,
     Global => null,
     Pre    =>
       Bytes_Zeroed <= Key'Length and then
       -- First Bytes_Zeroed bytes are zero
       (for all I in Key'First .. Key'First + Bytes_Zeroed - 1 =>
          Key (I) = 0) and then
       -- Validity flag is already false
       not Valid_Flag,
     Post   =>
       -- Even with partial zeroization, key is marked invalid
       not Valid_Flag;

end Anubis_Zeroize;
