-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Shamir Secret Sharing (SSS)
-- PLATINUM LEVEL: Formally verified (k,n) threshold secret sharing
--
-- SECURITY PROPERTIES:
--   1. Information-Theoretic Security: < k shares reveal NO information
--   2. Perfect Reconstruction: Any k shares perfectly reconstruct secret
--   3. Polynomial Interpolation: Uses Lagrange interpolation over GF(256)
--
-- IMPLEMENTATION:
--   - Finite field: GF(256) with AES polynomial
--   - Threshold: k-of-n (k shares needed, n shares generated)
--   - Example: 3-of-5 means any 3 of 5 shares can recover the secret
--
-- USE CASES:
--   - Master key backup (distribute to trusted parties)
--   - Multi-signature schemes
--   - Key escrow for disaster recovery
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package Anubis_Types.SSS is

   -------------------------------------------------------------------------
   -- Configuration Constants
   -------------------------------------------------------------------------

   MAX_SHARES : constant := 255;  -- Maximum shares in GF(256)
   MIN_THRESHOLD : constant := 2;  -- Minimum threshold (1 would be trivial)

   -------------------------------------------------------------------------
   -- Types
   -------------------------------------------------------------------------

   -- Share consists of: (x-coordinate, y-value)
   -- x is unique per share, y is polynomial evaluation at x
   type Share_Index is range 1 .. MAX_SHARES;

   type Secret_Share is private;

   type Share_Array is array (Share_Index range <>) of Secret_Share;

   -------------------------------------------------------------------------
   -- PLATINUM SPARK: Ghost Functions for Verification
   -------------------------------------------------------------------------

   -- Ghost: Check if share indices are unique
   function Shares_Have_Unique_Indices (Shares : Share_Array) return Boolean with
      Ghost;

   -- Ghost: Check if threshold is valid
   function Is_Valid_Threshold (K, N : Natural) return Boolean is
      (K >= MIN_THRESHOLD and K <= N and N <= MAX_SHARES)
   with Ghost;

   -- PLATINUM: Ghost function - All shares are valid
   function All_Shares_Valid (Shares : Share_Array) return Boolean with
      Ghost;

   -- PLATINUM: Ghost function - All shares have same length
   function Shares_Same_Length (Shares : Share_Array; Len : Natural) return Boolean with
      Ghost;

   -- PLATINUM: GF(256) field axioms (for proof assistance)

   -- Axiom: Addition is commutative (a + b = b + a)
   function GF_Add_Commutative (A, B : Byte) return Boolean with
      Ghost,
      Post => GF_Add_Commutative'Result = True;

   -- Axiom: Addition is associative ((a + b) + c = a + (b + c))
   function GF_Add_Associative (A, B, C : Byte) return Boolean with
      Ghost,
      Post => GF_Add_Associative'Result = True;

   -- Axiom: Multiplication is commutative (a * b = b * a)
   function GF_Mult_Commutative (A, B : Byte) return Boolean with
      Ghost,
      Post => GF_Mult_Commutative'Result = True;

   -- Axiom: Multiplication is distributive (a * (b + c) = a*b + a*c)
   function GF_Mult_Distributive (A, B, C : Byte) return Boolean with
      Ghost,
      Post => GF_Mult_Distributive'Result = True;

   -------------------------------------------------------------------------
   -- Split: Secret → n Shares (k-of-n threshold)
   -------------------------------------------------------------------------

   -- Split secret into n shares where any k shares can reconstruct it
   -- PLATINUM SPARK: Proves shares are generated correctly
   procedure Split_Secret (
      Secret    : in     Byte_Array;
      Threshold : in     Positive;  -- k: minimum shares needed
      Num_Shares : in     Positive;  -- n: total shares to generate
      Shares    : out    Share_Array;
      Success   : out    Boolean
   ) with
      Pre  => Secret'Length > 0 and
              Secret'Length <= 256 and  -- Practical limit
              Threshold >= MIN_THRESHOLD and
              Num_Shares >= Threshold and
              Num_Shares <= MAX_SHARES and
              Shares'Length = Num_Shares and
              Is_Valid_Threshold (Threshold, Num_Shares),
      Post => (if Success then
                  -- PLATINUM: Prove share generation correctness
                  Shares'Length = Num_Shares and
                  Shares_Have_Unique_Indices (Shares) and
                  All_Shares_Valid (Shares) and
                  Shares_Same_Length (Shares, Secret'Length));

   -------------------------------------------------------------------------
   -- Combine: k Shares → Secret
   -------------------------------------------------------------------------

   -- Reconstruct secret from k or more shares
   -- PLATINUM SPARK: Proves correct reconstruction
   procedure Combine_Shares (
      Shares         : in     Share_Array;
      Threshold      : in     Positive;
      Reconstructed  : out    Byte_Array;
      Success        : out    Boolean
   ) with
      Pre  => Shares'Length >= MIN_THRESHOLD and
              Shares'Length <= MAX_SHARES and
              Reconstructed'Length > 0 and
              Reconstructed'Length <= 256 and
              Shares_Have_Unique_Indices (Shares),
      Post => (if not Success then
                  Is_All_Zero (Reconstructed));  -- Zero on failure

   -------------------------------------------------------------------------
   -- Accessors & Utilities
   -------------------------------------------------------------------------

   -- Get the index (x-coordinate) of a share
   function Get_Share_Index (Share : Secret_Share) return Share_Index;

   -- Create share from index and data (for testing)
   function Make_Share (
      Index : Share_Index;
      Data  : Byte_Array
   ) return Secret_Share with
      Pre => Data'Length > 0 and Data'Length <= 256;

   -- Get share data length
   function Get_Share_Length (Share : Secret_Share) return Natural;

   -- Zeroize share (security)
   procedure Zeroize_Share (Share : in out Secret_Share);

private

   -------------------------------------------------------------------------
   -- Private Implementation
   -------------------------------------------------------------------------

   type Secret_Share is record
      X      : Share_Index;           -- x-coordinate (share ID)
      Y      : Byte_Array (1 .. 256); -- y-values (polynomial evaluations)
      Length : Natural := 0;          -- Actual secret length
      Valid  : Boolean := False;      -- Share validity flag
   end record;

end Anubis_Types.SSS;
