-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Secure Entropy Source
-- Cryptographically secure random number generation
-- Uses platform-specific secure sources (/dev/urandom, getrandom(), etc.)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;

package Anubis_Entropy is

   -------------------------------------------------------------------------
   -- Entropy Sources (in priority order):
   --
   -- 1. Hardware RNG (RDRAND/RDSEED on x86, RNDR on ARM)
   -- 2. Operating System:
   --    - macOS: SecRandomCopyBytes / arc4random_buf
   --    - Linux: getrandom() syscall
   --    - BSD: getentropy()
   --    - Fallback: /dev/urandom
   -- 3. liboqs internal RNG (ChaCha20-based DRBG)
   --
   -- Security Properties:
   -- - Non-blocking (always returns immediately)
   -- - Forward secrecy (compromise doesn't reveal past outputs)
   -- - Backtracking resistance (compromise doesn't reveal future outputs)
   -- - Prediction resistance (reseeded frequently)
   -------------------------------------------------------------------------

   Entropy_Error : exception;

   -- Generate cryptographically secure random bytes
   procedure Generate_Random_Bytes (
      Output  : out Byte_Array;
      Success : out Boolean
   ) with
      Global => null,  -- No global state (important for SPARK)
      Post   => (if not Success then (for all B of Output => B = 0));

   -- Fill a buffer with secure random data
   procedure Fill_Random (
      Buffer  : out Byte_Array;
      Success : out Boolean
   ) with
      Post => (if not Success then (for all B of Buffer => B = 0));

   -------------------------------------------------------------------------
   -- Specialized random generators for specific types
   -------------------------------------------------------------------------

   -- Generate random salt for Argon2
   procedure Generate_Random_Salt (
      Salt    : out Argon2_Salt;
      Success : out Boolean
   );

   -- Generate random nonce for XChaCha20
   procedure Generate_Random_Nonce (
      Nonce   : out XChaCha20_Nonce;
      Success : out Boolean
   );

   -- Generate random symmetric key
   procedure Generate_Random_Key (
      Key     : out XChaCha20_Key;
      Success : out Boolean
   ) with
      Post => (if Success then Is_Valid (Key));

   -------------------------------------------------------------------------
   -- Entropy Health Checks
   -------------------------------------------------------------------------

   -- Test entropy source is working correctly
   function Test_Entropy_Source return Boolean;

   -- Get entropy source name (for diagnostics)
   function Get_Entropy_Source_Name return String;

   -- Check if hardware RNG is available
   function Has_Hardware_RNG return Boolean;

   -------------------------------------------------------------------------
   -- Entropy Validation (Defensive Security)
   -------------------------------------------------------------------------

   -- Comprehensive entropy validation
   -- Checks for: all-zeros, repeating patterns, minimum Hamming weight
   -- SECURITY: Detects weak RNG output before using in crypto operations
   function Has_Sufficient_Entropy (Data : Byte_Array) return Boolean with
      Pre  => Data'Length >= 16,
      Post => (if Has_Sufficient_Entropy'Result then
                  not Is_All_Zeros (Data) and
                  not Is_Repeating_Pattern (Data) and
                  Hamming_Weight (Data) >= Data'Length * 2 and  -- At least 25% bits set
                  Unique_Byte_Count (Data) >= 8);  -- At least 8 different bytes

   -- Check if data is all zeros (weak/failed RNG)
   function Is_All_Zeros (Data : Byte_Array) return Boolean with
      Post => Is_All_Zeros'Result = (for all B of Data => B = 0);

   -- Check if data has obvious repeating pattern (same byte repeated)
   function Is_Repeating_Pattern (Data : Byte_Array) return Boolean with
      Pre  => Data'Length > 0,
      Post => (if Is_Repeating_Pattern'Result then
                  (for all B of Data => B = Data (Data'First)));

   -- Count number of set bits (Hamming weight)
   -- Higher weight = better entropy distribution
   function Hamming_Weight (Data : Byte_Array) return Natural with
      Post => Hamming_Weight'Result <= Data'Length * 8;

   -- Count number of unique byte values
   -- More unique bytes = better entropy
   function Unique_Byte_Count (Data : Byte_Array) return Natural with
      Post => Unique_Byte_Count'Result in 0 .. 256;

   -------------------------------------------------------------------------
   -- Entropy Estimation (for compliance/auditing)
   -------------------------------------------------------------------------

   -- Estimate bits of entropy per byte (should be ~8)
   function Estimate_Entropy_Per_Byte return Float with
      Post => Estimate_Entropy_Per_Byte'Result >= 7.0 and
              Estimate_Entropy_Per_Byte'Result <= 8.0;

private

   -- Platform-specific entropy source selection
   type Entropy_Source_Type is (
      Hardware_RNG,        -- CPU instruction (RDRAND/RDSEED)
      System_API,          -- SecRandomCopyBytes (macOS)
      System_Syscall,      -- getrandom() (Linux)
      Dev_Urandom,         -- /dev/urandom (fallback)
      LibOQS_RNG           -- liboqs ChaCha20-DRBG (last resort)
   );

   -- Detect best available entropy source at initialization
   function Detect_Entropy_Source return Entropy_Source_Type;

end Anubis_Entropy;
