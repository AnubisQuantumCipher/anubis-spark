-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Post-Quantum Cryptography Wrapper
-- SPARK-safe interface to ML-KEM-1024 and ML-DSA-87
-- Provides type safety, automatic zeroization, and formal contracts
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package Anubis_Types.PQC is

   -------------------------------------------------------------------------
   -- ML-KEM-1024: Key Encapsulation Mechanism (NIST Level 5)
   -------------------------------------------------------------------------

   -- Generate ML-KEM-1024 keypair
   procedure ML_KEM_Generate_Keypair (
      Public_Key  : out ML_KEM_Public_Key;
      Secret_Key  : out ML_KEM_Secret_Key;
      Success     : out Boolean
   ) with
      Global => null,  -- No global state
      Post   => (if Success then
                    Is_Valid (Secret_Key)
                 else
                    not Is_Valid (Secret_Key));

   -- Encapsulate: Generate shared secret and ciphertext for recipient
   procedure ML_KEM_Encapsulate (
      Recipient_Public_Key : in     ML_KEM_Public_Key;
      Ciphertext           : out    ML_KEM_Ciphertext;
      Shared_Secret        : out    ML_KEM_Shared_Secret;
      Success              : out    Boolean
   ) with
      Global => null,
      Post   => (if Success then Is_Valid (Shared_Secret));

   -- Decapsulate: Recover shared secret from ciphertext
   procedure ML_KEM_Decapsulate (
      Ciphertext    : in     ML_KEM_Ciphertext;
      Secret_Key    : in     ML_KEM_Secret_Key;
      Shared_Secret : out    ML_KEM_Shared_Secret;
      Success       : out    Boolean
   ) with
      Pre  => Is_Valid (Secret_Key),
      Post => (if Success then Is_Valid (Shared_Secret));

   -------------------------------------------------------------------------
   -- ML-DSA-87: Digital Signature Algorithm (NIST Level 5)
   -------------------------------------------------------------------------

   -- Generate ML-DSA-87 keypair
   procedure ML_DSA_Generate_Keypair (
      Public_Key  : out ML_DSA_Public_Key;
      Secret_Key  : out ML_DSA_Secret_Key;
      Success     : out Boolean
   ) with
      Global => null,
      Post   => (if Success then
                    Is_Valid (Secret_Key)
                 else
                    not Is_Valid (Secret_Key));

   -- Sign a message
   procedure ML_DSA_Sign (
      Message        : in     Byte_Array;
      Secret_Key     : in     ML_DSA_Secret_Key;
      Signature      : out    ML_DSA_Signature;
      Success        : out    Boolean
   ) with
      Pre  => Is_Valid (Secret_Key) and Message'Length > 0,
      Global => null;

   -- Verify a signature
   function ML_DSA_Verify (
      Message     : Byte_Array;
      Signature   : ML_DSA_Signature;
      Public_Key  : ML_DSA_Public_Key
   ) return Boolean with
      Pre => Message'Length > 0,
      Global => null;

   -------------------------------------------------------------------------
   -- Secure Zeroization (using liboqs secure cleanse)
   -------------------------------------------------------------------------

   -- Compare two shared secrets (constant-time)
   function Secrets_Match (
      Secret_A : ML_KEM_Shared_Secret;
      Secret_B : ML_KEM_Shared_Secret
   ) return Boolean;

   -- Securely zeroize ML-KEM shared secret
   procedure Zeroize_Shared_Secret (
      Secret : in out ML_KEM_Shared_Secret
   ) with
      Post => not Is_Valid (Secret);

   -- Securely zeroize ML-KEM secret key
   procedure Zeroize_ML_KEM_Secret (
      Secret_Key : in out ML_KEM_Secret_Key
   ) with
      Post => not Is_Valid (Secret_Key);

   -- Securely zeroize ML-DSA secret key
   procedure Zeroize_ML_DSA_Secret (
      Secret_Key : in out ML_DSA_Secret_Key
   ) with
      Post => not Is_Valid (Secret_Key);

   -------------------------------------------------------------------------
   -- Hybrid Operations (combines classical + post-quantum)
   -------------------------------------------------------------------------

   -- Hybrid key encapsulation (X25519 + ML-KEM-1024)
   -- Both must be broken to compromise the shared secret
   type Hybrid_Shared_Secret is private;

   function Is_Valid (Secret : Hybrid_Shared_Secret) return Boolean;

   procedure Hybrid_Encapsulate (
      X25519_Public  : in     X25519_Public_Key;
      ML_KEM_Public  : in     ML_KEM_Public_Key;
      X25519_Ephemeral_Secret : out X25519_Secret_Key;
      Ciphertext     : out ML_KEM_Ciphertext;
      Hybrid_Secret  : out    Hybrid_Shared_Secret;
      Success        : out    Boolean
   ) with
      Post => (if Success then Is_Valid (Hybrid_Secret));

   -- Derive final encryption key from hybrid shared secret
   procedure Derive_Encryption_Key (
      Hybrid_Secret : in     Hybrid_Shared_Secret;
      Encryption_Key : out    XChaCha20_Key;
      Success        : out    Boolean
   ) with
      Pre  => Is_Valid (Hybrid_Secret),
      Post => (if Success then Is_Valid (Encryption_Key));

   -------------------------------------------------------------------------
   -- Hybrid Signatures (Ed25519 + ML-DSA-87)
   -- PLATINUM LEVEL: Dual signatures provide quantum + classical security
   -------------------------------------------------------------------------

   -- Hybrid signature combines both classical and PQ signatures
   type Hybrid_Signature is private;

   -------------------------------------------------------------------------
   -- PLATINUM LEVEL: Ghost Functions for Hybrid Signatures
   -------------------------------------------------------------------------

   -- Ghost: Check if hybrid signature is properly zeroized
   function Hybrid_Signature_Zeroed (Sig : Hybrid_Signature) return Boolean with
      Ghost;

   -- Ghost: Check if both signatures in hybrid are valid
   function Both_Signatures_Present (Sig : Hybrid_Signature) return Boolean with
      Ghost;

   -- Generate hybrid signature (sign with BOTH Ed25519 AND ML-DSA-87)
   -- Both signatures must verify for the hybrid signature to be valid
   -- PLATINUM LEVEL: Dual signatures zeroized on failure (proven in body)
   procedure Hybrid_Sign (
      Message     : in     Byte_Array;
      Ed25519_SK  : in     Ed25519_Secret_Key;
      ML_DSA_SK   : in     ML_DSA_Secret_Key;
      Signature   : out    Hybrid_Signature;
      Success     : out    Boolean
   ) with
      Pre    => Is_Valid (Ed25519_SK) and
                Is_Valid (ML_DSA_SK) and
                Message'Length > 0,
      Global => null,
      Post   => (if not Success then
                    Hybrid_Signature_Zeroed (Signature));

   -- Verify hybrid signature (BOTH Ed25519 AND ML-DSA-87 must verify)
   function Hybrid_Verify (
      Message     : Byte_Array;
      Signature   : Hybrid_Signature;
      Ed25519_PK  : Ed25519_Public_Key;
      ML_DSA_PK   : ML_DSA_Public_Key
   ) return Boolean with
      Pre    => Message'Length > 0,
      Global => null;

private

   -- Hybrid shared secret combines both classical and PQ secrets
   -- PLATINUM SPARK: Formal verification ensures security properties
   type Hybrid_Shared_Secret is record
      Classical_Secret : Byte_Array (1 .. 32);  -- X25519 shared secret
      PQ_Secret       : Byte_Array (1 .. 32);  -- ML-KEM shared secret
      Valid           : Boolean := False;
   end record;

   -- Hybrid signature combines both classical and PQ signatures
   -- PLATINUM SPARK: Both signatures must verify for overall validity
   type Hybrid_Signature is record
      Ed25519_Sig : Ed25519_Signature;  -- Classical signature (64 bytes)
      ML_DSA_Sig  : ML_DSA_Signature;   -- Post-quantum signature (4627 bytes)
   end record;

   -------------------------------------------------------------------------
   -- PLATINUM: Ghost Function Implementations
   -------------------------------------------------------------------------

   -- Check if hybrid signature is properly zeroized
   function Hybrid_Signature_Zeroed (Sig : Hybrid_Signature) return Boolean is
      ((for all I in Sig.Ed25519_Sig.Data'Range => Sig.Ed25519_Sig.Data (I) = 0) and
       (for all I in Sig.ML_DSA_Sig.Data'Range => Sig.ML_DSA_Sig.Data (I) = 0));

   -- Check if both signatures in hybrid are valid (non-zero)
   function Both_Signatures_Present (Sig : Hybrid_Signature) return Boolean is
      ((for some I in Sig.Ed25519_Sig.Data'Range => Sig.Ed25519_Sig.Data (I) /= 0) and
       (for some I in Sig.ML_DSA_Sig.Data'Range => Sig.ML_DSA_Sig.Data (I) /= 0));

end Anubis_Types.PQC;
