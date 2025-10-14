-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Named Constants
-- Replaces magic numbers with descriptive named constants
-- Improves code maintainability and reduces errors
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package Anubis_Constants is

   -------------------------------------------------------------------------
   -- Cryptographic Sizes
   -------------------------------------------------------------------------

   -- Nonce sizes
   XCHACHA20_NONCE_SIZE : constant := 24;  -- XChaCha20-Poly1305 nonce
   FILE_NONCE_SIZE      : constant := 16;  -- File-level nonce component
   CHUNK_INDEX_SIZE     : constant := 8;   -- Chunk index component (big-endian)

   -- Authentication
   AUTH_TAG_SIZE : constant := 16;  -- Poly1305 authentication tag

   -- Key sizes (all 256-bit)
   X25519_KEY_SIZE      : constant := 32;  -- Curve25519 keys
   ED25519_KEY_SIZE     : constant := 32;  -- Ed25519 keys
   XCHACHA20_KEY_SIZE   : constant := 32;  -- XChaCha20 symmetric key
   ML_KEM_SHARED_SIZE   : constant := 32;  -- ML-KEM shared secret
   X25519_SHARED_SIZE   : constant := 32;  -- X25519 shared secret

   -- Post-Quantum key sizes
   ML_KEM_PUBLIC_KEY_SIZE  : constant := 1568;  -- ML-KEM-1024 public key
   ML_KEM_SECRET_KEY_SIZE  : constant := 3168;  -- ML-KEM-1024 secret key
   ML_KEM_CIPHERTEXT_SIZE  : constant := 1568;  -- ML-KEM-1024 ciphertext

   ML_DSA_PUBLIC_KEY_SIZE  : constant := 2592;  -- ML-DSA-87 public key
   ML_DSA_SECRET_KEY_SIZE  : constant := 4864;  -- ML-DSA-87 secret key
   ML_DSA_SIGNATURE_SIZE   : constant := 4627;  -- ML-DSA-87 signature

   -- Ed25519 signature size
   ED25519_SIGNATURE_SIZE : constant := 64;

   -- Argon2id
   ARGON2_SALT_SIZE : constant := 16;  -- Argon2id salt
   ARGON2_KEY_SIZE  : constant := 32;  -- Derived key output

   -------------------------------------------------------------------------
   -- File Format
   -------------------------------------------------------------------------

   -- Magic and version
   MAGIC_SIZE   : constant := 6;   -- "ANUB3\n"
   VERSION_SIZE : constant := 1;   -- Format version byte

   -- Header structure sizes
   HEADER_BASE_SIZE : constant := 7;  -- Magic (6) + Version (1)

   -------------------------------------------------------------------------
   -- Streaming Encryption Parameters
   -------------------------------------------------------------------------

   -- Chunk sizes for streaming encryption/decryption
   MIN_CHUNK_SIZE     : constant := 4 * 1024;           -- 4 KiB minimum
   DEFAULT_CHUNK_SIZE : constant := 64 * 1024 * 1024;   -- 64 MiB default
   MAX_CHUNK_SIZE     : constant := 1024 * 1024 * 1024; -- 1 GiB maximum

   -- Chunk metadata
   CHUNK_LENGTH_SIZE : constant := 8;  -- 64-bit chunk length (big-endian)

   -------------------------------------------------------------------------
   -- HKDF Limits
   -------------------------------------------------------------------------

   -- HKDF-SHA256 maximum output length (255 * HashLen)
   -- RFC 5869: OKM length <= 255 * HashLen (for SHA256, HashLen = 32)
   HKDF_MAX_OUTPUT_LENGTH : constant := 255 * 32;  -- 8160 bytes

   -------------------------------------------------------------------------
   -- Argon2id Parameters
   -------------------------------------------------------------------------

   -- Memory cost: 256 MiB (MODERATE)
   ARGON2_MEMORY_COST : constant := 256 * 1024;  -- KiB

   -- Time cost: 3 iterations (MODERATE)
   ARGON2_TIME_COST : constant := 3;

   -- Parallelism: 1 thread (sequential)
   ARGON2_PARALLELISM : constant := 1;

   -------------------------------------------------------------------------
   -- Entropy Validation Thresholds
   -------------------------------------------------------------------------

   -- Minimum Hamming weight: 25% of bits set
   -- For a 32-byte key, this means at least 64 bits set
   MIN_HAMMING_WEIGHT_RATIO : constant := 0.25;

   -- Minimum unique byte values in random data
   MIN_UNIQUE_BYTES : constant := 8;

   -- Minimum data length for entropy validation
   MIN_ENTROPY_CHECK_LENGTH : constant := 16;

   -------------------------------------------------------------------------
   -- Security Levels
   -------------------------------------------------------------------------

   -- All algorithms provide 256-bit security (NIST Level 5+)
   SECURITY_LEVEL_BITS : constant := 256;

   -------------------------------------------------------------------------
   -- Context Strings (for HKDF domain separation)
   -------------------------------------------------------------------------

   -- These are used in HKDF to derive different keys from the same secret
   CONTEXT_HYBRID_KEM   : constant String := "anubis-hybrid-kem-v1";
   CONTEXT_XCHACHA_KEY  : constant String := "anubis-xchacha20-key-v1";
   CONTEXT_FILE_KEY     : constant String := "anubis-file-key-v1";

   -------------------------------------------------------------------------
   -- File Format Version
   -------------------------------------------------------------------------

   CURRENT_FILE_VERSION : constant := 3;  -- ANUB3 format

end Anubis_Constants;
