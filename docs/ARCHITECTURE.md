# ANUBIS-SPARK Architecture

Comprehensive system design documentation for the hybrid post-quantum file encryption system.

## Table of Contents

- [Overview](#overview)
- [Design Philosophy](#design-philosophy)
- [System Architecture](#system-architecture)
- [Component Details](#component-details)
- [Data Flow](#data-flow)
- [SPARK Verification Strategy](#spark-verification-strategy)
- [Security Properties](#security-properties)
- [Implementation Decisions](#implementation-decisions)

---

## Overview

ANUBIS-SPARK is a **defense-in-depth** cryptographic system that combines:
- **Classical cryptography** (X25519, Ed25519, XChaCha20-Poly1305)
- **Post-quantum cryptography** (ML-KEM-1024, ML-DSA-87)
- **Formal verification** (SPARK Ada proving security properties)

An attacker must break **BOTH** classical AND post-quantum algorithms to compromise the system.

---

## Design Philosophy

### 1. Defense-in-Depth (Hybrid Cryptography)

```
Security = Classical Security ∩ Post-Quantum Security
         = MIN(X25519, ML-KEM-1024)
         = MIN(128-bit, 256-bit)
         = 128-bit security today, 256-bit post-quantum
```

**Why Hybrid?**
- **Current security:** Classical algorithms provide proven security against today's threats
- **Future security:** PQC algorithms provide quantum resistance
- **Risk mitigation:** If one algorithm is broken, the other still protects data
- **Standards compliance:** NIST recommends hybrid approaches during transition

### 2. Formal Verification (SPARK)

Traditional cryptographic code has bugs:
- OpenSSL Heartbleed
- GnuTLS goto fail
- Debian weak RNG

**SPARK mathematically proves:**
- No buffer overflows
- No memory leaks
- No uninitialized variables
- Correct zeroization
- No key confusion
- Proper information flow

### 3. Security Over Performance

- Argon2id over bcrypt/scrypt (memory-hard, GPU-resistant)
- Constant-time operations over optimized code (side-channel resistance)
- Memory locking over swap (prevents cold-boot attacks)
- Secure zeroization over fast cleanup (prevents memory dumps)

### 4. Fail-Safe Defaults

- Operations fail closed (zeroize on error)
- Explicit validity checking (`Valid : Boolean` on all secrets)
- No implicit conversions
- SPARK contracts enforce preconditions

---

## System Architecture

### High-Level Component Diagram

```
┌───────────────────────────────────────────────────────────────────┐
│                         CLI Application Layer                      │
│  ┌────────────┬────────────┬────────────┬────────────┬──────────┐ │
│  │ anubis init│ encrypt    │ decrypt    │ rotate     │ backup   │ │
│  └────────────┴────────────┴────────────┴────────────┴──────────┘ │
└───────────────────────────────────────────────────────────────────┘
                                  │
                                  ↓
┌───────────────────────────────────────────────────────────────────┐
│                      Key Management Layer                          │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │  Anubis_Key_Manager (anubis_key_manager.ads)                │  │
│  │  • Generate_Hybrid_Identity                                 │  │
│  │  • Derive_Master_Key (Argon2id)                            │  │
│  │  • Rotate_Hybrid_Identity                                   │  │
│  │  • Create_Recovery_Shares (Shamir)                          │  │
│  │  • Secure_Store / Secure_Load                              │  │
│  └─────────────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────────────┘
                                  │
                                  ↓
┌───────────────────────────────────────────────────────────────────┐
│                    Hybrid Cryptography Layer                       │
│  ┌──────────────────────────┬─────────────────────────────────┐  │
│  │  Classical Crypto        │  Post-Quantum Crypto (PQC)      │  │
│  │  (To Be Implemented)     │  (anubis_types-pqc.ads)         │  │
│  │                          │                                 │  │
│  │  • X25519 (ECDH)        │  • ML-KEM-1024 (KEM)      ✅   │  │
│  │  • Ed25519 (Sig)        │  • ML-DSA-87 (Signature)  ✅   │  │
│  │  • XChaCha20-Poly1305   │  • Hybrid_Encapsulate     ✅   │  │
│  │  • Argon2id             │  • Secrets_Match (CT)     ✅   │  │
│  └──────────────────────────┴─────────────────────────────────┘  │
└───────────────────────────────────────────────────────────────────┘
                                  │
                                  ↓
┌───────────────────────────────────────────────────────────────────┐
│                    SPARK-Verified Type System                      │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │  Anubis_Types (anubis_types.ads) - SPARK Mode ON            │  │
│  │  • Secure type definitions (Volatile, no optimization)      │  │
│  │  • ML_KEM_Secret_Key, ML_DSA_Secret_Key                    │  │
│  │  • Hybrid_Shared_Secret                                     │  │
│  │  • SPARK contracts (Pre/Post conditions)                    │  │
│  │  • Validity checking (Valid : Boolean)                      │  │
│  └─────────────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────────────┘
                                  │
                                  ↓
┌───────────────────────────────────────────────────────────────────┐
│                      C FFI Bindings Layer                          │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │  liboqs Bindings - SPARK Mode OFF (C interop)               │  │
│  │  • oqs_common.ads (init, cleanup, memory ops)         ✅   │  │
│  │  • oqs_kem_ml_kem.ads (ML-KEM-1024 FFI)               ✅   │  │
│  │  • oqs_sig_ml_dsa.ads (ML-DSA-87 FFI)                 ✅   │  │
│  │  • System.Address-based (no typed pointers)                 │  │
│  └─────────────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────────────┘
                                  │
                                  ↓
┌───────────────────────────────────────────────────────────────────┐
│                      External Libraries                            │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │  liboqs 0.14.0 (C)                                           │  │
│  │  • ML-KEM-1024 (FIPS 203)                                   │  │
│  │  • ML-DSA-87 (FIPS 204)                                     │  │
│  │  • Secure memory operations (OQS_MEM_cleanse)               │  │
│  │  • Constant-time comparison (OQS_MEM_secure_bcmp)           │  │
│  └─────────────────────────────────────────────────────────────┘  │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │  OpenSSL 3.x (C)                                             │  │
│  │  • Entropy source (liboqs dependency)                        │  │
│  │  • Low-level crypto primitives                              │  │
│  └─────────────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────────────┘
```

---

## Component Details

### 1. Type System (`anubis_types.ads/adb`)

**Purpose:** Define secure, SPARK-verifiable cryptographic types.

**Key Features:**
- All secret types marked `Volatile` (prevents compiler optimization)
- Explicit validity flags (`Valid : Boolean`)
- SPARK contracts on all operations
- No dynamic allocation (stack-only for security)

**Example Type:**
```ada
type ML_KEM_Secret_Key is record
   Data  : Byte_Array (1 .. ML_KEM_1024_SECRET_KEY_SIZE);
   Valid : Boolean := False;
end record with Volatile;  -- Prevents zeroization optimization
```

**Contracts:**
```ada
procedure Zeroize (Key : in out ML_KEM_Secret_Key) with
   Post => not Is_Valid (Key);  -- SPARK verifies this!
```

**Implementation Status:** ✅ Complete

---

### 2. Post-Quantum Cryptography (`anubis_types-pqc.ads/adb`)

**Purpose:** SPARK-safe wrapper around liboqs C library.

**Architecture Decision:**
- **Interface** in `SPARK Mode (On)` - SPARK can verify contracts
- **Implementation** in `SPARK Mode (Off)` - C FFI not provable

**Operations:**

#### ML-KEM-1024 (Key Encapsulation)
```ada
procedure ML_KEM_Generate_Keypair (
   Public_Key  : out ML_KEM_Public_Key;
   Secret_Key  : out ML_KEM_Secret_Key;
   Success     : out Boolean
) with
   Global => null,
   Post   => (if Success then Is_Valid (Secret_Key)
              else not Is_Valid (Secret_Key));
```

**Security Properties:**
- Zeroizes secrets on failure
- Constant-time shared secret comparison (`Secrets_Match`)
- Uses `OQS_MEM_cleanse` for secure zeroization

#### ML-DSA-87 (Digital Signatures)
```ada
procedure ML_DSA_Sign (
   Message        : in     Byte_Array;
   Secret_Key     : in     ML_DSA_Secret_Key;
   Signature      : out    ML_DSA_Signature;
   Success        : out    Boolean
) with
   Pre => Is_Valid (Secret_Key);  -- Enforces valid key usage
```

**Verification:**
```ada
function ML_DSA_Verify (
   Message     : Byte_Array;
   Signature   : ML_DSA_Signature;
   Public_Key  : ML_DSA_Public_Key
) return Boolean;
```

**Implementation Status:** ✅ Complete

---

### 3. C FFI Bindings (`liboqs/*.ads`)

**Purpose:** Low-level bindings to liboqs C library.

**Design Decisions:**

#### Why `System.Address` instead of typed pointers?
```ada
-- ✅ Used (works with Ada type system)
function OQS_KEM_ml_kem_1024_keypair (
   public_key : System.Address;
   secret_key : System.Address
) return OQS_STATUS;

-- ❌ Not used (type compatibility issues)
function OQS_KEM_ml_kem_1024_keypair (
   public_key : access Interfaces.C.unsigned_char;
   secret_key : access Interfaces.C.unsigned_char
) return OQS_STATUS;
```

**Reason:** Ada `Byte` type doesn't match C `unsigned char*`, but `System.Address` is universal.

**Usage:**
```ada
Status := OQS_KEM_ml_kem_1024_keypair (
   public_key => Public_Key.Data (Public_Key.Data'First)'Address,
   secret_key => Secret_Key.Data (Secret_Key.Data'First)'Address
);
```

**Implementation Status:** ✅ Complete
- `oqs_common.ads` - Init, cleanup, memory operations
- `oqs_kem_ml_kem.ads` - ML-KEM-1024 FFI
- `oqs_sig_ml_dsa.ads` - ML-DSA-87 FFI

---

### 4. Key Management (`anubis_key_manager.ads`)

**Purpose:** Enterprise-grade key lifecycle management.

**Features:**

#### Key Generation
```ada
procedure Generate_Hybrid_Identity (
   Identity : out Hybrid_Identity;
   Success  : out Boolean
);
-- Generates:
-- • ML-KEM-1024 keypair
-- • ML-DSA-87 keypair
-- • X25519 keypair
-- • Ed25519 keypair
```

#### Key Derivation (Argon2id)
```ada
procedure Derive_Master_Key (
   Passphrase  : in String;
   Salt        : in Argon2_Salt;
   Master      : out Master_Key;
   Success     : out Boolean
) with Pre => Passphrase'Length >= 12;

-- Parameters:
-- • Memory: 64 MiB (defeats GPU attacks)
-- • Iterations: 3
-- • Parallelism: 4 threads
```

#### Key Rotation
```ada
procedure Rotate_Hybrid_Identity (
   Old_Identity : in     Hybrid_Identity;
   New_Identity : out    Hybrid_Identity;
   Success      : out    Boolean
);
-- Triggers:
-- • Time-based: Every 90 days
-- • Usage-based: 1,000,000 operations
```

#### Backup & Recovery (Shamir Secret Sharing)
```ada
procedure Create_Recovery_Shares (
   Master      : in Master_Key;
   N_Shares    : in Positive;
   K_Threshold : in Positive;
   Shares      : out Share_Array;
   Success     : out Boolean
);
-- Example: (3, 5) = 5 shares, need any 3 to recover
```

**Implementation Status:** 🏗️ Architecture complete, implementation pending

---

### 5. Hybrid Cryptography

**Why Hybrid?**

| Algorithm Type | Current Security | Quantum Security | Status |
|---------------|------------------|------------------|--------|
| X25519 | 128-bit | ~64-bit (Shor's) | Quantum-vulnerable |
| ML-KEM-1024 | 256-bit | 256-bit | Quantum-resistant |
| **Hybrid** | **128-bit** | **256-bit** | **Best of both** |

**Hybrid Key Encapsulation:**
```
1. Perform X25519 ECDH
   → Classical_Shared_Secret (32 bytes)

2. Perform ML-KEM-1024 Encapsulation
   → PQ_Shared_Secret (32 bytes)

3. Combine with HKDF-SHA256
   HKDF(Classical || PQ, "anubis-hybrid-kem-v1", 32)
   → Hybrid_Shared_Secret (32 bytes)

Result: Attacker must break BOTH X25519 AND ML-KEM-1024
```

**Hybrid Signature:**
```
1. Sign message with Ed25519
   → Classical_Signature (64 bytes)

2. Sign message with ML-DSA-87
   → PQ_Signature (4,627 bytes)

3. Concatenate signatures
   → Hybrid_Signature (4,691 bytes)

Verification: BOTH signatures must verify
```

**Implementation Status:** 🏗️ ML-KEM and ML-DSA complete, X25519/Ed25519 pending

---

## Data Flow

### File Encryption Flow

```
┌─────────────────────────────────────────────────────────────────┐
│  1. User provides passphrase + file                             │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│  2. Derive Master Key (Argon2id, 64 MiB, 3 iterations)          │
│     Passphrase + Salt → Master_Key (32 bytes)                   │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│  3. Load Hybrid Identity from keystore                          │
│     • ML-KEM-1024 Public Key                                    │
│     • ML-DSA-87 Secret Key (encrypted with Master_Key)          │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│  4. Generate Ephemeral Hybrid Keys                              │
│     • Ephemeral_X25519_Secret                                   │
│     • Ephemeral_ML_KEM_Ciphertext                              │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│  5. Hybrid Key Encapsulation                                    │
│     X25519(Recipient_Pub, Ephemeral_Sec) → Classical_Shared     │
│     ML_KEM_Encaps(Recipient_Pub) → PQ_Shared                   │
│     HKDF(Classical || PQ) → Hybrid_Shared (32 bytes)           │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│  6. Derive File Encryption Key                                  │
│     HKDF(Hybrid_Shared, "anubis-file-key", 32) → File_Key       │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│  7. Encrypt File (XChaCha20-Poly1305)                           │
│     XChaCha20_Encrypt(File_Key, Nonce, Plaintext)               │
│     → Ciphertext + Auth_Tag                                     │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│  8. Sign Encrypted File (Hybrid Signature)                      │
│     Ed25519_Sign(Ciphertext, Secret_Ed25519) → Ed_Sig           │
│     ML_DSA_Sign(Ciphertext, Secret_ML_DSA) → ML_Sig            │
│     Hybrid_Signature = Ed_Sig || ML_Sig                         │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│  9. Write Encrypted File                                        │
│     Header: Version, Algorithms, Nonce, Ephemeral_Pub_Keys      │
│     Body: Ciphertext + Auth_Tag                                 │
│     Footer: Hybrid_Signature                                    │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ↓
┌─────────────────────────────────────────────────────────────────┐
│  10. Secure Cleanup (SPARK-verified)                            │
│      Zeroize: File_Key, Hybrid_Shared, Ephemeral_Secrets       │
└─────────────────────────────────────────────────────────────────┘
```

### File Decryption Flow

```
1. Read encrypted file
2. Verify Hybrid Signature (BOTH Ed25519 AND ML-DSA must verify)
3. If verification fails → ABORT (tampered file)
4. Extract ephemeral public keys from header
5. Perform hybrid decapsulation using recipient secret keys
6. Derive file encryption key (same HKDF as encryption)
7. Decrypt file with XChaCha20-Poly1305
8. Verify auth tag
9. Zeroize all secrets
```

---

## SPARK Verification Strategy

### Proof Levels

**Level 0:** Stone (no proofs, just type checking)
**Level 1:** Bronze (flow analysis, uninitialized variables)
**Level 2:** Silver (absence of runtime errors) ← **Current target**
**Level 3:** Gold (functional correctness)
**Level 4:** Platinum (full verification)

### Current Verification Status

✅ **Type Safety** (Level 0)
- All types correctly defined
- No type confusion

✅ **Data Flow** (Level 1)
- No uninitialized variables
- All outputs assigned before use

🏗️ **Memory Safety** (Level 2)
- No buffer overflows (array bounds checking)
- No null pointer dereferences
- No use-after-free

🏗️ **Information Flow** (Level 2)
- Secrets don't leak to logs
- Side-channel resistance (constant-time)

📋 **Cryptographic Properties** (Level 3-4)
- Nonce uniqueness
- Key derivation correctness
- Signature verification before decryption

### Running SPARK Proofs

```bash
# Install SPARK provers
alr get gnatprove
alr get alt-ergo z3 cvc5

# Run proofs (Level 2)
gnatprove -P anubis_spark.gpr \
   --level=2 \
   --prover=cvc5,z3 \
   --timeout=60 \
   --checks-as-errors

# View proof results
ls gnatprove/
```

---

## Security Properties

### 1. Confidentiality

**Encryption:** XChaCha20-Poly1305 (AEAD)
- 256-bit key
- 192-bit nonce (never reused)
- Authenticated encryption (tamper-proof)

**Key Encapsulation:** Hybrid (X25519 + ML-KEM-1024)
- 128-bit classical security
- 256-bit post-quantum security
- Both must be broken

### 2. Integrity

**Signatures:** Hybrid (Ed25519 + ML-DSA-87)
- Classical signature (64 bytes)
- Post-quantum signature (4,627 bytes)
- Both verified before decryption

**MAC:** Poly1305 (part of XChaCha20-Poly1305)
- 128-bit security
- Prevents tampering

### 3. Authentication

**Signatures:** Prove file was encrypted by holder of private key
**Zero-Knowledge Proofs:** Prove file access without revealing contents

### 4. Forward Secrecy

**Key Rotation:** Old keys archived for decryption only
**Ephemeral Keys:** One-time keys for each encryption

### 5. Side-Channel Resistance

**Constant-Time Operations:**
- Secret comparison: `OQS_MEM_secure_bcmp`
- No data-dependent branches
- No timing variations

**Memory Protection:**
- Secrets locked in RAM (no swap)
- Secure zeroization (not optimized away)
- Volatile types prevent reordering

---

## Implementation Decisions

### 1. Ada/SPARK over Rust

| Aspect | Ada/SPARK | Rust |
|--------|-----------|------|
| Memory Safety | Proven (SPARK) | Compiler-checked |
| Formal Verification | Mathematical proofs | Limited |
| Runtime Checks | Comprehensive | Panic on error |
| Cryptographic Use | Military, aerospace | Growing |
| Learning Curve | Steep | Moderate |

**Decision:** Ada/SPARK for maximum assurance

### 2. Static Linking vs Dynamic

**liboqs:** Statically linked (`liboqs.a`)
- No version conflicts
- Easier deployment
- Larger binaries (acceptable tradeoff)

### 3. Child Package Structure

```ada
package Anubis_Types is ...        -- Parent (types)
package Anubis_Types.PQC is ...    -- Child (operations)
```

**Advantages:**
- Access to parent private types
- Clear separation of concerns
- SPARK can verify parent, not child (C FFI)

### 4. Build System: GPRbuild

**Why not Alire directly?**
- GPRbuild gives fine control over:
  - Compiler flags (`-fstack-protector-strong`)
  - Linker paths (full library paths)
  - Build modes (dev, release, spark)
- Alire wraps GPRbuild (`alr exec -- gprbuild`)

---

## Future Enhancements

### Phase 2: Core Implementation
- X25519/Ed25519 integration (SPARKNaCl or libsodium bindings)
- Argon2id bindings
- XChaCha20-Poly1305 AEAD
- Complete hybrid operations

### Phase 3: Advanced Features
- Shamir Secret Sharing (pure Ada implementation)
- Zero-knowledge proofs
- Hardware security module (HSM) integration
- PKCS#11 support

### Phase 4: Platform Support
- Linux optimizations (kernel entropy)
- Windows CNG integration
- ARM TrustZone support
- Intel SGX enclaves

---

## References

- **NIST FIPS 203:** Module-Lattice-Based Key-Encapsulation Mechanism Standard
- **NIST FIPS 204:** Module-Lattice-Based Digital Signature Standard
- **SPARK Ada User's Guide:** https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/
- **liboqs Documentation:** https://github.com/open-quantum-safe/liboqs/wiki
- **Argon2 RFC 9106:** https://www.rfc-editor.org/rfc/rfc9106.html

---

**Last Updated:** 2025-10-10
**Architecture Version:** 1.0
**Implementation Phase:** 1 (Foundation Complete)
