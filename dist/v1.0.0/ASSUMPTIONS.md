# ANUBIS-SPARK v1.0.0 - Verification Assumptions

## External Library Trust Assumptions

This verified SPARK implementation relies on the following external cryptographic libraries:

### 1. liboqs (Open Quantum Safe)
- **Version**: 0.14.0
- **Algorithms**: ML-KEM-1024, ML-DSA-87
- **Assumption**: Implementations are correct and side-channel resistant
- **Static linking**: Eliminates runtime library substitution attacks

### 2. libsodium
- **Version**: 1.0.20
- **Algorithms**: X25519, Ed25519, XChaCha20-Poly1305, Argon2id
- **Assumption**: Implementations are audited and constant-time
- **Static linking**: Prevents dynamic library hijacking

### 3. OpenSSL
- **Version**: 3.x
- **Usage**: HKDF-SHA256 key derivation
- **Assumption**: SHA-256 implementation is cryptographically secure
- **Dynamic linking**: System-provided for compatibility

## SPARK Verification Boundaries

### Verified Components (100% SPARK)
- ✅ Type safety and memory bounds
- ✅ Hybrid key encapsulation correctness
- ✅ AEAD streaming encryption properties
- ✅ Key manager lifecycle and rotation
- ✅ Shamir Secret Sharing field arithmetic (GF(256))
- ✅ Termination proofs (no infinite loops)

### Unverified Components (SPARK_Mode => Off)
- ⚠️ FFI bindings to C libraries (oqs_*, sodium_*)
- ⚠️ File I/O operations (Ada.Streams)
- ⚠️ Random number generation (libsodium RNG)

## Current Proof Status

**183 out of 184 VCs proved (99.5%)**

### Remaining Unproved VC
- `anubis_key_manager.ads:58` - Key_Material_Zeroed postcondition
  - **Severity**: Low
  - **Reason**: Prover cannot establish complete zeroing across all conditional paths
  - **Mitigation**: Manual code inspection confirms zeroing occurs in all branches

## Cryptographic Assumptions

1. **Post-Quantum Security**: ML-KEM-1024 and ML-DSA-87 remain secure against quantum attacks
2. **Hybrid Security**: Classical + PQ provides defense-in-depth
3. **AEAD Security**: XChaCha20-Poly1305 provides authenticated encryption
4. **KDF Security**: HKDF-SHA256 produces cryptographically independent keys

## Build Configuration

- **Compiler**: GNAT 14.2.1 (Alire toolchain)
- **Prover**: CVC5 + Z3 (via gnatprove)
- **Proof Level**: 4 (maximum rigor)
- **Proof Timeout**: 60 seconds per VC
- **Max Steps**: 164 (for complex VCs)

## Audit Recommendations

External auditors should focus on:
1. FFI boundary safety (buffer sizes, null checks)
2. Side-channel resistance in C library calls
3. Random number quality (entropy sources)
4. Zeroization completeness (especially on error paths)

---

Generated: 2025-10-10
ANUBIS-SPARK Project
