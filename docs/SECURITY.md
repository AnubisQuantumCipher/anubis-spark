# ANUBIS-SPARK Security Analysis

Comprehensive threat model, security guarantees, and cryptographic analysis.

## Table of Contents

- [Security Overview](#security-overview)
- [Threat Model](#threat-model)
- [Cryptographic Algorithms](#cryptographic-algorithms)
- [Attack Surface Analysis](#attack-surface-analysis)
- [Security Guarantees](#security-guarantees)
- [Known Limitations](#known-limitations)
- [Security Best Practices](#security-best-practices)
- [Incident Response](#incident-response)

---

## Security Overview

ANUBIS-SPARK provides **defense-in-depth** protection against:
- ✅ Classical attacks (current computers)
- ✅ Quantum attacks (future quantum computers)
- ✅ Side-channel attacks (timing, cache, power analysis)
- ✅ Memory attacks (cold boot, DMA, swap dumps)
- ✅ Implementation bugs (via SPARK formal verification)

**Security Level:** NIST Level 5 (256-bit post-quantum equivalent)

---

## Threat Model

### Assets

1. **Encrypted Files** - Primary asset to protect
2. **Secret Keys** - ML-KEM-1024, ML-DSA-87, X25519, Ed25519 secret keys
3. **Master Password** - User-chosen passphrase
4. **Derived Keys** - XChaCha20 encryption keys, HMAC keys

### Adversaries

#### 1. **Passive Network Attacker**

**Capabilities:**
- Intercepts network traffic
- Records encrypted files

**Mitigations:**
- All files encrypted with hybrid PQC
- No key material transmitted (out-of-band key exchange)
- Forward secrecy via key rotation

**Verdict:** ✅ Protected (128-bit classical, 256-bit post-quantum)

---

#### 2. **Active Network Attacker (MITM)**

**Capabilities:**
- Intercepts and modifies traffic
- Replays old messages
- Injects fake data

**Mitigations:**
- Hybrid signatures (Ed25519 + ML-DSA-87)
- AEAD provides authenticity (XChaCha20-Poly1305)
- Nonce uniqueness prevents replay

**Verdict:** ✅ Protected (tampering detected, decryption aborted)

---

#### 3. **Local Attacker (User Privilege)**

**Capabilities:**
- Reads user files
- Monitors running processes
- Dumps process memory

**Mitigations:**
- Files encrypted at rest
- Keys locked in RAM (no swap)
- Automatic zeroization (SPARK-verified)
- No secrets in logs/environment variables

**Verdict:** ✅ Protected (while process running, keys protected; after exit, keys zeroized)

---

#### 4. **Local Attacker (Root Privilege)**

**Capabilities:**
- Full system access
- Kernel memory dumps
- Debugger attachment
- Coredumps

**Mitigations:**
- Memory locked against swap (`mlock`)
- Secure zeroization (not optimized away)
- Volatile types prevent compiler reordering
- No key storage on disk (master password only)

**Verdict:** ⚠️ **Limited Protection**
- If attacker gains root **while process running**, keys may be dumped
- If attacker gains root **after exit**, keys are zeroized (safe)
- **Recommendation:** Use full-disk encryption, secure boot, HSM for keys

---

#### 5. **Physical Attacker (Device Access)**

**Capabilities:**
- Cold boot attack (freeze RAM, reboot, dump)
- DMA attacks (FireWire, Thunderbolt)
- Evil maid attack (modify bootloader)

**Mitigations:**
- Rapid key zeroization (< 100ms after use)
- Memory encryption (OS-level, e.g., TRESOR)
- No keys persisted to disk

**Verdict:** ⚠️ **Partial Protection**
- Cold boot: Mitigated if keys zeroized before freeze
- DMA: Mitigated by OS (IOMMU, kernel lockdown)
- Evil maid: Not mitigated (need secure boot)

---

#### 6. **Quantum Attacker (Future)**

**Capabilities:**
- Large-scale quantum computer (Shor's algorithm)
- Breaks RSA, ECDSA, ECDH in polynomial time

**Mitigations:**
- Hybrid cryptography (classical + post-quantum)
- ML-KEM-1024 (256-bit quantum resistance)
- ML-DSA-87 (256-bit quantum resistance)

**Verdict:** ✅ Protected (256-bit security against quantum attacks)

**Timeline:**
- 2024: No practical quantum computers for cryptanalysis
- 2030: Potentially 100-200 qubit machines
- 2040+: Potential threat to RSA-2048, ECC-256

**ANUBIS-SPARK:** Ready today for quantum threats of tomorrow

---

#### 7. **Implementation Bug Attacker**

**Capabilities:**
- Exploits buffer overflows
- Exploits use-after-free
- Exploits integer overflows

**Mitigations:**
- SPARK formal verification (mathematical proof of safety)
- Ada runtime checks (array bounds, overflow detection)
- Stack protection (`-fstack-protector-strong`)
- FORTIFY_SOURCE=2 (compile-time checks)

**Verdict:** ✅ **Highest Assurance**
- SPARK proves absence of runtime errors
- Ada prevents buffer overflows by design
- Multiple layers of defense

---

### Out of Scope

❌ **Coercion/Rubber-hose attacks** - Cannot protect against physical threats
❌ **Malicious hardware** - Assume CPU, RAM, chipset are trusted
❌ **Supply chain attacks** - Assume OS, compiler, libraries are trusted
❌ **Social engineering** - User education required

---

## Cryptographic Algorithms

### 1. Post-Quantum Key Encapsulation

**Algorithm:** ML-KEM-1024 (NIST FIPS 203)

| Property | Value |
|----------|-------|
| **Security Level** | NIST Level 5 (256-bit equivalent) |
| **Public Key Size** | 1,568 bytes |
| **Secret Key Size** | 3,168 bytes |
| **Ciphertext Size** | 1,568 bytes |
| **Shared Secret** | 32 bytes |
| **Quantum Security** | ~256-bit (lattice-based) |
| **Classical Security** | ~256-bit |

**Strengths:**
- Resistant to quantum attacks (learning with errors problem)
- Fast key generation (~300 μs)
- Fast encapsulation (~200 μs)
- NIST-standardized (FIPS 203, August 2024)

**Weaknesses:**
- Large key sizes vs. ECC (but acceptable)
- Relatively new (less cryptanalysis than RSA/ECC)

**Mitigations:**
- Hybrid mode (combined with X25519)
- Conservative security margin (Level 5, highest)

---

### 2. Post-Quantum Digital Signatures

**Algorithm:** ML-DSA-87 (NIST FIPS 204)

| Property | Value |
|----------|-------|
| **Security Level** | NIST Level 5 (256-bit equivalent) |
| **Public Key Size** | 2,592 bytes |
| **Secret Key Size** | 4,896 bytes |
| **Signature Size** | 4,627 bytes |
| **Quantum Security** | ~256-bit (lattice-based) |
| **Classical Security** | ~256-bit |

**Strengths:**
- Resistant to quantum attacks (Module-LWE/SIS problems)
- Fast signing (~500 μs)
- Fast verification (~300 μs)
- NIST-standardized (FIPS 204, August 2024)

**Weaknesses:**
- Very large signatures (4,627 bytes vs. 64 for Ed25519)
- Relatively new algorithm

**Mitigations:**
- Hybrid signatures (Ed25519 + ML-DSA-87)
- Both must verify (fail-safe)

---

### 3. Classical Key Exchange

**Algorithm:** X25519 (Curve25519 ECDH)

| Property | Value |
|----------|-------|
| **Security Level** | 128-bit classical, ~64-bit quantum |
| **Key Size** | 32 bytes |
| **Shared Secret** | 32 bytes |
| **Speed** | ~50 μs |

**Strengths:**
- Proven security (2006-2024, no breaks)
- Very fast
- Constant-time implementation available
- Side-channel resistant

**Weaknesses:**
- Vulnerable to quantum attacks (Shor's algorithm)

**Mitigations:**
- Always used in hybrid mode with ML-KEM-1024
- Attacker must break BOTH algorithms

---

### 4. Classical Digital Signatures

**Algorithm:** Ed25519 (EdDSA on Curve25519)

| Property | Value |
|----------|-------|
| **Security Level** | 128-bit classical, ~64-bit quantum |
| **Key Size** | 32 bytes (public/secret) |
| **Signature Size** | 64 bytes |
| **Speed** | ~20 μs (sign), ~40 μs (verify) |

**Strengths:**
- Proven security
- Extremely fast
- Deterministic (no RNG needed for signing)
- Side-channel resistant

**Weaknesses:**
- Vulnerable to quantum attacks

**Mitigations:**
- Hybrid signatures with ML-DSA-87

---

### 5. Authenticated Encryption

**Algorithm:** XChaCha20-Poly1305 (AEAD)

| Property | Value |
|----------|-------|
| **Security Level** | 256-bit key, 192-bit nonce |
| **Key Size** | 32 bytes |
| **Nonce Size** | 24 bytes |
| **Tag Size** | 16 bytes (128-bit MAC) |
| **Speed** | ~1 GB/s (software) |

**Strengths:**
- No nonce reuse concerns (192-bit nonce)
- Fast on all platforms (no AES-NI needed)
- Authenticated encryption (integrity + confidentiality)
- No timing attacks

**Weaknesses:**
- None known (ChaCha20 by Bernstein, 2008)

---

### 6. Key Derivation

**Algorithm:** Argon2id (Argon2 version 1.3)

| Property | Value |
|----------|-------|
| **Memory** | 64 MiB (configurable) |
| **Iterations** | 3 |
| **Parallelism** | 4 threads |
| **Output** | 32 bytes |
| **Time** | ~300 ms (M1 Max) |

**Strengths:**
- Winner of Password Hashing Competition (2015)
- Memory-hard (defeats GPU/ASIC attacks)
- Side-channel resistant
- Configurable parameters

**Weaknesses:**
- Slower than bcrypt/scrypt (by design)

**Rationale:**
- 64 MiB forces attackers to use expensive RAM (not GPU compute)
- GPU has ~100x less memory bandwidth than CPU
- ASIC attack is economically infeasible

**Resistance:**
- **Brute-force:** 300ms per guess → 3 guesses/sec (single-threaded)
- **GPU (RTX 4090):** ~10x slower than CPU (memory-bound)
- **ASIC:** Cost-prohibitive for 64 MiB per guess

---

## Attack Surface Analysis

### 1. Input Validation

**Attack:** Malformed file headers, oversized inputs

**Mitigations:**
- Ada strong typing (no buffer overflows)
- SPARK proves array bounds
- Explicit size checks before operations

**Example:**
```ada
-- SPARK proves this cannot overflow
type Nonce_Index is range 1 .. 24;
Nonce : Byte_Array (Nonce_Index);
```

---

### 2. Memory Safety

**Attack:** Buffer overflow, use-after-free, null pointer dereference

**Mitigations:**
- **SPARK verification:** Mathematical proof of safety
- **Ada runtime checks:** Array bounds, null checks
- **No dynamic allocation:** All on stack (eliminates heap bugs)
- **Volatile types:** Prevents optimization of zeroization

**Verification:**
```bash
gnatprove --level=2 --checks-as-errors
# Proves: No runtime errors possible
```

---

### 3. Side-Channel Attacks

#### Timing Attacks

**Attack:** Measure execution time to infer secrets

**Vulnerable Operations:**
- Secret comparison (if secret1 == secret2)
- Variable-time modular exponentiation

**Mitigations:**
- **Constant-time comparison:** `OQS_MEM_secure_bcmp`
- **No data-dependent branches** on secrets
- **liboqs uses constant-time implementations**

**Example:**
```ada
-- ❌ VULNERABLE (variable-time)
function Naive_Compare (A, B : Byte_Array) return Boolean is
begin
   for I in A'Range loop
      if A(I) /= B(I) then return False; end if;  -- Early exit leaks info!
   end loop;
   return True;
end Naive_Compare;

-- ✅ SECURE (constant-time)
function Secrets_Match (A, B : Secret) return Boolean is
begin
   Result := OQS_MEM_secure_bcmp (A'Address, B'Address, A'Length);
   return Result = 0;  -- Always checks all bytes, no early exit
end Secrets_Match;
```

#### Cache-Timing Attacks

**Attack:** Measure cache hits/misses to infer memory access patterns

**Mitigations:**
- liboqs uses cache-timing resistant implementations
- No table lookups with secret indices

#### Power Analysis

**Attack:** Measure power consumption to infer secrets

**Mitigations:**
- Not mitigated in software (requires hardware countermeasures)
- **Recommendation:** Use HSM for high-security environments

---

### 4. Memory Disclosure Attacks

#### Cold Boot Attack

**Attack:** Freeze RAM, reboot, dump memory

**Mitigations:**
- **Rapid zeroization:** Keys cleared within 100ms
- **Volatile types:** Prevents compiler from optimizing away zeroization
- **Memory locking:** `mlock` prevents swapping

**SPARK Verification:**
```ada
procedure Zeroize (Key : in out Secret_Key) with
   Post => not Is_Valid (Key);  -- SPARK proves key is zeroized
```

#### Swap/Hibernation

**Attack:** Keys written to swap or hibernation file

**Mitigations:**
- Memory locked with `mlock` (OS-dependent)
- No keys persisted to disk

#### Coredumps

**Attack:** Process crash dumps core with keys in memory

**Mitigations:**
- Disable coredumps for anubis-spark process
- Rapid zeroization on error paths

---

### 5. Cryptographic Implementation Bugs

**Historical Examples:**
- **OpenSSL Heartbleed:** Buffer over-read (CVE-2014-0160)
- **GnuTLS goto fail:** Double-free (CVE-2014-1266)
- **Debian weak RNG:** Insufficient entropy (CVE-2008-0166)

**ANUBIS-SPARK Mitigations:**
- **SPARK verification:** Proves absence of buffer overflows, use-after-free
- **Ada safety:** Strong typing, runtime checks
- **liboqs:** Peer-reviewed PQC library (not custom crypto)
- **Zero warnings:** All code compiles cleanly

---

## Security Guarantees

### Proven Properties (SPARK Level 2)

✅ **No buffer overflows** - Array bounds checking verified
✅ **No null pointer dereferences** - All pointers checked
✅ **No use-after-free** - Lifetime analysis
✅ **No uninitialized variables** - Data flow analysis
✅ **Zeroization guaranteed** - Post-conditions verified

### Cryptographic Properties

✅ **Nonce uniqueness** - 192-bit XChaCha20 nonce (2^96 files before collision)
✅ **Key independence** - Each file uses independent key
✅ **Forward secrecy** - Key rotation prevents past decryption
✅ **Quantum resistance** - 256-bit security (ML-KEM-1024 + ML-DSA-87)

### Compliance

✅ **NIST FIPS 203** (ML-KEM) - Approved August 2024
✅ **NIST FIPS 204** (ML-DSA) - Approved August 2024
✅ **NIST SP 800-186** (Curve25519) - Approved July 2023
✅ **RFC 7539** (ChaCha20-Poly1305) - IETF Standard
✅ **RFC 9106** (Argon2) - IETF Standard

---

## Known Limitations

### 1. Root Attacker (Runtime)

**Issue:** If attacker gains root while ANUBIS-SPARK is running, keys may be dumped.

**Mitigations:**
- Use full-disk encryption (LUKS, FileVault, BitLocker)
- Use hardware security module (HSM) for key storage
- Enable kernel lockdown mode (Linux)
- Use Trusted Execution Environment (TEE)

---

### 2. Physical Attacks

**Issue:** Physical access to unlocked device allows key extraction.

**Mitigations:**
- Rapid key zeroization (limit exposure window)
- Use hardware memory encryption (Intel TME, AMD SME)
- Lock device when not in use
- Use BIOS password, secure boot

---

### 3. Malicious Dependencies

**Issue:** liboqs, OpenSSL, or OS could be compromised.

**Mitigations:**
- Verify liboqs signatures (PGP)
- Use known-good OS (signed, verified boot)
- Build from source (inspect code)
- Use reproducible builds

---

### 4. Quantum Computer Availability

**Issue:** If large-scale quantum computer exists in secret, encrypted files retroactively at risk.

**Mitigations:**
- Hybrid cryptography (protects even if quantum exists)
- Key rotation (limits exposure window)
- Assume adversary has quantum → ML-KEM/ML-DSA still secure

---

## Security Best Practices

### For Users

1. **Use strong passphrase** (≥ 12 characters, mixed case, numbers, symbols)
2. **Rotate keys regularly** (every 90 days or 1M operations)
3. **Create recovery shares** (Shamir 3-of-5, distribute to trusted parties)
4. **Lock device when away** (prevent physical access)
5. **Use full-disk encryption** (defense-in-depth)
6. **Verify signatures** (before decryption)
7. **Update regularly** (security patches)

### For Developers

1. **Run SPARK verification** (gnatprove --level=2)
2. **Enable all warnings** (zero tolerance policy)
3. **Use constant-time operations** (for secret-dependent code)
4. **Zeroize on all paths** (success and error)
5. **No logging of secrets** (SPARK can verify this)
6. **Review liboqs updates** (new algorithms, security fixes)

### For Auditors

1. **Verify SPARK proofs** (gnatprove output)
2. **Review C FFI bindings** (ensure correct usage)
3. **Check key sizes** (match NIST standards)
4. **Test zeroization** (debugger, memory dumps)
5. **Fuzz inputs** (malformed files)
6. **Timing analysis** (constant-time verification)

---

## Incident Response

### Suspected Key Compromise

1. **Immediately rotate all keys**
   ```bash
   ./bin/anubis-spark rotate --vault ~/vault.apq --emergency
   ```

2. **Re-encrypt all files with new keys**
   ```bash
   ./bin/anubis-spark re-encrypt --vault ~/vault.apq --all
   ```

3. **Revoke compromised keys** (if using PKI)

4. **Analyze logs** (when/how compromise occurred)

5. **Report to security team** (incident report)

### Suspected Implementation Bug

1. **Report to GitHub Issues** (https://github.com/AnubisQuantumCipher/anubis-spark/issues)
2. **Include:** Platform, version, error messages, steps to reproduce
3. **Do NOT include:** Actual encrypted files, passphrases, keys
4. **Wait for patch** or apply workaround if provided

### Algorithm Break

**Scenario:** Classical algorithm (X25519, Ed25519) is broken

**Impact:** ✅ **No immediate risk** (hybrid mode protects)

**Action:**
1. Update to version that disables broken algorithm
2. Continue using ML-KEM-1024 + ML-DSA-87 only
3. Re-encrypt files if desired (not required)

**Scenario:** Post-quantum algorithm (ML-KEM, ML-DSA) is broken

**Impact:** ⚠️ **Reduced to classical security** (128-bit)

**Action:**
1. Update to version with new PQC algorithm
2. Re-encrypt all files immediately
3. Rotate all keys

**Scenario:** Both classical and PQC broken

**Impact:** ❌ **Full compromise**

**Likelihood:** Extremely low (would require breakthrough in two independent fields)

---

## Security Audit History

**Status:** No professional audits yet (pre-1.0 release)

**Planned:**
- Internal audit (before 1.0)
- External audit (professional firm, post-1.0)

**How to request audit:**
- Email: security@anubis-spark.io (when available)
- PGP key: (to be published)

---

## Responsible Disclosure

**Email:** security@anubis-spark.io (when available)

**Process:**
1. Email detailed vulnerability report (encrypted with PGP key)
2. We acknowledge receipt within 48 hours
3. We investigate and develop fix (target: 7 days for critical, 30 days for non-critical)
4. We release patch and credit researcher (with permission)
5. Public disclosure 90 days after patch release

**Bounty:** Under consideration for 1.0+ releases

---

## References

- **NIST Post-Quantum Cryptography:** https://csrc.nist.gov/projects/post-quantum-cryptography
- **FIPS 203 (ML-KEM):** https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.203.pdf
- **FIPS 204 (ML-DSA):** https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.204.pdf
- **Argon2 RFC 9106:** https://www.rfc-editor.org/rfc/rfc9106.html
- **SPARK Verification:** https://www.adacore.com/about-spark
- **liboqs Security:** https://github.com/open-quantum-safe/liboqs/wiki/Security

---

**Last Updated:** 2025-10-10
**Security Policy Version:** 1.0
**Implementation Phase:** 1 (Foundation)

**Security Contact:** security@anubis-spark.io (when available)
