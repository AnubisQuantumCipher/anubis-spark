# ANUBIS-SPARK – Assurance Case (v1.1.0 Platinum)

## 1. Scope

**Purpose:** Hybrid post-quantum file encryption CLI ensuring confidentiality and authenticity of large files.

**Verified scope:** Ada/SPARK orchestration (header serialize/parse; hybrid KDF; streaming AEAD; keystore).

**Trusted:** libsodium (XChaCha20-Poly1305, X25519, Ed25519, Argon2id), liboqs (ML-KEM-1024, ML-DSA-87), OS RNG, atomic rename.

---

## 2. Assets & Threats

**Assets:** plaintext, keystores, minimal metadata.

**Adversaries:** remote attackers, offline brute-forcers, future quantum attackers.

**Data lifetime:** 10–20+ years (post-quantum security required).

---

## 3. What is Proved (SPARK)

### Functional Correctness

- **AoRTE** (Absence of Run-Time Errors) on orchestration modules:
  - No out-of-bounds array access
  - No uninitialized reads
  - No integer overflows
  - No type confusion

- **Encrypt∘Decrypt = Identity** (pure model):
  - Valid preconditions → Ok and original plaintext restored
  - Proven in `anubis_aead_pure.ads`

- **Header-to-Chunk AAD Binding:**
  - Any header modification causes **all** chunk verifications to fail
  - No partial acceptance of tampered files
  - Proven in `anubis_header_io.ads`

- **Zeroization Postconditions:**
  - Buffers wiped (all bytes = 0) on failure/teardown
  - Validity flags cleared
  - Proven in `anubis_zeroize.ads` and `anubis_types.ads`

- **Data-flow Contracts:**
  - `Global`/`Depends` clauses restrict unintended information flows
  - Present in all SPARK packages

---

## 4. What is Assumed (TCB)

- Correctness & constant-time behavior of cryptographic primitives in libsodium/liboqs
- Unpredictable OS RNG (`/dev/urandom` or equivalent)
- Atomic rename semantics (file system guarantees)
- Compiler correctness (GNAT FSF 14.2.1)
- No microarchitectural side-channel attacks beyond constant-time assumptions

---

## 5. What is Tested

### Known-Answer Tests (KATs)

- Header serialize/parse round-trip
- Encrypt/decrypt across multiple file sizes
- Hybrid KDF derivations (X25519 + ML-KEM-1024)
- Argon2id parameter enforcement (1 GiB RAM, 4 iterations)

### Boundary/Negative Tests

- **Truncation:** Files truncated at various offsets → rejected
- **Reordering:** Chunks reordered → rejected
- **Header field mutation:** Any header field modified → rejected
- **Tag corruption:** Authentication tags flipped → rejected
- **Wrong keys:** Decryption with incorrect keys → rejected with zeroized outputs

All tests in `tests/test_boundary.adb`

### Fuzz Testing

- Random mutation of serialized headers
- Random mutation of chunk order
- Random byte flips in ciphertext

### Self-Test CLI

- KEM keygen, encap/decap (ML-KEM-1024)
- Classical + PQ signatures (Ed25519, ML-DSA-87)
- Hybrid signatures (both must verify)

Command: `anubis-spark test`

---

## 6. Side-Channel Hygiene

- **Constant-time comparisons:** Secret comparisons use `sodium_memcmp`
- **No secret-dependent branching:** Ada orchestration avoids conditional branches on secret data
- **Restricted logging:** Only public metadata logged (file sizes, operation types)
- **Primitive reliance:** Heavy lifting delegated to constant-time primitives in libsodium/liboqs

---

## 7. Supply Chain & Build Integrity

- **Pinned versions/hashes:** libsodium 1.0.20, liboqs 0.14.0
- **CI proofs:** GitHub Actions runs GNATprove on every commit
- **Reproducible builds:** Build flags documented in `anubis_spark.gpr`
- **Static linking:** liboqs.a and libsodium.a linked directly (no dynamic dependencies)

---

## 8. Operational Guidance

### Keystore Security

- **Passphrase required:** Use ANUBISK2 encrypted format (Argon2id SENSITIVE)
- **File permissions:** `chmod 600` on all `.key` files
- **Offline backups:** Store encrypted keystores on offline media
- **Key rotation:** Rotate identity keys every 12–24 months
- **Environment separation:** Use separate identities for dev/staging/production

### Key Generation

```bash
anubis-spark keygen --output identity.key --passphrase "YourSecurePassphrase"
```

### Encryption

```bash
anubis-spark encrypt --key identity.key --passphrase "YourSecurePassphrase" --input sensitive.doc
```

### Decryption

```bash
anubis-spark decrypt --key identity.key --passphrase "YourSecurePassphrase" --input sensitive.doc.anubis
```

---

## 9. Residual Risks

### Out of Scope

- **Microarchitectural side-channels:** Spectre, Meltdown, cache timing attacks beyond constant-time primitives
- **Compromised OS or toolchain:** Kernel-level malware, compiler backdoors
- **Physical exfiltration:** Cold-boot attacks, DMA attacks, hardware implants
- **Novel cryptanalysis:** Future breaks of ML-KEM-1024, ML-DSA-87, or classical primitives
- **Supply-chain attacks:** Compromised libsodium/liboqs distributions (mitigated by hash verification)

### Mitigation

- **Defense-in-depth:** Hybrid classical + PQ ensures both must fail
- **Operational security:** Isolate sensitive operations on air-gapped systems
- **Monitoring:** Audit logs for unauthorized access to keystores

---

## 10. Evidence Index

### Proof Logs

- Location: `gnatprove/`
- Generated by: `gnatprove -P anubis_spark.gpr --level=4`
- View summary: `cat gnatprove/gnatprove.out`

### CI Runs & Artifacts

- GitHub Actions: "SPARK Platinum Gates" workflow
- Workflow file: `.github/workflows/prove.yml`
- Badge: [![SPARK Platinum Gates](https://github.com/AnubisQuantumCipher/anubis-spark/actions/workflows/prove.yml/badge.svg)](https://github.com/AnubisQuantumCipher/anubis-spark/actions/workflows/prove.yml)

### Test Outputs

- KAT logs: `bin/anubis_main test`
- Boundary test: `bin/test_boundary`
- Fuzz seeds: TBD (future work)

---

## 11. Version & Contact

- **Version:** 1.1.0 Platinum
- **Date:** 2025-10-11
- **Repository:** https://github.com/AnubisQuantumCipher/anubis-spark
- **Issues:** https://github.com/AnubisQuantumCipher/anubis-spark/issues
- **License:** Apache 2.0

---

## 12. Auditor Checklist

For external auditors reviewing this system:

- [ ] Verify GNATprove runs clean (no unproven checks)
- [ ] Review contracts in `src/crypto/anubis_contracts.ads`
- [ ] Inspect zeroization postconditions in `anubis_types.ads`
- [ ] Run boundary tests (`bin/test_boundary`)
- [ ] Check library versions: `anubis-spark version`
- [ ] Review CI workflow logs
- [ ] Confirm no compiler warnings: `alr build 2>&1 | grep -i warning`
- [ ] Test encrypted keystore workflow end-to-end

---

**End of Assurance Case**
