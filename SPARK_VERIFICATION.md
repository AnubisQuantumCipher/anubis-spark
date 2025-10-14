# SPARK Verification Status

**Project**: ANUBIS-SPARK
**Version**: 2.0.8

## Verification Summary

ANUBIS-SPARK uses SPARK contracts for some interface-level verification of the Ada wrapper code.

**Verification Conditions**: 151 VCs total
- 145 VCs proven automatically by SMT solvers (CVC5, Z3)
- 6 VCs resolved using `pragma Assume` for properties not automatically provable

**What This Means**:
- Some interface contracts are verified (buffer bounds, basic pre/postconditions)
- Not all code is under SPARK verification (some uses `pragma SPARK_Mode (Off)`)
- The underlying cryptographic implementations (libsodium, liboqs C code) are not verified by SPARK

## What's Verified

**Ada Wrapper Interfaces**:
- Buffer length checks on some operations
- Basic preconditions and postconditions
- Some initialization checking

**Examples of Contracts**:
```ada
-- Example: Basic length preservation property
procedure XChaCha20_Encrypt
  with Pre => Plaintext'Length > 0 and Ciphertext'Length >= Plaintext'Length,
       Post => (if Success then Ciphertext'Length = Plaintext'Length);
```

## What's NOT Verified

**Cryptographic Implementations**: The actual crypto (X25519, Ed25519, XChaCha20, Argon2id, ML-KEM, ML-DSA) is implemented in C libraries (libsodium, liboqs). SPARK verification does not cover these.

**Code Using pragma Assume**: 6 verification conditions use `pragma Assume` which tells the prover to accept properties without proof. These are not mathematically proven.

**Code with SPARK_Mode (Off)**: Some modules disable SPARK analysis entirely where the contracts couldn't be satisfied.

## Verification Approach

```bash
# Run SPARK verification
gnatprove -P anubis_spark.gpr --level=1 --prover=cvc5 --timeout=300

# Expected output: 151 total VCs, 0 unproved (but 6 assumed)
```

**Tools Used**:
- GNATprove 14.1.1
- CVC5 1.1.2 (SMT solver)
- Z3 4.13.0 (backup SMT solver)

## Security Foundation

**Where Security Actually Comes From**:
1. **libsodium 1.0.20** - Battle-tested classical crypto (widely audited)
2. **liboqs 0.14.0** - NIST-standardized post-quantum algorithms
3. **Ada type system** - Prevents some classes of bugs
4. **SPARK contracts** - Catches some interface-level errors

**What SPARK Verification Provides**:
- Some confidence in wrapper code correctness
- Detection of certain buffer overflow conditions
- Basic interface validation

**What It Doesn't Provide**:
- Cryptographic algorithm correctness (delegated to C libraries)
- Side-channel resistance proofs
- Complete formal verification of all code paths

## Reproducing Verification

```bash
git clone https://github.com/AnubisQuantumCipher/anubis-spark
cd anubis-spark
alr build
~/.local/share/alire/releases/gnatprove_14.1.1_*/bin/gnatprove \
  -P anubis_spark.gpr --level=1 --prover=cvc5 --timeout=300
```

Check `obj/gnatprove/gnatprove.out` for results.

## References

- [SPARK User's Guide](https://docs.adacore.com/spark2014-docs/html/ug/)
- [GNATprove Tool](https://docs.adacore.com/gnatprove-docs/html/)
- [libsodium](https://libsodium.org/)
- [Open Quantum Safe](https://openquantumsafe.org/)

---

**Last Updated**: 2025-10-14
**Maintainer**: sic.tau@pm.me
