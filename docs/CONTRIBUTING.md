# Contributing to ANUBIS-SPARK

Thank you for your interest in contributing to ANUBIS-SPARK! This document provides guidelines for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [How Can I Contribute?](#how-can-i-contribute)
- [Development Setup](#development-setup)
- [Coding Standards](#coding-standards)
- [Testing Requirements](#testing-requirements)
- [Pull Request Process](#pull-request-process)
- [SPARK Verification](#spark-verification)
- [Security Considerations](#security-considerations)

---

## Code of Conduct

### Our Standards

- **Be respectful** - Treat all contributors with respect
- **Be constructive** - Provide helpful, actionable feedback
- **Be collaborative** - Work together towards the best solution
- **Be patient** - Remember that everyone is learning

### Unacceptable Behavior

- Harassment, discrimination, or personal attacks
- Trolling, insulting comments, or political attacks
- Publishing others' private information
- Other conduct which could reasonably be considered inappropriate

---

## How Can I Contribute?

### Reporting Bugs

**Before submitting a bug report:**
- Check the [existing issues](https://github.com/AnubisQuantumCipher/anubis-spark/issues)
- Try to reproduce on the latest version
- Collect system information (OS, Ada version, liboqs version)

**Bug Report Template:**

```markdown
**Description:**
Clear description of the bug

**Steps to Reproduce:**
1. Step one
2. Step two
3. ...

**Expected Behavior:**
What should happen

**Actual Behavior:**
What actually happened

**Environment:**
- OS: macOS 14.2 / Ubuntu 24.04 / etc.
- GNAT version: 14.2.1
- liboqs version: 0.14.0
- Alire version: 2.0.2

**Error Messages:**
```
Paste full error output
```

**Additional Context:**
Any other relevant information
```

---

### Suggesting Enhancements

**Enhancement suggestions are welcome for:**
- New cryptographic algorithms
- Performance optimizations
- Additional SPARK proofs
- Platform support
- Documentation improvements

**Enhancement Template:**

```markdown
**Feature Request:**
Clear description of the enhancement

**Motivation:**
Why is this feature needed?

**Proposed Solution:**
How should it work?

**Alternatives:**
What other approaches did you consider?

**Additional Context:**
Mockups, examples, references
```

---

### Contributing Code

We welcome contributions in these areas:

#### High Priority
- ✅ **SPARK Proofs** - Additional formal verification
- ✅ **Test Coverage** - More comprehensive tests
- ✅ **Documentation** - API docs, examples, tutorials
- ✅ **Bug Fixes** - Security vulnerabilities, correctness issues

#### Medium Priority
- **X25519/Ed25519 Integration** - Classical crypto bindings
- **Argon2id Bindings** - Key derivation
- **XChaCha20-Poly1305** - AEAD encryption
- **Performance Optimizations** - Benchmarking, profiling

#### Future
- **Shamir Secret Sharing** - Pure Ada implementation
- **Zero-Knowledge Proofs** - ZK protocol implementation
- **HSM Integration** - Hardware security module support
- **Platform Support** - Windows, ARM, embedded

---

## Development Setup

### Prerequisites

```bash
# Install dependencies (see INSTALL.md)
brew install liboqs  # macOS
# or
sudo apt install liboqs-dev  # Linux

# Install Alire
curl -L https://github.com/alire-project/alire/releases/latest/download/alr-*.zip -o alire.zip
unzip alire.zip && mv bin/alr ~/.local/bin/
```

### Fork and Clone

```bash
# Fork on GitHub, then:
git clone https://github.com/YOUR_USERNAME/anubis-spark.git
cd anubis-spark
```

### Build and Test

```bash
# Build
alr exec -- gprbuild -P anubis_spark.gpr

# Run tests
./bin/test_minimal
./bin/test_pqc

# Run SPARK verification
gnatprove -P anubis_spark.gpr --level=2
```

### Create a Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/issue-number-description
```

---

## Coding Standards

### Ada Style Guide

Follow the **Ada Quality and Style Guide** ([link](https://en.wikibooks.org/wiki/Ada_Style_Guide)):

#### Naming Conventions

```ada
-- ✅ GOOD
Package_Name
Type_Name
Variable_Name
CONSTANT_NAME

procedure Do_Something;
function Is_Valid (Key : Secret_Key) return Boolean;

-- ❌ BAD
packagename
typeName
variable_name
constantName
```

#### Indentation

- **3 spaces** per level (Ada standard)
- No tabs (spaces only)

```ada
-- ✅ GOOD
procedure Example is
begin
   if Condition then
      Do_Something;
   end if;
end Example;

-- ❌ BAD (2 spaces, tabs)
procedure Example is
begin
  if Condition then
    Do_Something;
  end if;
end Example;
```

#### Comments

```ada
-- ✅ GOOD
-- This function validates the signature using ML-DSA-87.
-- Returns True if signature is valid, False otherwise.
function ML_DSA_Verify (...) return Boolean;

-- ❌ BAD (useless comment)
-- Verify
function ML_DSA_Verify (...) return Boolean;
```

#### Line Length

- **Maximum 100 characters** per line (hard limit)
- **Preferred 80 characters** for readability

---

### SPARK Annotations

#### Always Include SPARK Mode

```ada
-- ✅ GOOD (spec)
pragma SPARK_Mode (On);

package My_Package is
   ...
end My_Package;

-- ✅ GOOD (body with C FFI)
pragma SPARK_Mode (Off);  -- C FFI not provable

package body My_Package is
   ...
end My_Package;
```

#### Use Contracts

```ada
-- ✅ GOOD
procedure Generate_Keypair (
   Public_Key  : out Public_Key_Type;
   Secret_Key  : out Secret_Key_Type;
   Success     : out Boolean
) with
   Global => null,  -- No global state
   Post   => (if Success then Is_Valid (Secret_Key)
              else not Is_Valid (Secret_Key));

-- ❌ BAD (no contracts)
procedure Generate_Keypair (
   Public_Key  : out Public_Key_Type;
   Secret_Key  : out Secret_Key_Type;
   Success     : out Boolean
);
```

#### Mark Secrets as Volatile

```ada
-- ✅ GOOD
type Secret_Key is record
   Data  : Byte_Array (1 .. KEY_SIZE);
   Valid : Boolean := False;
end record with Volatile;  -- Prevents optimization

-- ❌ BAD (compiler can optimize away zeroization)
type Secret_Key is record
   Data  : Byte_Array (1 .. KEY_SIZE);
   Valid : Boolean := False;
end record;
```

---

### Security Requirements

#### No Custom Cryptography

```ada
-- ✅ GOOD (use liboqs, libsodium, OpenSSL)
Status := OQS_KEM_ml_kem_1024_keypair (...);

-- ❌ BAD (custom crypto is usually wrong)
function My_Own_AES (...) return Ciphertext;
```

#### Always Zeroize Secrets

```ada
-- ✅ GOOD
Derive_Key (Secret, Key, Success);
Zeroize_Secret (Secret);  -- Immediate

-- ❌ BAD
Derive_Key (Secret, Key, Success);
-- ... many operations ...
Zeroize_Secret (Secret);  -- Too late
```

#### Use Constant-Time Operations

```ada
-- ✅ GOOD
if Secrets_Match (A, B) then  -- Constant-time

-- ❌ BAD
if A.Data = B.Data then  -- Timing leak!
```

#### Check All Return Values

```ada
-- ✅ GOOD
Generate_Keypair (Public, Secret, Success);
if not Success then
   raise Crypto_Error with "Keypair generation failed";
end if;

-- ❌ BAD
Generate_Keypair (Public, Secret, Success);
-- Continue regardless
```

---

## Testing Requirements

### All Contributions Must Include Tests

#### Unit Tests

```ada
-- src/tests/test_my_feature.adb
with Ada.Text_IO; use Ada.Text_IO;
with My_Package;

procedure Test_My_Feature is
   -- Test setup
begin
   Put_Line ("Testing My_Feature...");

   -- Test 1: Normal case
   -- Test 2: Edge case
   -- Test 3: Error case

   Put_Line ("All tests passed!");
end Test_My_Feature;
```

#### Integration Tests

Test interaction between components:
- ML-KEM encapsulation + decapsulation
- ML-DSA sign + verify
- Hybrid operations

#### Edge Cases

Test boundary conditions:
- Empty inputs
- Maximum-size inputs
- Invalid keys
- Tampered data

---

### Test Coverage Goals

- **Critical paths:** 100% coverage (crypto operations, zeroization)
- **Normal paths:** 90%+ coverage
- **Error paths:** All error conditions tested

---

### Running Tests

```bash
# Run all tests
./bin/test_minimal
./bin/test_pqc

# Add your test to anubis_spark.gpr:
# for Main use ("anubis_main.adb", "test_pqc.adb", "test_my_feature.adb");

# Rebuild
alr exec -- gprbuild -P anubis_spark.gpr

# Run new test
./bin/test_my_feature
```

---

## Pull Request Process

### Before Submitting

**Checklist:**

- [ ] Code follows Ada style guide
- [ ] All tests pass (`./bin/test_pqc`)
- [ ] SPARK verification passes (`gnatprove --level=2`)
- [ ] No compilation warnings (zero tolerance)
- [ ] Documentation updated (if API changed)
- [ ] Commits are atomic and well-described
- [ ] Branch is up-to-date with `main`

---

### Commit Messages

**Format:**

```
<type>(<scope>): <subject>

<body>

<footer>
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `test`: Test additions/changes
- `refactor`: Code refactoring
- `perf`: Performance improvement
- `security`: Security fix (critical)

**Examples:**

```
feat(crypto): Add X25519 key exchange bindings

Implement Ada FFI bindings for libsodium's X25519 ECDH.
Includes:
- X25519_keypair
- X25519_shared_secret
- Constant-time implementation
- Full test coverage

Closes #42
```

```
fix(pqc): Fix memory leak in ML_KEM_Decapsulate

Secret_Key was not zeroized on error path.
Added explicit zeroization and SPARK postcondition.

Security impact: Medium (secret exposure on failure)
```

---

### Pull Request Template

```markdown
## Description
Brief description of changes

## Motivation
Why is this change needed?

## Changes
- [ ] Added/modified files
- [ ] Tests added
- [ ] Documentation updated

## Testing
How was this tested?

## SPARK Verification
```
Output of gnatprove
```

## Checklist
- [ ] Code follows style guide
- [ ] All tests pass
- [ ] SPARK verification passes
- [ ] No warnings
- [ ] Documentation updated
```

---

### Review Process

1. **Automated Checks** (CI/CD when available)
   - Build succeeds
   - All tests pass
   - SPARK verification passes
   - No warnings

2. **Code Review** (maintainers)
   - Security review (crypto correctness)
   - Style compliance
   - Test coverage
   - Documentation quality

3. **Approval**
   - At least 1 maintainer approval required
   - Security-critical changes require 2 approvals

4. **Merge**
   - Squash and merge (clean history)
   - Delete branch after merge

---

## SPARK Verification

### Required Proof Levels

**Level 2 (Silver):** Minimum for all contributions
- No buffer overflows
- No uninitialized variables
- No runtime errors

**Level 3 (Gold):** For critical operations
- Functional correctness
- Zeroization guarantees
- Key validity enforcement

### Running Proofs

```bash
# Level 2 (required)
gnatprove -P anubis_spark.gpr --level=2 --checks-as-errors

# Level 3 (optional, for critical code)
gnatprove -P anubis_spark.gpr --level=3 --prover=cvc5,z3,alt-ergo

# Specific file
gnatprove -P anubis_spark.gpr --level=2 -u src/crypto/anubis_types.adb
```

### Interpreting Results

**✅ Success:**
```
Phase 1 of 2: generation of Global contracts ...
Phase 2 of 2: flow analysis and proof ...
Summary of SPARK analysis
=========================

SPARK Analysis results        Total      Flow   Interval   CodePeer  Provers   Justified  Unproved
-------------------------------------------------------------------------------------------------
Data Dependencies                 .         .          .          .         .           .         .
Flow Dependencies                 .         .          .          .         .           .         .
Initialization                   12        12          .          .         .           .         .
Non-Aliasing                      .         .          .          .         .           .         .
Run-time Checks                  58         .          .          .        58           .         .
Assertions                       15         .          .          .        15           .         .
Functional Contracts             10         .          .          .        10           .         .
LSP Verification                  .         .          .          .         .           .         .
-------------------------------------------------------------------------------------------------
Total                            95        12 (12%)    .          .        83 (88%)     .         .

Proved checks:  95
Unproved checks: 0  ← This is success!
```

**❌ Failure:**
```
anubis_types.adb:42:10: medium: overflow check might fail
```

**Fix failures before submitting PR.**

---

## Security Considerations

### Security-Critical Contributions

**Extra scrutiny for:**
- Cryptographic implementations
- Key handling
- Memory management
- Zeroization
- Constant-time operations

**Required:**
- Full test coverage (100%)
- SPARK Level 3 verification
- Security review by at least 2 maintainers
- Proof that no secrets leak

---

### Reporting Security Vulnerabilities

**DO NOT** open a public issue for security vulnerabilities.

**Email:** security@anubis-spark.io (when available)

**PGP Key:** (to be published)

**Include:**
- Detailed description
- Steps to reproduce
- Proof of concept (if applicable)
- Suggested fix (if known)

We will respond within 48 hours and work to release a patch within 7 days for critical issues.

---

## Getting Help

**Questions?**
- Open a [GitHub Discussion](https://github.com/AnubisQuantumCipher/anubis-spark/discussions)
- Check existing [documentation](../README.md)
- Read the [ARCHITECTURE.md](ARCHITECTURE.md)

**Stuck on SPARK verification?**
- Check [SPARK documentation](https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/)
- Ask in GitHub Discussions
- Share `gnatprove` output for help

---

## Recognition

All contributors will be recognized in:
- `CONTRIBUTORS.md` file
- Release notes
- Project credits

**Hall of Fame:**
- Major feature contributions
- Critical bug fixes
- Significant documentation improvements
- Security vulnerability disclosures

---

## License

By contributing, you agree that your contributions will be dual-licensed under:
- MIT License
- Apache License 2.0

See [LICENSE-MIT](../LICENSE-MIT) and [LICENSE-APACHE](../LICENSE-APACHE) for details.

---

## Questions?

Feel free to:
- Open a GitHub Discussion
- Email the maintainers (when available)
- Join our chat (when available)

**Thank you for contributing to ANUBIS-SPARK!** 🔐

---

**Last Updated:** 2025-10-10
**Document Version:** 1.0
