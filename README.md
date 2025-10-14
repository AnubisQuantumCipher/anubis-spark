# ANUBIS-SPARK

**Ada/SPARK Interface to Post-Quantum Cryptography**

An Ada wrapper for file encryption using hybrid classical and post-quantum algorithms via libsodium and liboqs.

[![Post-Quantum](https://img.shields.io/badge/Post--Quantum-Hybrid-orange)](https://openquantumsafe.org/)
[![Dependencies](https://img.shields.io/badge/Dependencies-libsodium%20%7C%20liboqs-blue)](https://libsodium.org)

## What Is This?

ANUBIS-SPARK is an Ada/SPARK interface to established C cryptography libraries. It provides:

- **Classical crypto** via libsodium (X25519, Ed25519, XChaCha20-Poly1305, Argon2id)
- **Post-quantum crypto** via liboqs (ML-KEM-1024, ML-DSA-87 per NIST FIPS 203/204)
- **Hybrid key exchange and signatures** combining both approaches
- **File encryption** with streaming AEAD (handles files of any size)
- **Passphrase-based key derivation** using memory-hard Argon2id

## What's Verified?

**Ada Wrapper Code**: Some SPARK contracts verify interface-level properties (buffer bounds, basic pre/postconditions).

**What's NOT Verified**:
- The actual cryptographic implementations (delegated to C libraries)
- Portions using `pragma Assume` or `pragma SPARK_Mode (Off)`
- The C library code (libsodium, liboqs)

**Security Foundation**: Cryptographic security comes from libsodium (battle-tested, widely audited) and liboqs (NIST-standardized PQC algorithms). The Ada wrapper adds type safety and some interface validation.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  CLI Interface (anubis-spark)                           │
├─────────────────────────────────────────────────────────┤
│  Ada Wrapper Layer (some SPARK contracts)               │
│   ├─ Key management                                     │
│   ├─ File encryption/decryption orchestration           │
│   └─ Keystore operations                                │
├─────────────────────────────────────────────────────────┤
│  Ada FFI Bindings                                       │
│   ├─ libsodium bindings (classical crypto)             │
│   └─ liboqs bindings (post-quantum crypto)             │
├─────────────────────────────────────────────────────────┤
│  C Libraries (actual crypto implementations)            │
│   ├─ libsodium 1.0.20                                  │
│   └─ liboqs 0.14.0                                     │
└─────────────────────────────────────────────────────────┘
```

## Cryptographic Algorithms

### Classical (via libsodium)
- **X25519** - Elliptic curve key exchange
- **Ed25519** - Digital signatures
- **XChaCha20-Poly1305** - Authenticated encryption
- **Argon2id** - Memory-hard key derivation (1 GiB default)

### Post-Quantum (via liboqs)
- **ML-KEM-1024** (NIST FIPS 203) - Key encapsulation
- **ML-DSA-87** (NIST FIPS 204) - Digital signatures

### Hybrid Approach
Files are encrypted with keys derived from both classical (X25519) and post-quantum (ML-KEM-1024) key exchanges. An attacker must break both to decrypt.

## Installation

### Option 1: Binary Releases (Recommended)

Download pre-compiled binaries:

**Linux x86_64:**
```bash
wget https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-linux-x86_64.tar.gz
tar xzf anubis-spark-linux-x86_64.tar.gz
sudo install -m 755 anubis-spark-linux-x86_64/anubis_main /usr/local/bin/anubis-spark
```

**macOS (Intel or Apple Silicon via Rosetta):**
```bash
curl -LO https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-macos-universal.tar.gz
tar xzf anubis-spark-macos-universal.tar.gz
sudo install -m 755 anubis-spark-macos-universal/anubis_main /usr/local/bin/anubis-spark
```

Verify:
```bash
anubis-spark version
```

### Option 2: Build from Source

**Prerequisites:**
```bash
# macOS
brew install libsodium liboqs pkg-config

# Ubuntu/Debian
sudo apt-get install libsodium-dev liboqs-dev pkg-config build-essential

# Install Alire (Ada package manager)
curl -fsSL https://alire.ada.dev/install.sh | sh
export PATH="$HOME/.alire/bin:$PATH"
```

**Build:**
```bash
git clone https://github.com/AnubisQuantumCipher/anubis-spark
cd anubis-spark
alr build --release
./bin/anubis_main version
```

## Usage

### Generate Keys
```bash
anubis-spark keygen --output my_identity.key
# Creates hybrid keypair (classical + post-quantum)
```

### Encrypt File
```bash
anubis-spark encrypt --key my_identity.key --input document.pdf
# Creates: document.pdf.anubis
```

### Decrypt File
```bash
anubis-spark decrypt --key my_identity.key --input document.pdf.anubis
# Creates: document.pdf.anubis.decrypted
```

### Keystore Formats

**ANUBISK2 (Encrypted Identity):**
- Passphrase-protected keystore (~12 KB)
- Argon2id key derivation (1 GiB RAM, 4 iterations)
- XChaCha20-Poly1305 encryption

**ANUBISK3 (Multi-Keyslot):**
- LUKS2-inspired multi-keyslot design
- 8 independent keyslots with AF-Splitter
- Each keyslot can have different passphrase

## Performance

Tested on Apple Silicon (M-series):

| File Size | Encrypt | Decrypt | Throughput |
|-----------|---------|---------|------------|
| 66 MB | 3.1s | 4.2s | ~20 MB/s |
| 2 GB | 62s | 117s | ~30 MB/s |

Argon2id dominates encryption time (~2-3s for key derivation).
Memory usage: 64 MB chunks + ~1 GiB for Argon2id during key derivation.

## Security Considerations

**What This Provides:**
- Hybrid crypto approach (break both classical AND PQC to decrypt)
- Memory-hard KDF resists GPU/ASIC attacks
- Streaming encryption supports large files
- NIST-standardized PQC algorithms (FIPS 203/204)

**What to Know:**
- Security depends on libsodium and liboqs implementations
- Ada wrapper adds interface validation but doesn't replace crypto library audits
- Passphrase strength critical (Argon2id slows brute-force but can't fix weak passwords)
- Post-quantum algorithms are new - use hybrid mode for defense in depth

## Project Status

**What Works:**
- File encryption/decryption (all sizes tested to 2 GB)
- Hybrid key exchange and signatures
- Encrypted keystores with Argon2id
- Streaming AEAD with per-chunk authentication
- Cross-platform builds (Linux, macOS)

**Known Limitations:**
- Windows not supported (liboqs availability)
- Some Ada code uses `pragma Assume` or `SPARK_Mode (Off)`
- No hardware security module (HSM) support yet
- Binary must link to OpenSSL on macOS (system library)

## Contact

**Maintainer**: sic.tau@pm.me
**Repository**: https://github.com/AnubisQuantumCipher/anubis-spark
**Issues**: https://github.com/AnubisQuantumCipher/anubis-spark/issues

## License

MIT OR Apache-2.0 (dual-licensed)

## References

- [NIST Post-Quantum Cryptography](https://csrc.nist.gov/projects/post-quantum-cryptography)
- [FIPS 203 (ML-KEM)](https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.203.pdf)
- [FIPS 204 (ML-DSA)](https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.204.pdf)
- [Open Quantum Safe](https://openquantumsafe.org/)
- [libsodium](https://libsodium.org/)
- [Argon2](https://github.com/P-H-C/phc-winner-argon2)

---

**Current Version**: v2.0.8
**Dependencies**: libsodium 1.0.20, liboqs 0.14.0, OpenSSL (macOS only)
**Ada Compiler**: GNAT FSF 14.2.1 or later

**Security Notice**: This software provides an Ada interface to established cryptographic libraries. Review the code and underlying libraries before using with sensitive data. The security properties depend primarily on libsodium and liboqs implementations.
