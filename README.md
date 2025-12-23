# ANUBIS-SPARK

**Ada/SPARK Interface to Post-Quantum Cryptography**

[![Post-Quantum](https://img.shields.io/badge/Post--Quantum-Hybrid-orange)](https://openquantumsafe.org/)
[![SPARK](https://img.shields.io/badge/SPARK-Verified-green)](https://www.adacore.com/about-spark)
[![License](https://img.shields.io/badge/License-MIT%20%7C%20Apache--2.0-blue)](LICENSE-MIT)

Hybrid classical + post-quantum file encryption using libsodium and liboqs, with SPARK formal verification.

## Quick Start

```bash
git clone https://github.com/AnubisQuantumCipher/anubis-spark
cd anubis-spark
./bootstrap
./bin/anubis_main version
```

That's it. The bootstrap script installs everything locally (nothing touches your system directories):
- Ada compiler and build tools
- libsodium 1.0.20 and liboqs 0.14.0
- Builds and tests the project

### Prerequisites

You only need basic build tools:

```bash
# Ubuntu/Debian
sudo apt-get install -y build-essential cmake libssl-dev wget unzip

# macOS
xcode-select --install
brew install cmake openssl wget
```

## What Is This?

ANUBIS-SPARK provides:

- **Classical crypto** via libsodium (X25519, Ed25519, XChaCha20-Poly1305, Argon2id)
- **Post-quantum crypto** via liboqs (ML-KEM-1024, ML-DSA-87 per NIST FIPS 203/204)
- **Hybrid key exchange and signatures** combining both approaches
- **File encryption** with streaming AEAD (handles files of any size)
- **Passphrase-based key derivation** using memory-hard Argon2id

## Architecture

```
+----------------------------------------------------------+
|  CLI Interface (anubis-spark)                            |
+----------------------------------------------------------+
|  Ada Wrapper Layer (SPARK verified)                      |
|   +- Key management                                      |
|   +- File encryption/decryption orchestration            |
|   +- Keystore operations                                 |
+----------------------------------------------------------+
|  Ada FFI Bindings                                        |
|   +- libsodium bindings (classical crypto)               |
|   +- liboqs bindings (post-quantum crypto)               |
+----------------------------------------------------------+
|  C Libraries (actual crypto implementations)             |
|   +- libsodium 1.0.20                                    |
|   +- liboqs 0.14.0                                       |
+----------------------------------------------------------+
```

## Installation Options

### Option 1: Bootstrap (Recommended)

Everything installs locally in the repo - no system modifications:

```bash
git clone https://github.com/AnubisQuantumCipher/anubis-spark
cd anubis-spark
./bootstrap
```

The bootstrap creates:
- `.tools/` - Alire package manager and Ada toolchain
- `.deps/` - libsodium and liboqs static libraries
- `bin/anubis_main` - The built binary

### Option 2: Binary Releases

Download pre-compiled binaries (no build required):

**Linux x86_64:**
```bash
wget https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-linux-x86_64.tar.gz
tar xzf anubis-spark-linux-x86_64.tar.gz
sudo install -m 755 anubis-spark-linux-x86_64/anubis_main /usr/local/bin/anubis-spark
```

**macOS:**
```bash
curl -LO https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-macos-universal.tar.gz
tar xzf anubis-spark-macos-universal.tar.gz
sudo install -m 755 anubis-spark-macos-universal/anubis_main /usr/local/bin/anubis-spark
```

### Option 3: Dev Container (VS Code)

Open in VS Code and select "Reopen in Container" - everything is pre-configured.

### Option 4: Docker

```bash
docker build -t anubis-spark .
docker run -it anubis-spark ./bin/anubis_main version
```

## Usage

### Generate Keys
```bash
./bin/anubis_main keygen --output my_identity.key
# Creates hybrid keypair (classical + post-quantum)
```

### Encrypt File
```bash
./bin/anubis_main encrypt --key my_identity.key --input document.pdf
# Creates: document.pdf.anubis
```

### Decrypt File
```bash
./bin/anubis_main decrypt --key my_identity.key --input document.pdf.anubis
# Creates: document.pdf.anubis.decrypted
```

### Self-Test
```bash
./bin/anubis_main test
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

## SPARK Verification

ANUBIS-SPARK includes formal verification of critical Ada code:

```bash
make prove-fast    # Quick verification (~5 min)
make prove-full    # Full verification (~20 min)
```

**Verification Status:**
- Stone: Valid SPARK subset
- Bronze: Flow analysis (no uninitialized vars)
- Silver: Absence of Runtime Errors
- Gold: Integrity properties (31/31 proofs)
- Platinum: Functional correctness (151/151 proofs)

## Makefile Targets

```bash
make help          # Show all targets

# Bootstrap
make bootstrap     # Full setup (same as ./bootstrap)
make bootstrap-env # Setup without building

# Build
make build         # Build release binary
make test          # Run self-tests
make install       # Install to ~/.local/bin

# SPARK
make prove-fast    # Quick proofs
make prove-full    # Full proofs
make boundary      # Boundary tests

# Cleanup
make clean         # Remove build artifacts
make clean-all     # Remove everything (tools, deps)
```

## Troubleshooting

**"./bootstrap: Permission denied"**
```bash
chmod +x bootstrap
```

**"cmake not found" or "OpenSSL not found"**
```bash
# Ubuntu/Debian
sudo apt-get install -y build-essential cmake libssl-dev wget unzip

# macOS
brew install cmake openssl wget
```

**"No toolchain selected"**
```bash
# The bootstrap should handle this, but if needed:
source ./env.sh
alr toolchain --select
```

**Rebuild from scratch**
```bash
make clean-all
./bootstrap
```

## Performance

Tested on Apple Silicon (M-series):

| File Size | Encrypt | Decrypt | Throughput |
|-----------|---------|---------|------------|
| 66 MB | 3.1s | 4.2s | ~20 MB/s |
| 2 GB | 62s | 117s | ~30 MB/s |

Argon2id dominates encryption time (~2-3s for key derivation).

## Security Considerations

**What This Provides:**
- Hybrid crypto (break both classical AND PQC to decrypt)
- Memory-hard KDF resists GPU/ASIC attacks
- Streaming encryption supports large files
- NIST-standardized PQC algorithms (FIPS 203/204)

**What to Know:**
- Security depends on libsodium and liboqs implementations
- Ada wrapper adds interface validation but doesn't replace crypto library audits
- Passphrase strength critical (Argon2id slows brute-force but can't fix weak passwords)

## Project Structure

```
anubis-spark/
├── bootstrap           # One-command setup script
├── Makefile            # Build targets
├── anubis_spark.gpr    # GNAT project file
├── src/                # Ada source code
│   └── crypto/         # Cryptographic implementations
├── tests/              # Test suite
├── scripts/            # Helper scripts
│   ├── bootstrap.sh    # Full bootstrap implementation
│   └── install-deps.sh # Dependency builder
├── .devcontainer/      # VS Code dev container
└── Dockerfile          # Docker build
```

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

---

**Version**: v2.0.8 | **Dependencies**: libsodium 1.0.20+, liboqs 0.14.0+ | **Compiler**: GNAT 14.2.1+
