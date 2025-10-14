# ANUBIS-SPARK Installation Prerequisites

**Version**: 2.0.7
**Last Updated**: 2025-10-14
**Maintainer**: sic.tau@pm.me
**Repository**: https://github.com/AnubisQuantumCipher/anubis-spark

---

## Overview

This document provides complete installation requirements for building ANUBIS-SPARK from source.

**⚡ Want to skip installation?** Download ready-to-use static binaries with zero dependencies from the [Releases](https://github.com/AnubisQuantumCipher/anubis-spark/releases/tag/v2.0.7) page - see Quick Start below.

---

## Quick Start (Recommended) - No Build Required

### Using Static Binary Releases

**✅ ZERO prerequisites required** - pre-compiled binaries include all dependencies statically linked:
- 100% SPARK Platinum Certification (151/151 VCs proven)
- liboqs 0.14.0 (final FIPS 203/204 ML-KEM/ML-DSA)
- libsodium 1.0.20 (classical cryptography)
- ~400 KB binary size
- Works on any compatible system immediately

**Latest Release**: [v2.0.7](https://github.com/AnubisQuantumCipher/anubis-spark/releases/tag/v2.0.7)

---

#### Linux x86_64 (Intel/AMD servers, cloud instances)

```bash
# Download latest release (always up-to-date)
wget https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-linux-x86_64.tar.gz

# Download checksum for verification
wget https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-linux-x86_64.tar.gz.sha256

# Verify integrity
sha256sum -c anubis-spark-linux-x86_64.tar.gz.sha256
# Expected: anubis-spark-linux-x86_64.tar.gz: OK

# Extract and run
tar xzf anubis-spark-linux-x86_64.tar.gz
cd anubis-spark-linux-x86_64
./anubis_main version

# System-wide installation (optional)
sudo install -m 755 anubis_main /usr/local/bin/anubis-spark
anubis-spark version
```

**Supported Distributions**: Ubuntu 20.04+, Debian 11+, RHEL 8+, CentOS 8+, Fedora 35+, Arch, Alpine 3.15+

---

#### macOS Apple Silicon (M1/M2/M3/M4)

```bash
# Download latest release (always up-to-date)
curl -LO https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-macos-arm64.tar.gz

# Download checksum for verification
curl -LO https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-macos-arm64.tar.gz.sha256

# Verify integrity
shasum -a 256 -c anubis-spark-macos-arm64.tar.gz.sha256
# Expected: anubis-spark-macos-arm64.tar.gz: OK

# Extract and run
tar xzf anubis-spark-macos-arm64.tar.gz
cd anubis-spark-macos-arm64
./anubis_main version

# System-wide installation (optional)
sudo install -m 755 anubis_main /usr/local/bin/anubis-spark
anubis-spark version
```

**macOS Version**: Requires macOS 11.0 (Big Sur) or later

---

#### macOS Intel (x86_64)

```bash
# Download latest release (always up-to-date)
curl -LO https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-macos-x86_64.tar.gz

# Download checksum for verification
curl -LO https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-macos-x86_64.tar.gz.sha256

# Verify integrity
shasum -a 256 -c anubis-spark-macos-x86_64.tar.gz.sha256
# Expected: anubis-spark-macos-x86_64.tar.gz: OK

# Extract and run
tar xzf anubis-spark-macos-x86_64.tar.gz
cd anubis-spark-macos-x86_64
./anubis_main version

# System-wide installation (optional)
sudo install -m 755 anubis_main /usr/local/bin/anubis-spark
anubis-spark version
```

**macOS Version**: Requires macOS 10.15 (Catalina) or later

---

#### Quick Test After Installation

```bash
# Generate identity key
anubis-spark keygen --output test.key

# Encrypt sample file
echo "Hello, Post-Quantum World!" > test.txt
anubis-spark encrypt --key test.key --input test.txt

# Decrypt file
anubis-spark decrypt --key test.key --input test.txt.anubis

# Verify output
cat test.txt.anubis.decrypted
# Expected: Hello, Post-Quantum World!

# Run self-tests
anubis-spark test
```

---

### Platform Support Summary

| Platform | Architecture | Binary Available | Build from Source |
|----------|-------------|------------------|-------------------|
| Linux | x86_64 (Intel/AMD) | ✅ YES | ✅ YES |
| Linux | ARM64 (Raspberry Pi, etc.) | ❌ Not yet | ✅ YES |
| macOS | Apple Silicon (M1/M2/M3/M4) | ✅ YES | ✅ YES |
| macOS | Intel (x86_64) | ✅ YES | ✅ YES |
| Windows | WSL2 (Ubuntu) | ✅ Use Linux binary | ✅ YES |
| FreeBSD | x86_64 | ❌ Not yet | ⚠️ Experimental |

**Note**: ARM64 Linux builds are not available in binary form due to Ada toolchain limitations. Linux ARM64 users must build from source (see instructions below).

### Using Docker

**✅ ZERO local installation required** - run directly in container with all dependencies included:
- Docker image: `ghcr.io/anubisquantumcipher/anubis-spark:latest`
- Multi-architecture support: linux/amd64
- Size: ~250 MB (runtime), includes full verification
- Based on Ubuntu 22.04

```bash
# Pull latest image from GitHub Container Registry
docker pull ghcr.io/anubisquantumcipher/anubis-spark:latest

# Show version and verify SPARK certification
docker run --rm ghcr.io/anubisquantumcipher/anubis-spark:latest version
# Expected: ANUBIS-SPARK v2.0.7, SPARK Platinum (151/151 VCs), liboqs 0.14.0, libsodium 1.0.20

# Generate identity key (mount current directory as /data)
docker run --rm -v $(pwd):/data ghcr.io/anubisquantumcipher/anubis-spark:latest \
  keygen --output /data/identity.key

# Encrypt file
echo "Hello, Post-Quantum World!" > test.txt
docker run --rm -v $(pwd):/data ghcr.io/anubisquantumcipher/anubis-spark:latest \
  encrypt --key /data/identity.key --input /data/test.txt

# Decrypt file
docker run --rm -v $(pwd):/data ghcr.io/anubisquantumcipher/anubis-spark:latest \
  decrypt --key /data/identity.key --input /data/test.txt.anubis

# Verify result
cat test.txt.anubis.decrypted
# Expected: Hello, Post-Quantum World!

# Run cryptographic self-tests
docker run --rm ghcr.io/anubisquantumcipher/anubis-spark:latest test
```

**Docker Version Tags**:
- `latest` - Always points to most recent stable release (v2.0.7)
- `2.0.7` - Specific version tag
- `2.0` - Latest 2.0.x release
- `2` - Latest 2.x release

---

## Building from Source

### System Requirements

**Operating Systems**:
- Linux (x86_64, ARM64) - Ubuntu 20.04+, Debian 11+, RHEL 8+
- macOS (Intel, Apple Silicon) - macOS 11.0+ (Big Sur or later)
- FreeBSD 13+ (experimental)
- Windows WSL2 (Ubuntu 22.04 recommended)

**Hardware**:
- CPU: x86_64 (Intel/AMD) or ARM64
- RAM: Minimum 4 GB, recommended 8 GB
- Disk: 2 GB free space for build artifacts

**Build Time**:
- Clean build: 5-10 minutes (depending on CPU)
- Incremental rebuild: 1-2 minutes

---

## Core Dependencies

### 1. Ada Toolchain (via Alire)

**Alire** is the Ada package manager that automatically installs the complete Ada toolchain.

**Installation**:

**Linux/macOS**:
```bash
curl -fsSL https://alire.ada.dev/install.sh | sh -s -- -y
export PATH="$HOME/.alire/bin:$PATH"
alr --version
```

**Manual Installation**:
```bash
# Download Alire for your platform from:
# https://github.com/alire-project/alire/releases/tag/v2.0.1

# Linux x86_64
wget https://github.com/alire-project/alire/releases/download/v2.0.1/alr-2.0.1-bin-x86_64-linux.zip
unzip alr-2.0.1-bin-x86_64-linux.zip
mv bin/alr ~/.local/bin/

# Linux ARM64
wget https://github.com/alire-project/alire/releases/download/v2.0.1/alr-2.0.1-bin-aarch64-linux.zip
unzip alr-2.0.1-bin-aarch64-linux.zip
mv bin/alr ~/.local/bin/

# macOS Intel
wget https://github.com/alire-project/alire/releases/download/v2.0.1/alr-2.0.1-bin-x86_64-macos.zip
unzip alr-2.0.1-bin-x86_64-macos.zip
mv bin/alr ~/.local/bin/

# macOS Apple Silicon
wget https://github.com/alire-project/alire/releases/download/v2.0.1/alr-2.0.1-bin-aarch64-macos.zip
unzip alr-2.0.1-bin-aarch64-macos.zip
mv bin/alr ~/.local/bin/
```

**Components Installed by Alire**:
- GNAT 14.2.1 (Ada compiler based on GCC)
- GPRbuild 24.0.1 (Ada build system)
- GNATprove 14.1.1 (SPARK formal verification - optional)

**Verification**:
```bash
alr --version
# Expected: alr 2.0.1

# Alire will auto-install toolchain on first build
cd anubis-spark
alr build
```

### 2. liboqs (Post-Quantum Cryptography)

**Version Required**: 0.14.0 or later (final FIPS 203/204 ML-KEM/ML-DSA)

liboqs provides ML-KEM-1024 and ML-DSA-87 implementations.

**IMPORTANT**: ANUBIS-SPARK v2.0.7 requires liboqs 0.14.0 which includes the final NIST FIPS 203/204 standardized function names (not the IPD versions from 0.10.1).

#### Linux (Ubuntu/Debian)

```bash
# Build from source (apt version may be outdated)
sudo apt install build-essential cmake ninja-build libssl-dev
wget https://github.com/open-quantum-safe/liboqs/archive/refs/tags/0.14.0.tar.gz
tar xzf 0.14.0.tar.gz
cd liboqs-0.14.0
mkdir build && cd build
cmake -GNinja \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_SHARED_LIBS=ON \
  -DCMAKE_INSTALL_PREFIX=/usr/local \
  -DOQS_USE_OPENSSL=ON \
  ..
ninja
sudo ninja install
sudo ldconfig
```

#### Linux (RHEL/CentOS/Fedora)

```bash
sudo dnf groupinstall "Development Tools"
sudo dnf install cmake ninja-build openssl-devel
wget https://github.com/open-quantum-safe/liboqs/archive/refs/tags/0.14.0.tar.gz
tar xzf 0.14.0.tar.gz
cd liboqs-0.14.0
mkdir build && cd build
cmake -GNinja \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_SHARED_LIBS=ON \
  -DCMAKE_INSTALL_PREFIX=/usr/local \
  -DOQS_USE_OPENSSL=ON \
  ..
ninja
sudo ninja install
sudo ldconfig
```

#### macOS (Homebrew)

```bash
brew install liboqs
# Note: Verify version is 0.14.0 or later
brew info liboqs | grep -E "liboqs:"
```

#### macOS (From Source)

```bash
brew install cmake ninja openssl@3
wget https://github.com/open-quantum-safe/liboqs/archive/refs/tags/0.14.0.tar.gz
tar xzf 0.14.0.tar.gz
cd liboqs-0.14.0
mkdir build && cd build
cmake -GNinja \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_SHARED_LIBS=ON \
  -DCMAKE_INSTALL_PREFIX=/opt/homebrew \
  -DOQS_USE_OPENSSL=ON \
  ..
ninja
sudo ninja install
```

**Verification**:
```bash
pkg-config --modversion liboqs
# Expected: 0.14.0 or later

ls /usr/local/lib/liboqs.* || ls /opt/homebrew/lib/liboqs.*
# Should show liboqs.a and/or liboqs.dylib/liboqs.so
```

### 3. libsodium (Classical Cryptography)

**Version Required**: 1.0.20 or later

libsodium provides X25519, Ed25519, XChaCha20-Poly1305, Argon2id.

#### Linux (Ubuntu/Debian)

```bash
# Option 1: From apt
sudo apt update
sudo apt install libsodium-dev

# Option 2: Build from source
sudo apt install build-essential
wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.20.tar.gz
tar xzf libsodium-1.0.20.tar.gz
cd libsodium-stable
./configure --prefix=/usr/local
make -j$(nproc)
make check
sudo make install
sudo ldconfig
```

#### Linux (RHEL/CentOS/Fedora)

```bash
sudo dnf install libsodium-devel

# Or from source:
wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.20.tar.gz
tar xzf libsodium-1.0.20.tar.gz
cd libsodium-stable
./configure --prefix=/usr/local
make -j$(nproc)
make check
sudo make install
sudo ldconfig
```

#### macOS (Homebrew)

```bash
brew install libsodium
```

#### macOS (From Source)

```bash
wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.20.tar.gz
tar xzf libsodium-1.0.20.tar.gz
cd libsodium-stable
./configure --prefix=/opt/homebrew
make -j$(sysctl -n hw.ncpu)
make check
sudo make install
```

**Verification**:
```bash
pkg-config --modversion libsodium
# Expected: 1.0.20 or later

ls /usr/local/lib/libsodium.* || ls /opt/homebrew/lib/libsodium.*
# Should show libsodium.a and/or libsodium.dylib/libsodium.so
```

---

## Optional Dependencies

### 1. GNATprove (SPARK Verification)

**Required for**: Reproducing formal verification proofs

**Installation via Alire**:
```bash
cd anubis-spark
alr toolchain --install gnatprove=14.1.1
eval "$(alr printenv)"
gnatprove --version
```

**Usage**:
```bash
# Fast proofs (2-3 minutes)
gnatprove -P anubis_spark.gpr --level=1 --prover=cvc5 --timeout=300

# Check results
grep "Total" obj/gnatprove/gnatprove.out
# Expected: Total: 151 ... Unproved: 0
```

### 2. Docker

**Required for**: Building Docker images, running containerized builds

**Linux**:
```bash
# Ubuntu/Debian
sudo apt install docker.io
sudo systemctl start docker
sudo usermod -aG docker $USER
# Log out and log back in

# RHEL/CentOS/Fedora
sudo dnf install docker
sudo systemctl start docker
sudo usermod -aG docker $USER
```

**macOS**:
```bash
brew install --cask docker
# Or download Docker Desktop from docker.com
```

**Verification**:
```bash
docker --version
docker run hello-world
```

### 3. Git

**Required for**: Cloning repository, version control

**Linux**:
```bash
sudo apt install git      # Ubuntu/Debian
sudo dnf install git      # RHEL/Fedora
```

**macOS**:
```bash
brew install git
# Or use Xcode command line tools: xcode-select --install
```

**Verification**:
```bash
git --version
```

---

## Platform-Specific Notes

### Linux

**Static Linking**:
ANUBIS-SPARK links liboqs and libsodium statically by default on Linux. Ensure static libraries are installed:

```bash
# Ubuntu/Debian
sudo apt install liboqs-dev libsodium-dev

# Verify static libraries exist
ls /usr/lib/x86_64-linux-gnu/liboqs.a
ls /usr/lib/x86_64-linux-gnu/libsodium.a
```

**SELinux/AppArmor**:
If running on systems with mandatory access control:
```bash
# SELinux - allow execution
chcon -t bin_t ./bin/anubis_main

# AppArmor - no special configuration needed
```

### macOS

**Apple Silicon vs Intel**:
- Apple Silicon (M1/M2/M3): Use ARM64 binaries
- Intel Macs: Use x86_64 binaries

**Homebrew Paths**:
- Intel Macs: Libraries in `/usr/local`
- Apple Silicon: Libraries in `/opt/homebrew`

**Code Signing** (optional):
```bash
# Sign binary for distribution
codesign -s "Developer ID Application" bin/anubis_main
```

**Gatekeeper**:
If running downloaded binary:
```bash
# Remove quarantine attribute
xattr -d com.apple.quarantine bin/anubis_main
```

### Windows (WSL2)

ANUBIS-SPARK runs on Windows via Windows Subsystem for Linux (WSL2):

```powershell
# Install WSL2
wsl --install -d Ubuntu-22.04

# Start WSL2
wsl

# Inside WSL2, follow Linux (Ubuntu) instructions above
```

**Native Windows**: Not currently supported (Ada toolchain limitations).

---

## Build Instructions

### Standard Build

```bash
# Clone repository
git clone https://github.com/AnubisQuantumCipher/anubis-spark
cd anubis-spark

# Install Ada toolchain (first time only)
curl -fsSL https://alire.ada.dev/install.sh | sh -s -- -y
export PATH="$HOME/.alire/bin:$PATH"

# Build
alr build --release

# Verify
./bin/anubis_main version
./bin/anubis_main test
```

### Using Makefile

```bash
# Build release binary
make build

# Install to ~/.local/bin
make install

# Run tests
make test

# Run SPARK proofs (requires GNATprove)
make prove-fast

# Clean build artifacts
make clean
```

### Docker Build

```bash
# Build Docker image
docker build -t anubis-spark:2.0.0 .

# Run proofs in container
docker run --rm anubis-spark:2.0.0 bash -c \
  "eval \"\$(alr printenv)\" && make prove-fast"

# Extract binary
docker create --name anubis-temp anubis-spark:2.0.0
docker cp anubis-temp:/home/anubis/anubis-spark/bin/anubis_main ./
docker rm anubis-temp
```

---

## Troubleshooting

### Issue: alr command not found

**Solution**:
```bash
export PATH="$HOME/.alire/bin:$PATH"
echo 'export PATH="$HOME/.alire/bin:$PATH"' >> ~/.bashrc
# Or ~/.zshrc on macOS
```

### Issue: liboqs not found

**Linux**:
```bash
sudo ldconfig
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
```

**macOS**:
```bash
export PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig:$PKG_CONFIG_PATH
```

### Issue: libsodium not found

**Linux**:
```bash
sudo ldconfig
pkg-config --libs libsodium
```

**macOS**:
```bash
brew link libsodium
pkg-config --libs libsodium
```

### Issue: GNAT compiler errors

**Solution**: Ensure Alire installed correct toolchain:
```bash
alr toolchain --select gnat_native=14.2.1 gprbuild=24.0.1
eval "$(alr printenv)"
gnatls --version
```

### Issue: Build errors on macOS 15.4+ (Sequoia)

**Solution**: Duplicate RPATH issue (see fix-rpath.sh):
```bash
make build
./scripts/fix-rpath.sh
```

### Issue: Out of memory during build

**Solution**: Reduce parallel jobs:
```bash
alr build --release -- -j1
```

---

## Dependency Version Matrix

| Component | Minimum Version | Tested Version | Release v2.0.7 | Notes |
|-----------|----------------|----------------|----------------|-------|
| **Alire** | 2.0.0 | 2.0.1 | 2.0.1 | Ada package manager |
| **GNAT** | 14.0.0 | 14.2.1 | 14.2.1 | Ada compiler (via Alire) |
| **GPRbuild** | 24.0.0 | 24.0.1 | 24.0.1 | Build system (via Alire) |
| **GNATprove** | 14.0.0 | 14.1.1 | 14.1.1 | Optional, for proofs |
| **liboqs** | 0.14.0 | 0.14.0 | 0.14.0 | **REQUIRED** - final FIPS 203/204 |
| **libsodium** | 1.0.18 | 1.0.20 | 1.0.20 | Classical crypto |
| **CMake** | 3.16 | 3.27 | 3.27 | For building liboqs |
| **Ninja** | 1.10 | 1.11 | 1.11 | For building liboqs |
| **Docker** | 20.10 | 24.0 | 24.0 | Optional, for containers |

**Breaking Change Alert**: liboqs 0.14.0 is REQUIRED for v2.0.7. Earlier versions (0.10.x, 0.11.x) used IPD (initial public draft) function names and are not compatible with the final NIST FIPS 203/204 standards.

---

## Binary Release Verification

All binary releases include SHA256 checksums for integrity verification:

### Linux x86_64

```bash
# Download binary and checksum (always latest)
wget https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-linux-x86_64.tar.gz
wget https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-linux-x86_64.tar.gz.sha256

# Verify checksum
sha256sum -c anubis-spark-linux-x86_64.tar.gz.sha256
# Expected output: anubis-spark-linux-x86_64.tar.gz: OK

# Manual verification (alternative)
sha256sum anubis-spark-linux-x86_64.tar.gz
cat anubis-spark-linux-x86_64.tar.gz.sha256
# Compare the two hashes - they should match exactly
```

### macOS (Apple Silicon / Intel)

```bash
# Download binary and checksum (replace arch with arm64 or x86_64)
curl -LO https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-macos-arm64.tar.gz
curl -LO https://github.com/AnubisQuantumCipher/anubis-spark/releases/latest/download/anubis-spark-macos-arm64.tar.gz.sha256

# Verify checksum
shasum -a 256 -c anubis-spark-macos-arm64.tar.gz.sha256
# Expected output: anubis-spark-macos-arm64.tar.gz: OK

# Manual verification (alternative)
shasum -a 256 anubis-spark-macos-arm64.tar.gz
cat anubis-spark-macos-arm64.tar.gz.sha256
# Compare the two hashes - they should match exactly
```

### Verifying Binary Authenticity

**After extraction, verify the binary:**

```bash
# Extract archive
tar xzf anubis-spark-*.tar.gz

# Check binary info
file anubis-spark-*/anubis_main
# Linux: should show "ELF 64-bit LSB executable, x86-64"
# macOS: should show "Mach-O 64-bit executable arm64/x86_64"

# Run version check
./anubis-spark-*/anubis_main version
# Expected: ANUBIS-SPARK v2.0.7
#           SPARK Platinum Certification: 151/151 VCs proven
#           liboqs 0.14.0 (ML-KEM-1024, ML-DSA-87)
#           libsodium 1.0.20 (X25519, Ed25519, XChaCha20-Poly1305, Argon2id)

# Run self-tests
./anubis-spark-*/anubis_main test
# Expected: All tests PASSED
```

---

## Security Considerations

### Compiler Security

ANUBIS-SPARK uses GNAT (GCC Ada frontend) which benefits from GCC security features:
- Stack canaries (enabled by default)
- Position-independent executables (PIE)
- RELRO (Relocation Read-Only)

### Supply Chain Security

All dependencies are pinned and verified:
- Alire: SHA256 checksums for all packages
- liboqs: Downloaded from official GitHub releases
- libsodium: Downloaded from official website with SHA256 verification

See [third_party/LOCKFILE.md](third_party/LOCKFILE.md) for complete dependency manifest.

### Reproducible Builds

ANUBIS-SPARK supports reproducible builds:
```bash
# Build twice with same inputs
docker build -t anubis-spark:build1 .
docker build -t anubis-spark:build2 .

# Extract binaries
docker create --name b1 anubis-spark:build1
docker cp b1:/home/anubis/anubis-spark/bin/anubis_main ./anubis_main_1
docker rm b1

docker create --name b2 anubis-spark:build2
docker cp b2:/home/anubis/anubis-spark/bin/anubis_main ./anubis_main_2
docker rm b2

# Verify identical
sha256sum anubis_main_1 anubis_main_2
# Both should have identical SHA256 hashes
```

---

## Support

**Issues**: https://github.com/AnubisQuantumCipher/anubis-spark/issues
**Email**: sic.tau@pm.me
**Documentation**: https://github.com/AnubisQuantumCipher/anubis-spark/tree/main/docs

---

**Document Version**: 1.0
**License**: MIT OR Apache-2.0
