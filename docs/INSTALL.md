# Installation Guide

Complete installation instructions for ANUBIS-SPARK on all supported platforms.

## Table of Contents

- [System Requirements](#system-requirements)
- [macOS Installation](#macos-installation)
- [Linux Installation](#linux-installation)
- [Windows Installation](#windows-installation)
- [Verifying Installation](#verifying-installation)
- [Troubleshooting](#troubleshooting)

---

## System Requirements

### Minimum Requirements
- **CPU**: x86_64 or ARM64 (Apple Silicon M1/M2/M3)
- **RAM**: 2 GB minimum, 4 GB recommended
- **Disk**: 500 MB for dependencies + build artifacts
- **OS**: macOS 11+, Linux (glibc 2.28+), Windows 10+

### Required Software
- **GNAT Ada Compiler**: 13.1 or later (14.2.1 recommended)
- **GPRbuild**: 24.0.1 or later
- **Alire**: 2.0+ (Ada package manager)
- **liboqs**: 0.14.0 (Open Quantum Safe library)
- **OpenSSL**: 3.0+ (dependency of liboqs)

---

## macOS Installation

### Step 1: Install Homebrew (if not already installed)

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

### Step 2: Install liboqs and OpenSSL

```bash
# Install liboqs (includes OpenSSL as dependency)
brew install liboqs

# Verify installation
brew list liboqs
brew list openssl@3

# Check library locations
ls -la /opt/homebrew/lib/liboqs.a
ls -la /opt/homebrew/lib/libssl.dylib
ls -la /opt/homebrew/lib/libcrypto.dylib
```

### Step 3: Install Alire (Ada Package Manager)

```bash
# Download latest Alire release for macOS
curl -L https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-aarch64-macos.zip -o alire.zip

# For Intel Macs, use:
# curl -L https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-x86_64-macos.zip -o alire.zip

# Extract and install
unzip alire.zip
mkdir -p ~/.local/bin
mv bin/alr ~/.local/bin/

# Add to PATH (add to ~/.zshrc or ~/.bash_profile)
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

# Verify installation
alr version
```

### Step 4: Install GNAT Toolchain via Alire

```bash
# Alire will automatically install GNAT and GPRbuild
cd /tmp
alr init --bin test_project
cd test_project
alr build  # This triggers toolchain installation

# Verify toolchain installation
ls ~/.local/share/alire/toolchains/
# Should see: gnat_native_14.2.1_* and gprbuild_24.0.1_*
```

### Step 5: Clone and Build ANUBIS-SPARK

```bash
# Clone the repository
cd ~/Desktop
git clone https://github.com/AnubisQuantumCipher/anubis-spark.git
cd anubis-spark

# Build the project
alr exec -- gprbuild -P anubis_spark.gpr

# Verify build
ls -la bin/
# Should see: anubis_main, test_pqc, test_minimal
```

### Step 6: Run Tests

```bash
# Smoke test (liboqs initialization)
./bin/test_minimal

# Expected output:
# Starting minimal test...
# Calling OQS_init...
# SUCCESS: liboqs initialized!
# Calling OQS_destroy...
# SUCCESS: Test complete!

# Full test suite
./bin/test_pqc

# Expected output:
# ========================================
# ANUBIS-SPARK Post-Quantum Cryptography Test Suite
# ========================================
#
# [ML-KEM-1024 Tests]
# ✓ Keypair generation
# ✓ Encapsulation
# ✓ Decapsulation
# ✓ Shared secret matching
# ✓ Constant-time comparison
# ✓ Secure zeroization
#
# [ML-DSA-87 Tests]
# ✓ Keypair generation
# ✓ Message signing
# ✓ Signature verification
# ✓ Invalid signature rejection
# ✓ Secure key zeroization
#
# All tests PASSED! ✅
```

### Step 7: Fix RPATH Issues (if needed)

If you encounter `dyld: duplicate LC_RPATH` errors:

```bash
# Check current RPATHs
otool -l bin/test_pqc | grep -A2 LC_RPATH

# Remove duplicate RPATH
install_name_tool -delete_rpath '/Users/YOUR_USERNAME/.local/share/alire/toolchains/gnat_native_14.2.1_*/lib' bin/test_pqc

# Verify fix
./bin/test_pqc
```

---

## Linux Installation

### Ubuntu/Debian

#### Step 1: Install System Dependencies

```bash
sudo apt update
sudo apt install -y \
    build-essential \
    git \
    curl \
    cmake \
    ninja-build \
    libssl-dev \
    pkg-config
```

#### Step 2: Install liboqs from Source

```bash
# Clone liboqs
cd /tmp
git clone --depth 1 --branch 0.14.0 https://github.com/open-quantum-safe/liboqs.git
cd liboqs

# Build and install
mkdir build && cd build
cmake -GNinja \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DBUILD_SHARED_LIBS=OFF \
    -DOQS_USE_OPENSSL=ON \
    ..
ninja
sudo ninja install

# Verify installation
ls -la /usr/local/lib/liboqs.a
```

#### Step 3: Install Alire

```bash
# Download Alire for Linux
curl -L https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-x86_64-linux.zip -o alire.zip

# For ARM64 Linux:
# curl -L https://github.com/alire-project/alire/releases/download/v2.0.2/alr-2.0.2-bin-aarch64-linux.zip -o alire.zip

unzip alire.zip
mkdir -p ~/.local/bin
mv bin/alr ~/.local/bin/

# Add to PATH
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

alr version
```

#### Step 4: Install GNAT Toolchain

```bash
cd /tmp
alr init --bin test_project
cd test_project
alr build  # Installs toolchain
```

#### Step 5: Build ANUBIS-SPARK

```bash
cd ~/Desktop
git clone https://github.com/AnubisQuantumCipher/anubis-spark.git
cd anubis-spark

# Update library paths in anubis_spark.gpr if needed
# Change /opt/homebrew/lib to /usr/local/lib

alr exec -- gprbuild -P anubis_spark.gpr
```

#### Step 6: Run Tests

```bash
./bin/test_minimal
./bin/test_pqc
```

### Fedora/RHEL/CentOS

```bash
# Install dependencies
sudo dnf install -y gcc g++ git curl cmake ninja-build openssl-devel

# Follow same steps as Ubuntu, starting from Step 2
```

### Arch Linux

```bash
# Install from AUR
yay -S liboqs

# Or build from source (follow Ubuntu Step 2)

# Install other dependencies
sudo pacman -S base-devel git curl

# Follow same steps as Ubuntu, starting from Step 3
```

---

## Windows Installation (WSL2 Recommended)

### Option 1: WSL2 (Recommended)

```powershell
# Install WSL2 (PowerShell as Administrator)
wsl --install -d Ubuntu-24.04

# Restart computer, then launch Ubuntu from Start Menu

# Inside WSL2, follow Linux (Ubuntu) installation steps
```

### Option 2: Native Windows (Advanced)

**Requirements:**
- Visual Studio 2022 Build Tools
- CMake
- MSYS2 or Cygwin
- GNAT Community Edition

This is more complex and not fully tested. WSL2 is strongly recommended.

---

## Verifying Installation

### Check All Components

```bash
# Check Alire
alr version
# Expected: Alire 2.0.2 or later

# Check GNAT
gnat --version
# Expected: GNAT 14.2.1 or later

# Check GPRbuild
gprbuild --version
# Expected: GPRbuild 24.0.1 or later

# Check liboqs (macOS)
ls -la /opt/homebrew/lib/liboqs.a
# Or Linux:
ls -la /usr/local/lib/liboqs.a

# Check ANUBIS-SPARK build
cd ~/Desktop/anubis-spark
./bin/test_minimal && ./bin/test_pqc
```

### Expected Test Output

**test_minimal:**
```
Starting minimal test...
Calling OQS_init...
SUCCESS: liboqs initialized!
Calling OQS_destroy...
SUCCESS: Test complete!
```

**test_pqc:**
```
========================================
ANUBIS-SPARK Post-Quantum Cryptography Test Suite
========================================

Testing ML-KEM-1024 (NIST Level 5)...
✓ Keypair generation successful
✓ Encapsulation successful
✓ Decapsulation successful
✓ Shared secrets match
✓ Secure zeroization verified

Testing ML-DSA-87 (NIST Level 5)...
✓ Keypair generation successful
✓ Signature generation successful
✓ Signature verification successful
✓ Invalid signature rejected
✓ Secure key zeroization verified

All tests PASSED! ✅
```

---

## Troubleshooting

### Common Issues

#### 1. `alr: command not found`

**Solution:** Add Alire to PATH
```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc  # or ~/.bashrc
source ~/.zshrc
```

#### 2. `can't find a toolchain for building` (Alire warning)

**Solution:** Install toolchain via Alire
```bash
cd /tmp
alr init --bin test
cd test
alr build  # Triggers toolchain installation
```

#### 3. `library not found: -loqs` (linker error)

**macOS Solution:**
```bash
# Verify liboqs installation
brew list liboqs
ls -la /opt/homebrew/lib/liboqs.a

# If missing, reinstall
brew reinstall liboqs
```

**Linux Solution:**
```bash
# Check installation
ls -la /usr/local/lib/liboqs.a

# If missing, rebuild from source (see Step 2 above)
```

#### 4. `dyld: duplicate LC_RPATH` (macOS runtime error)

**Solution:**
```bash
# Find duplicate RPATH
otool -l bin/test_pqc | grep -A2 LC_RPATH

# Remove GNAT toolchain RPATH
install_name_tool -delete_rpath '/Users/YOUR_USERNAME/.local/share/alire/toolchains/gnat_native_14.2.1_*/lib' bin/test_pqc
```

#### 5. `use clause for package has no effect` (compilation warning)

**Solution:** This was fixed in the source code. Pull latest version:
```bash
cd ~/Desktop/anubis-spark
git pull origin main
alr exec -- gprbuild -P anubis_spark.gpr
```

#### 6. `permission denied` when running binaries

**Solution:**
```bash
chmod +x bin/*
```

#### 7. OpenSSL version conflicts

**macOS Solution:**
```bash
# Use Homebrew OpenSSL
brew link --force openssl@3
export PKG_CONFIG_PATH="/opt/homebrew/opt/openssl@3/lib/pkgconfig"
```

**Linux Solution:**
```bash
# Install OpenSSL 3.x
sudo apt install libssl3 libssl-dev
```

---

## Building from Different Modes

### Development Mode (default)
```bash
alr exec -- gprbuild -P anubis_spark.gpr -XBUILD_MODE=dev
# Includes debug symbols, stack checking, all warnings
```

### Release Mode (optimized)
```bash
alr exec -- gprbuild -P anubis_spark.gpr -XBUILD_MODE=release
# Full optimizations, no runtime checks (use for production)
```

### SPARK Verification Mode
```bash
alr exec -- gprbuild -P anubis_spark.gpr -XBUILD_MODE=spark
# Enables SPARK verification and assertions
```

---

## Platform-Specific Notes

### macOS Apple Silicon (M1/M2/M3)
- liboqs has native ARM64 support
- Use `aarch64-macos` Alire binary
- Libraries installed at `/opt/homebrew/lib`

### macOS Intel
- Use `x86_64-macos` Alire binary
- Libraries installed at `/usr/local/lib`

### Linux ARM64 (Raspberry Pi, etc.)
- Use `aarch64-linux` Alire binary
- May need to build liboqs with specific CPU optimizations

### WSL2 Performance
- File I/O is faster on Linux filesystem (`/home`) vs Windows mount (`/mnt/c`)
- Clone repository to `~/Desktop`, not `/mnt/c/Users/...`

---

## Next Steps

After successful installation:

1. **Read the documentation:**
   - [Architecture Guide](ARCHITECTURE.md)
   - [Security Analysis](SECURITY.md)
   - [API Reference](API.md)
   - [Key Management](KEY_MANAGEMENT.md)

2. **Explore the code:**
   - `src/crypto/anubis_types.ads` - Type definitions
   - `src/crypto/anubis_types-pqc.ads` - PQC interface
   - `src/test_pqc.adb` - Comprehensive test suite

3. **Run SPARK verification:**
   ```bash
   gnatprove -P anubis_spark.gpr --level=2
   ```

4. **Contribute:**
   - See [CONTRIBUTING.md](CONTRIBUTING.md)

---

## Additional Resources

- **Alire Documentation:** https://alire.ada.dev/docs/
- **GNAT User's Guide:** https://docs.adacore.com/gnat_ugn-docs/
- **liboqs Documentation:** https://github.com/open-quantum-safe/liboqs
- **SPARK Documentation:** https://docs.adacore.com/spark2014-docs/

---

**Installation Support:** If you encounter issues not covered here, please open an issue on GitHub with:
- Your platform (OS, version, architecture)
- Exact error messages
- Output of `alr version`, `gnat --version`, and `ls -la /path/to/liboqs.a`
