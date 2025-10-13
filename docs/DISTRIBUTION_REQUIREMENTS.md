# Anubis-SPARK Distribution Requirements

## Binary Dependencies Analysis

The `anubis_main` binary is **NOT fully standalone**. It requires external libraries to be installed on the target system.

### Current Binary Status

**Binary Size:** 1.2 MB
**Platform:** macOS ARM64 (Apple Silicon)
**Format:** Mach-O 64-bit executable

### Dependency Breakdown

#### ✅ Statically Linked (Included in Binary)
- **liboqs 0.14.0** - Post-quantum cryptography library (ML-KEM-1024, ML-DSA-87)
- **GNAT Runtime** - Ada runtime libraries

#### ❌ Dynamically Linked (Must Be Installed)
```
/opt/homebrew/opt/libsodium/lib/libsodium.26.dylib
/opt/homebrew/opt/openssl@3/lib/libssl.3.dylib
/opt/homebrew/opt/openssl@3/lib/libcrypto.3.dylib
/usr/lib/libSystem.B.dylib (macOS system library - always present)
```

**Required External Libraries:**
1. **libsodium** - For XChaCha20-Poly1305, X25519, Ed25519, Argon2id
2. **OpenSSL 3** - For HKDF-SHA256 and additional cryptographic primitives

---

## GitHub Distribution Options

### Option 1: Dynamic Binary (Current) - Requires Installation

**Pros:**
- Smaller binary size (1.2 MB)
- Uses system-provided crypto libraries (receives security updates)
- Easier to build

**Cons:**
- ❌ Not instantly runnable after download
- ❌ Requires users to install dependencies
- ❌ Homebrew required on macOS

**Installation Instructions Required:**
```bash
# macOS (Homebrew required)
brew install libsodium openssl@3
./anubis_main version

# Linux (example for Ubuntu/Debian)
sudo apt-get install libsodium23 libssl3
./anubis_main version
```

---

### Option 2: Static Binary (Fully Standalone) - Recommended for GitHub Releases

**Pros:**
- ✅ Instantly runnable after download
- ✅ No dependencies to install
- ✅ Works on any macOS system (with matching architecture)
- ✅ Ideal for GitHub Releases

**Cons:**
- Larger binary size (~8-12 MB estimated)
- Crypto libraries frozen at build-time (no automatic security updates)
- More complex build process

**Build Command:**
```bash
# Add static linking flags to GPR project file
gprbuild -p -XBUILD_MODE=release \
  -largs -static-libgcc -static-libstdc++ \
  /opt/homebrew/lib/libsodium.a \
  /opt/homebrew/lib/libssl.a \
  /opt/homebrew/lib/libcrypto.a
```

---

### Option 3: Bundled Libraries (App Bundle) - macOS Specific

**Pros:**
- ✅ Instantly runnable
- ✅ Libraries can be updated independently
- ✅ Standard macOS distribution pattern

**Cons:**
- macOS-specific solution
- Requires proper code signing
- More complex packaging

**Structure:**
```
anubis-spark.app/
├── Contents/
│   ├── MacOS/
│   │   └── anubis_main (binary)
│   ├── Frameworks/
│   │   ├── libsodium.26.dylib
│   │   ├── libssl.3.dylib
│   │   └── libcrypto.3.dylib
│   └── Info.plist
```

---

### Option 4: Docker Container - Cross-Platform

**Pros:**
- ✅ Instantly runnable on any platform with Docker
- ✅ All dependencies included
- ✅ Reproducible environment
- ✅ Easy CI/CD integration

**Cons:**
- Requires Docker installation
- Larger download size (~50-100 MB)
- Overhead for simple CLI usage

**Dockerfile Example:**
```dockerfile
FROM ubuntu:22.04
RUN apt-get update && apt-get install -y libsodium23 libssl3
COPY bin/anubis_main /usr/local/bin/
ENTRYPOINT ["anubis_main"]
```

---

## Recommended GitHub Release Strategy

### For Immediate Download & Run (Recommended)

**Static Binaries** for multiple platforms:
```
Releases/
├── anubis-spark-v2.0.0-macos-arm64 (static, 10 MB)
├── anubis-spark-v2.0.0-macos-x86_64 (static, 10 MB)
├── anubis-spark-v2.0.0-linux-x86_64 (static, 12 MB)
├── anubis-spark-v2.0.0-linux-arm64 (static, 12 MB)
└── anubis-spark-v2.0.0-windows-x86_64.exe (static, 8 MB)
```

**User Experience:**
```bash
# Download from GitHub
curl -L https://github.com/AnubisQuantumCipher/anubis-spark/releases/download/v2.0.0/anubis-spark-v2.0.0-macos-arm64 -o anubis-spark

# Make executable
chmod +x anubis-spark

# Run immediately (no installation needed!)
./anubis-spark version
```

---

### For Package Manager Distribution

**Dynamic Binaries** with package manager support:

**Homebrew Formula (macOS/Linux):**
```ruby
class AnubisSpark < Formula
  desc "Quantum-resistant file encryption with SPARK verification"
  homepage "https://github.com/AnubisQuantumCipher/anubis-spark"
  url "https://github.com/AnubisQuantumCipher/anubis-spark/releases/download/v2.0.0/anubis-spark-v2.0.0.tar.gz"
  sha256 "..."

  depends_on "libsodium"
  depends_on "openssl@3"

  def install
    bin.install "anubis_main" => "anubis-spark"
  end
end
```

**Installation:**
```bash
brew install anubis-spark
```

---

## Current Binary Limitations

### ❌ Not Portable As-Is

The current binary **will not work** if uploaded to GitHub and downloaded by users because:

1. **Hardcoded Homebrew Paths:**
   ```
   /opt/homebrew/opt/libsodium/lib/libsodium.26.dylib
   /opt/homebrew/opt/openssl@3/lib/libssl.3.dylib
   ```
   These paths won't exist on user systems without Homebrew.

2. **ARM64 Architecture:**
   Only works on Apple Silicon Macs (M1/M2/M3)
   Intel Macs and other platforms cannot run this binary.

3. **Missing Dependencies:**
   Users must install libsodium and OpenSSL 3 before running.

### ✅ Works If Users Have:
- macOS with Apple Silicon (M1/M2/M3)
- Homebrew installed
- `brew install libsodium openssl@3` executed

---

## Action Items for GitHub Release Readiness

### High Priority (Standalone Binaries)

1. **Build Static macOS Binary:**
   ```bash
   # Modify anubis_spark.gpr to use static linking
   gprbuild -XBUILD_MODE=release -XSTATIC=true
   ```

2. **Build Linux Static Binary:**
   ```bash
   # Cross-compile or build on Linux
   docker run --rm -v $(pwd):/work ubuntu:22.04 bash -c \
     "apt-get update && apt-get install -y gnat gprbuild libsodium-dev libssl-dev && \
      cd /work && make static"
   ```

3. **Build Windows Binary:**
   ```bash
   # Use MinGW or MSVC toolchain
   gprbuild -XBUILD_MODE=release --target=x86_64-w64-mingw32
   ```

4. **Test on Fresh Systems:**
   - macOS without Homebrew
   - Clean Linux VM
   - Windows without MSYS2/Cygwin

### Medium Priority (Package Managers)

5. **Create Homebrew Formula** (see example above)
6. **Create Debian Package (.deb)**
7. **Create RPM Package (.rpm)**
8. **Create Arch Linux PKGBUILD**

### Low Priority (Containers)

9. **Create Docker Image** and push to Docker Hub
10. **Create Flatpak** for Linux desktop integration
11. **Create Snap Package** for Ubuntu

---

## Immediate Fix for Current Binary

To make the current binary work on other systems with Homebrew:

```bash
# Fix library paths to use @rpath (relative paths)
install_name_tool -change /opt/homebrew/opt/libsodium/lib/libsodium.26.dylib @rpath/libsodium.26.dylib bin/anubis_main
install_name_tool -change /opt/homebrew/opt/openssl@3/lib/libssl.3.dylib @rpath/libssl.3.dylib bin/anubis_main
install_name_tool -change /opt/homebrew/opt/openssl@3/lib/libcrypto.3.dylib @rpath/libcrypto.3.dylib bin/anubis_main

# Add RPATH for Homebrew locations
install_name_tool -add_rpath /opt/homebrew/lib bin/anubis_main
install_name_tool -add_rpath /usr/local/lib bin/anubis_main
```

**This still requires users to install dependencies, but at least it finds them in standard locations.**

---

## Recommended Next Steps

**For immediate GitHub release:**

1. **Build static binary** (10 minutes of work)
2. **Upload to GitHub Releases** with clear installation instructions
3. **Add SHA256 checksums** for verification
4. **Provide installation script:**
   ```bash
   curl -fsSL https://raw.githubusercontent.com/AnubisQuantumCipher/anubis-spark/main/install.sh | bash
   ```

**For professional distribution:**

1. Set up CI/CD pipeline (GitHub Actions) to build for multiple platforms
2. Automate release artifact creation
3. Create Homebrew tap for easy installation
4. Add to package repositories (Debian, Arch AUR, etc.)

---

## Summary

**Current Status:** ❌ Binary requires installation of libsodium + OpenSSL 3
**Recommended Fix:** ✅ Build static binaries for GitHub Releases
**Time to Fix:** ~30 minutes to 1 hour
**User Experience After Fix:** ✅ Download → Run (no installation needed)

---

**Last Updated:** 2025-10-13
**Binary Version:** v2.0.0
**Platform:** macOS ARM64 (Apple Silicon)
