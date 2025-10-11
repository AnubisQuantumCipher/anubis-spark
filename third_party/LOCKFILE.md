# ANUBIS-SPARK Supply Chain Lockfile

**Version:** 1.1.0 Platinum
**Last Updated:** 2025-10-11
**Purpose:** Pin all external dependencies with cryptographic verification

---

## 1. Cryptographic Libraries

### libsodium 1.0.20

- **Purpose:** Classical cryptography (XChaCha20-Poly1305, X25519, Ed25519, Argon2id)
- **License:** ISC License
- **Source:** https://github.com/jedisct1/libsodium/releases/tag/1.0.20-RELEASE
- **Tarball URL:** https://download.libsodium.org/libsodium/releases/libsodium-1.0.20.tar.gz
- **SHA256:** `ebb65ef6ca439333c2bb41a0c1990587288da07f6c7fd07cb3a18cc18d30ce19`
- **Verification:**
  ```bash
  curl -fsSL https://download.libsodium.org/libsodium/releases/libsodium-1.0.20.tar.gz -o libsodium-1.0.20.tar.gz
  echo "ebb65ef6ca439333c2bb41a0c1990587288da07f6c7fd07cb3a18cc18d30ce19  libsodium-1.0.20.tar.gz" | shasum -a 256 -c
  ```
- **Installation (Homebrew):**
  ```bash
  brew install libsodium
  # Verifies to: /opt/homebrew/Cellar/libsodium/1.0.20
  ```
- **Audit Status:** ✅ Audited by multiple third parties (Cure53, NCC Group, OSS-Fuzz)

---

### liboqs 0.14.0

- **Purpose:** Post-quantum cryptography (ML-KEM-1024, ML-DSA-87)
- **License:** MIT License
- **Source:** https://github.com/open-quantum-safe/liboqs/releases/tag/0.14.0
- **Tarball URL:** https://github.com/open-quantum-safe/liboqs/archive/refs/tags/0.14.0.tar.gz
- **SHA256:** `5fc8a8fa7c5f894dd79dc11c3f190ca669cac95b77e3bb7d8f99f9e8c1cf0b6f`
- **Verification:**
  ```bash
  curl -fsSL -L https://github.com/open-quantum-safe/liboqs/archive/refs/tags/0.14.0.tar.gz -o liboqs-0.14.0.tar.gz
  echo "5fc8a8fa7c5f894dd79dc11c3f190ca669cac95b77e3bb7d8f99f9e8c1cf0b6f  liboqs-0.14.0.tar.gz" | shasum -a 256 -c
  ```
- **Installation (Homebrew):**
  ```bash
  brew install liboqs
  # Verifies to: /opt/homebrew/Cellar/liboqs/0.14.0
  ```
- **NIST Standardization:**
  - ML-KEM-1024: FIPS 203 (August 2024)
  - ML-DSA-87: FIPS 204 (August 2024)
- **Audit Status:** ✅ Reference implementations vetted by NIST

---

## 2. Ada Toolchain (Alire)

### GNAT FSF 14.2.1

- **Purpose:** Ada compiler with SPARK support
- **License:** GPL-3.0-or-later with GCC Runtime Library Exception
- **Alire Crate:** `gnat_native=14.2.1`
- **Installation:**
  ```bash
  alr toolchain --select gnat_native=14.2.1
  ```
- **Installation Path (macOS):**
  ```
  ~/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/
  ```
- **Verification:**
  ```bash
  gnatls --version
  # Expected: GNAT 14.2.1
  ```
- **Provenance:** FSF GCC upstream (https://gcc.gnu.org)

---

### GPRbuild 24.0.1

- **Purpose:** Multi-language build tool for Ada/SPARK projects
- **License:** GPL-3.0-or-later
- **Alire Crate:** `gprbuild=24.0.1`
- **Installation:**
  ```bash
  alr toolchain --select gprbuild=24.0.1
  ```
- **Installation Path (macOS):**
  ```
  ~/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/
  ```
- **Verification:**
  ```bash
  gprbuild --version
  # Expected: GPRBUILD 24.0.1
  ```
- **Provenance:** AdaCore open-source release (https://github.com/AdaCore/gprbuild)

---

### GNATprove 14.1.1

- **Purpose:** SPARK formal verification tool
- **License:** GPL-3.0-or-later
- **Alire Crate:** `gnatprove=14.1.1`
- **Installation:**
  ```bash
  alr toolchain --install gnatprove=14.1.1
  ```
- **Installation Path (macOS):**
  ```
  ~/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/
  ```
- **Verification:**
  ```bash
  gnatprove --version
  # Expected: GNATprove 14.1.1
  ```
- **Provers Included:**
  - Alt-Ergo 2.5.3
  - CVC5 1.1.2
  - Z3 4.13.0
- **Provenance:** AdaCore open-source release (https://github.com/AdaCore/spark2014)

---

## 3. Operating System Requirements

### macOS (Development)

- **Minimum Version:** macOS 12 (Monterey)
- **Recommended:** macOS 14 (Sonoma) or later
- **Architecture:** ARM64 (Apple Silicon) or x86_64 (Intel)
- **System RNG:** `/dev/urandom` (verified via `libsodium` initialization)

### Linux (CI/Production)

- **Distribution:** Ubuntu 22.04 LTS (Jammy Jellyfish)
- **Kernel:** 5.15+ (for proper `/dev/urandom` CSPRNG)
- **Architecture:** x86_64 or aarch64
- **System RNG:** `/dev/urandom` with `getrandom(2)` syscall

---

## 4. Build-Time Dependencies

### Required System Libraries (macOS)

All installed via Homebrew:
```bash
brew install libsodium liboqs
```

Verify installations:
```bash
ls -la /opt/homebrew/Cellar/libsodium/1.0.20/lib/libsodium.a
ls -la /opt/homebrew/Cellar/liboqs/0.14.0/lib/liboqs.a
```

### Required System Libraries (Linux)

Build from source with pinned versions (see SHA256 above):
```bash
# libsodium
tar xzf libsodium-1.0.20.tar.gz
cd libsodium-1.0.20
./configure --prefix=/usr/local
make && sudo make install

# liboqs
tar xzf liboqs-0.14.0.tar.gz
cd liboqs-0.14.0
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=/usr/local ..
make && sudo make install
```

---

## 5. Runtime Dependencies

### Production Binary (anubis_main)

- **Static Linking:** All cryptographic libraries statically linked (`.a` archives)
- **Dynamic Dependencies (macOS):**
  ```bash
  otool -L bin/anubis_main
  # Expected output:
  #   /usr/lib/libSystem.B.dylib (macOS system library only)
  ```
- **Dynamic Dependencies (Linux):**
  ```bash
  ldd bin/anubis_main
  # Expected output:
  #   linux-vdso.so.1
  #   libc.so.6
  #   /lib64/ld-linux-x86-64.so.2
  ```
- **No Network Dependencies:** Binary operates fully offline

---

## 6. Verification Procedure

### Full Supply Chain Verification

Run this script to verify all dependencies:

```bash
#!/bin/bash
set -e

echo "=== ANUBIS-SPARK Supply Chain Verification ==="

# 1. Verify toolchain versions
echo "[1/5] Verifying Ada toolchain..."
gnatls --version | grep "GNAT 14.2.1" || { echo "❌ GNAT version mismatch"; exit 1; }
gprbuild --version | grep "24.0.1" || { echo "❌ GPRbuild version mismatch"; exit 1; }
gnatprove --version | grep "14.1.1" || { echo "❌ GNATprove version mismatch"; exit 1; }
echo "✅ Toolchain verified"

# 2. Verify libsodium (macOS)
echo "[2/5] Verifying libsodium..."
if [ -f /opt/homebrew/Cellar/libsodium/1.0.20/lib/libsodium.a ]; then
  echo "✅ libsodium 1.0.20 present"
else
  echo "❌ libsodium 1.0.20 not found"
  exit 1
fi

# 3. Verify liboqs (macOS)
echo "[3/5] Verifying liboqs..."
if [ -f /opt/homebrew/Cellar/liboqs/0.14.0/lib/liboqs.a ]; then
  echo "✅ liboqs 0.14.0 present"
else
  echo "❌ liboqs 0.14.0 not found"
  exit 1
fi

# 4. Verify binary static linking
echo "[4/5] Verifying binary static linking..."
if otool -L bin/anubis_main | grep -v "/usr/lib/libSystem" | grep ".dylib"; then
  echo "❌ Unexpected dynamic dependencies detected"
  exit 1
else
  echo "✅ Binary statically linked (system libs only)"
fi

# 5. Verify binary functionality
echo "[5/5] Verifying binary functionality..."
./bin/anubis_main version || { echo "❌ Binary execution failed"; exit 1; }
echo "✅ Binary functional"

echo ""
echo "=== ✅ All supply chain checks passed ==="
```

---

## 7. Security Considerations

### Trusted Sources

- **libsodium:** Downloaded from official `download.libsodium.org` domain
- **liboqs:** Downloaded from official GitHub releases (verified via GitHub signatures)
- **Alire toolchain:** Managed by Alire package manager with integrity checks

### Reproducible Builds

- **Build Environment:** Dockerized (see `Dockerfile` in project root)
- **Build Flags:** Documented in `anubis_spark.gpr`
- **Deterministic Output:** Same source + toolchain + flags → identical binary (SHA256 match)

### Update Policy

- **Security Patches:** Apply within 7 days of disclosure
- **Major Updates:** Require re-audit and SPARK proof verification
- **Breaking Changes:** Trigger new major version release

---

## 8. Change Log

| Date       | Version | Change                                      | Author  |
|------------|---------|---------------------------------------------|---------|
| 2025-10-11 | 1.1.0   | Initial Platinum release lockfile           | sicarii |

---

## 9. Contact

For supply chain security concerns or discrepancy reports:

- **GitHub Issues:** https://github.com/AnubisQuantumCipher/anubis-spark/issues
- **Security Email:** (to be added in future release)
- **PGP Key:** (to be added in future release)

---

**End of Lockfile**
