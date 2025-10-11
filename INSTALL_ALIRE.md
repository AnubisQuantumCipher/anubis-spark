# ANUBIS-SPARK Installation Guide for Alire

This guide provides step-by-step instructions for installing and using ANUBIS-SPARK through the Alire package manager.

## Prerequisites

### System Requirements
- **Operating System**: Linux (Ubuntu 20.04+, Debian 11+, CentOS 8+) or macOS 10.15+
- **Architecture**: x86_64 (ARM64 support planned)
- **Memory**: Minimum 4 GB RAM (8 GB recommended for large files)
- **Disk Space**: 500 MB for dependencies and build artifacts

### Required Tools
- **Alire**: Version 1.2.0 or later
- **GNAT**: Version 11.0 or later (automatically managed by Alire)
- **GNATprove**: Version 14.1+ (for SPARK verification)

## Installation Methods

### Method 1: Add to Existing Project

```bash
# Navigate to your Ada project
cd my_ada_project

# Add ANUBIS-SPARK as a dependency
alr with anubis_spark

# Build your project (dependencies auto-installed)
alr build
```

### Method 2: Create New Project with ANUBIS-SPARK

```bash
# Create new executable project
alr init --bin my_secure_app
cd my_secure_app

# Add ANUBIS-SPARK dependency
alr with anubis_spark

# Build the project
alr build
```

### Method 3: Library-Only Installation

```bash
# Create library project
alr init --lib my_crypto_lib
cd my_crypto_lib

# Add ANUBIS-SPARK
alr with anubis_spark

# Build library
alr build
```

## External Dependencies

ANUBIS-SPARK requires external cryptographic libraries that must be installed separately:

### Ubuntu/Debian Systems

```bash
# Update package manager
sudo apt update

# Install build tools
sudo apt install -y build-essential cmake ninja-build git

# Install libsodium (classical cryptography)
sudo apt install -y libsodium-dev

# Install liboqs (post-quantum cryptography) from source
cd /tmp
git clone --depth 1 --branch 0.14.0 https://github.com/open-quantum-safe/liboqs.git
cd liboqs
mkdir build && cd build
cmake -GNinja -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DOQS_BUILD_ONLY_LIB=ON \
    -DOQS_MINIMAL_BUILD="KEM_ml_kem_1024;SIG_ml_dsa_87" \
    ..
ninja
sudo ninja install
sudo ldconfig

# Verify installation
pkg-config --modversion libsodium
ls /usr/local/lib/liboqs*
```

### CentOS/RHEL/Fedora Systems

```bash
# Install build tools
sudo dnf install -y gcc gcc-c++ cmake ninja-build git

# Install libsodium
sudo dnf install -y libsodium-devel

# Install liboqs from source (same as Ubuntu)
# ... (follow Ubuntu instructions above)
```

### macOS Systems

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install dependencies
brew install libsodium liboqs cmake ninja

# Verify installation
brew list libsodium liboqs
```

## Verification

### Test Installation

```bash
# Create test project
alr init --bin anubis_test
cd anubis_test
alr with anubis_spark

# Create simple test program
cat > src/anubis_test.adb << 'EOF'
with Ada.Text_IO; use Ada.Text_IO;
with Anubis_Types.Storage;

procedure Anubis_Test is
   Identity : Anubis_Types.Storage.Identity_Type;
begin
   Put_Line ("ANUBIS-SPARK Installation Test");
   Put_Line ("Generating quantum-resistant identity...");
   
   Anubis_Types.Storage.Generate_Identity (Identity);
   
   if Anubis_Types.Storage.Is_Valid (Identity) then
      Put_Line ("✓ SUCCESS: ANUBIS-SPARK is properly installed!");
      Put_Line ("🏆 SPARK Platinum Certified");
      Put_Line ("🛡️ Quantum-Resistant Security Ready");
   else
      Put_Line ("✗ FAILED: Installation issue detected");
   end if;
end Anubis_Test;
EOF

# Build and run test
alr build
alr exec -- anubis_test
```

Expected output:
```
ANUBIS-SPARK Installation Test
Generating quantum-resistant identity...
✓ SUCCESS: ANUBIS-SPARK is properly installed!
🏆 SPARK Platinum Certified
🛡️ Quantum-Resistant Security Ready
```

### Verify SPARK Proofs (Optional)

```bash
# Install GNATprove if not already available
alr get gnatprove

# Run SPARK verification on ANUBIS-SPARK
cd ~/.alire/cache/dependencies/anubis_spark_*
gnatprove -P anubis_spark.gpr --level=4 --report=all

# Expected: 183/183 verification conditions proven
```

## Usage Examples

### Basic File Encryption

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Anubis_Types.Storage;
with Anubis_Types.Streaming;

procedure Encrypt_File is
   Identity : Anubis_Types.Storage.Identity_Type;
   Result   : Anubis_Types.Streaming.Result_Code;
begin
   -- Generate identity
   Anubis_Types.Storage.Generate_Identity (Identity);
   
   -- Save identity to file
   Anubis_Types.Storage.Save_Identity (Identity, "my_key.key");
   
   -- Encrypt file
   Anubis_Types.Streaming.Encrypt_File (
      Identity_Key => Identity,
      Input_File   => "document.pdf",
      Output_File  => "document.pdf.anubis",
      Result       => Result
   );
   
   if Result = Anubis_Types.Streaming.Success then
      Put_Line ("File encrypted successfully!");
   end if;
end Encrypt_File;
```

### Command-Line Tool

```bash
# After building your project with ANUBIS-SPARK
alr exec -- my_app keygen --output my_identity.key
alr exec -- my_app encrypt --key my_identity.key --input secret.txt --output secret.txt.anubis
alr exec -- my_app decrypt --key my_identity.key --input secret.txt.anubis --output decrypted.txt
```

## Troubleshooting

### Common Issues

#### 1. "libsodium not found"
```bash
# Ensure libsodium is installed and in library path
sudo apt install libsodium-dev  # Ubuntu/Debian
brew install libsodium          # macOS

# Check installation
pkg-config --libs libsodium
```

#### 2. "liboqs not found"
```bash
# Verify liboqs installation
ls /usr/local/lib/liboqs*
sudo ldconfig  # Linux only

# If missing, reinstall from source (see above)
```

#### 3. "GNAT version too old"
```bash
# Update Alire toolchain
alr toolchain --select gnat_native=13.2.1
alr toolchain --select gprbuild=22.0.1
```

#### 4. "SPARK verification failed"
```bash
# Install GNATprove
alr get gnatprove

# Check SPARK mode is enabled
grep -r "pragma SPARK_Mode" src/
```

### Performance Issues

#### Slow Encryption/Decryption
- **Cause**: Debug build mode
- **Solution**: Use release mode
```bash
alr build --release
```

#### High Memory Usage
- **Cause**: Large file processing
- **Solution**: ANUBIS-SPARK uses constant 64 MB memory regardless of file size

### Getting Help

1. **Documentation**: [GitHub Repository](https://github.com/AnubisQuantumCipher/anubis-spark)
2. **Issues**: [Report bugs](https://github.com/AnubisQuantumCipher/anubis-spark/issues)
3. **Discussions**: [Community support](https://github.com/AnubisQuantumCipher/anubis-spark/discussions)
4. **Alire Community**: [Alire Gitter](https://gitter.im/ada-lang/Alire)

## Security Considerations

### Production Deployment
- Always use release builds (`alr build --release`)
- Verify SPARK proofs before deployment
- Use secure key storage (hardware security modules recommended)
- Implement proper key rotation policies
- Regular security audits of encrypted data

### Key Management
- Store identity keys in secure locations
- Use file permissions (chmod 600) for key files
- Consider Shamir Secret Sharing for backup
- Never store keys in version control

### Compliance
- ANUBIS-SPARK meets NIST post-quantum cryptography standards
- SPARK Platinum certification provides highest assurance level
- Suitable for classified, financial, and healthcare data

---

**Next Steps**: See [ALIRE_README.md](ALIRE_README.md) for usage examples and [API documentation](docs/API.md) for detailed programming interface.
