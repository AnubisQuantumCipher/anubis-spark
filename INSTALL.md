# ANUBIS-SPARK Installation Guide

## Quick Install

```bash
cd ~/Desktop/anubis-spark
make install
```

This installs `anubis-spark` to `~/.local/bin/anubis-spark`.

## Add to PATH

Add `~/.local/bin` to your PATH by adding this line to your shell config:

**For Zsh** (~/.zshrc):
```bash
export PATH="$HOME/.local/bin:$PATH"
```

**For Bash** (~/.bash_profile or ~/.bashrc):
```bash
export PATH="$HOME/.local/bin:$PATH"
```

Then reload your shell:
```bash
source ~/.zshrc  # or ~/.bash_profile
```

## Verify Installation

```bash
anubis-spark version
```

You should see:
```
╔═══════════════════════════════════════════════════════════════╗
║  ANUBIS-SPARK v1.1.0 - Quantum-Resistant File Encryption     ║
║  PLATINUM-LEVEL SPARK VERIFICATION + NIST POST-QUANTUM       ║
╚═══════════════════════════════════════════════════════════════╝
```

## Usage

### Generate Identity
```bash
anubis-spark keygen --output my_identity.key
```

### Encrypt File
```bash
anubis-spark encrypt --key my_identity.key --input document.pdf
```

### Decrypt File
```bash
anubis-spark decrypt --key my_identity.key --input document.pdf.anubis
```

### Run Self-Tests
```bash
anubis-spark test
```

## System Requirements

- macOS 10.15+ or Linux
- ~100 MB RAM for operation
- liboqs 0.14.0 (installed via Homebrew)
- libsodium 1.0.20 (installed via Homebrew)

## Uninstall

```bash
cd ~/Desktop/anubis-spark
make uninstall
```

## Advanced Installation

Install to a custom location:
```bash
make install PREFIX=/usr/local
```

This installs to `/usr/local/bin/anubis-spark`.

## Building from Source

Requirements:
- GNAT 14.2.1+ (Ada compiler)
- GPRbuild 24.0.1+
- Alire package manager

Build:
```bash
make build
```

The binary will be in `bin/anubis_main`.

## Support

- GitHub: https://github.com/AnubisQuantumCipher/anubis-spark
- Issues: https://github.com/AnubisQuantumCipher/anubis-spark/issues
