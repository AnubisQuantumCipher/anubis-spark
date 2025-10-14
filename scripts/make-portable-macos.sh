#!/usr/bin/env bash
#
# make-portable-macos.sh - Make macOS binary portable across different Homebrew installations
#
# This script fixes library paths in the binary to work on both:
# - Intel Macs (Homebrew at /usr/local)
# - Apple Silicon Macs (Homebrew at /opt/homebrew)
#
# Usage:
#   ./scripts/make-portable-macos.sh [binary_path]

set -euo pipefail

BINARY="${1:-./bin/anubis_main}"

if [[ ! -f "$BINARY" ]]; then
    echo "Error: Binary not found: $BINARY"
    echo "Usage: $0 [binary_path]"
    exit 1
fi

if [[ "$(uname -s)" != "Darwin" ]]; then
    echo "This script is only for macOS. Skipping."
    exit 0
fi

echo "====================================="
echo "Making macOS binary portable"
echo "====================================="
echo "Binary: $BINARY"
echo ""

# Get current architecture
ARCH=$(uname -m)
echo "Current architecture: $ARCH"
echo ""

# Detect Homebrew prefix
if [[ -d "/opt/homebrew" ]]; then
    BREW_PREFIX="/opt/homebrew"
elif [[ -d "/usr/local" ]]; then
    BREW_PREFIX="/usr/local"
else
    echo "Error: Homebrew not found at /opt/homebrew or /usr/local"
    exit 1
fi

echo "Homebrew prefix: $BREW_PREFIX"
echo ""

echo "Current library dependencies:"
otool -L "$BINARY"
echo ""

# Change absolute paths to @rpath-based paths
echo "Fixing library paths to use @rpath..."

# Fix libsodium
if otool -L "$BINARY" | grep -q "libsodium"; then
    CURRENT_SODIUM=$(otool -L "$BINARY" | grep libsodium | awk '{print $1}' | head -1)
    echo "  libsodium: $CURRENT_SODIUM -> @rpath/libsodium.26.dylib"
    install_name_tool -change "$CURRENT_SODIUM" "@rpath/libsodium.26.dylib" "$BINARY"
fi

# Fix OpenSSL libssl
if otool -L "$BINARY" | grep -q "libssl"; then
    CURRENT_SSL=$(otool -L "$BINARY" | grep libssl | grep -v ":" | awk '{print $1}' | head -1)
    echo "  libssl: $CURRENT_SSL -> @rpath/libssl.3.dylib"
    install_name_tool -change "$CURRENT_SSL" "@rpath/libssl.3.dylib" "$BINARY"
fi

# Fix OpenSSL libcrypto
if otool -L "$BINARY" | grep -q "libcrypto"; then
    CURRENT_CRYPTO=$(otool -L "$BINARY" | grep libcrypto | grep -v ":" | awk '{print $1}' | head -1)
    echo "  libcrypto: $CURRENT_CRYPTO -> @rpath/libcrypto.3.dylib"
    install_name_tool -change "$CURRENT_CRYPTO" "@rpath/libcrypto.3.dylib" "$BINARY"
fi

echo ""
echo "Adding RPATH search paths..."

# Add both Homebrew locations to RPATH (order matters - prefer native architecture)
if [[ "$ARCH" == "arm64" ]]; then
    # Apple Silicon: prefer /opt/homebrew, fallback to /usr/local (Rosetta)
    install_name_tool -add_rpath "/opt/homebrew/opt/libsodium/lib" "$BINARY" 2>/dev/null || true
    install_name_tool -add_rpath "/opt/homebrew/opt/openssl@3/lib" "$BINARY" 2>/dev/null || true
    install_name_tool -add_rpath "/usr/local/opt/libsodium/lib" "$BINARY" 2>/dev/null || true
    install_name_tool -add_rpath "/usr/local/opt/openssl@3/lib" "$BINARY" 2>/dev/null || true
else
    # Intel: prefer /usr/local, fallback to /opt/homebrew
    install_name_tool -add_rpath "/usr/local/opt/libsodium/lib" "$BINARY" 2>/dev/null || true
    install_name_tool -add_rpath "/usr/local/opt/openssl@3/lib" "$BINARY" 2>/dev/null || true
    install_name_tool -add_rpath "/opt/homebrew/opt/libsodium/lib" "$BINARY" 2>/dev/null || true
    install_name_tool -add_rpath "/opt/homebrew/opt/openssl@3/lib" "$BINARY" 2>/dev/null || true
fi

echo "  Added RPATH entries for both Homebrew locations"
echo ""

echo "Updated library dependencies:"
otool -L "$BINARY"
echo ""

echo "RPATH entries:"
otool -l "$BINARY" | grep -A 2 LC_RPATH | grep "path " | awk '{print "  " $2}'
echo ""

echo "====================================="
echo "Testing binary..."
echo "====================================="

if "$BINARY" version > /dev/null 2>&1; then
    echo "✅ SUCCESS: Binary runs correctly!"
    "$BINARY" version
else
    echo "❌ FAILED: Binary still has issues"
    echo ""
    echo "Troubleshooting:"
    echo "1. Ensure libsodium is installed: brew install libsodium"
    echo "2. Ensure openssl@3 is installed: brew install openssl@3"
    echo "3. Check library availability:"
    echo "   ls $BREW_PREFIX/opt/libsodium/lib/libsodium.26.dylib"
    echo "   ls $BREW_PREFIX/opt/openssl@3/lib/libssl.3.dylib"
    exit 1
fi

echo ""
echo "====================================="
echo "✅ Binary is now portable!"
echo "====================================="
