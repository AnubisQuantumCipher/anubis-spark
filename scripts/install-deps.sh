#!/bin/bash
# ANUBIS-SPARK Dependency Installer
# Builds libsodium 1.0.20 and liboqs 0.14.0 from source
#
# Usage: ./scripts/install-deps.sh [--prefix /path] [--system]
#   --prefix PATH   Install to PATH (default: $HOME/anubis-deps)
#   --system        Install to /usr/local (requires sudo)

set -e

# Default installation prefix
PREFIX="${HOME}/anubis-deps"
USE_SUDO=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --prefix)
            PREFIX="$2"
            shift 2
            ;;
        --system)
            PREFIX="/usr/local"
            USE_SUDO="sudo"
            shift
            ;;
        --help|-h)
            echo "ANUBIS-SPARK Dependency Installer"
            echo ""
            echo "Usage: $0 [--prefix /path] [--system]"
            echo ""
            echo "Options:"
            echo "  --prefix PATH   Install to PATH (default: \$HOME/anubis-deps)"
            echo "  --system        Install to /usr/local (requires sudo)"
            echo ""
            echo "This script builds and installs:"
            echo "  - libsodium 1.0.20 (required for HKDF support)"
            echo "  - liboqs 0.14.0 (required for ML-KEM/ML-DSA)"
            echo ""
            echo "After running, set ANUBIS_LIB_DIR=${PREFIX}/lib before building."
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=========================================="
echo "ANUBIS-SPARK Dependency Installer"
echo "=========================================="
echo ""
echo "Installation prefix: ${PREFIX}"
echo ""

# Check for required build tools
echo "Checking build dependencies..."

check_cmd() {
    if ! command -v "$1" &> /dev/null; then
        echo "Error: $1 is required but not installed."
        case "$1" in
            cmake)
                echo "  Ubuntu/Debian: sudo apt install cmake"
                echo "  macOS: brew install cmake"
                ;;
            make)
                echo "  Ubuntu/Debian: sudo apt install build-essential"
                echo "  macOS: xcode-select --install"
                ;;
            wget|curl)
                echo "  Ubuntu/Debian: sudo apt install wget"
                echo "  macOS: brew install wget"
                ;;
        esac
        exit 1
    fi
}

check_cmd cmake
check_cmd make
check_cmd gcc

# Check for wget or curl
if command -v wget &> /dev/null; then
    DOWNLOAD="wget -q"
elif command -v curl &> /dev/null; then
    DOWNLOAD="curl -sLO"
else
    echo "Error: wget or curl is required"
    exit 1
fi

# Check for ninja (optional but faster)
if command -v ninja &> /dev/null; then
    CMAKE_GENERATOR="-GNinja"
    BUILD_CMD="ninja"
else
    CMAKE_GENERATOR=""
    BUILD_CMD="make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)"
fi

# Check for OpenSSL headers (required by liboqs)
echo "Checking for OpenSSL development headers..."
if [ -f /usr/include/openssl/ssl.h ] || [ -f /usr/local/include/openssl/ssl.h ] || [ -f /opt/homebrew/include/openssl/ssl.h ]; then
    echo "  OpenSSL headers found"
else
    echo "Error: OpenSSL development headers not found."
    echo "  Ubuntu/Debian: sudo apt install libssl-dev"
    echo "  macOS: brew install openssl"
    exit 1
fi

# Create build directory
BUILDDIR=$(mktemp -d)
trap "rm -rf ${BUILDDIR}" EXIT
cd "${BUILDDIR}"

echo ""
echo "=========================================="
echo "Building libsodium 1.0.20..."
echo "=========================================="

SODIUM_VERSION="1.0.20"
SODIUM_URL="https://download.libsodium.org/libsodium/releases/libsodium-${SODIUM_VERSION}.tar.gz"

echo "Downloading libsodium ${SODIUM_VERSION}..."
$DOWNLOAD "${SODIUM_URL}"
tar xzf "libsodium-${SODIUM_VERSION}.tar.gz"
cd "libsodium-${SODIUM_VERSION}"

echo "Configuring libsodium..."
./configure --prefix="${PREFIX}" --enable-static --disable-shared --with-pic

echo "Building libsodium..."
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

echo "Installing libsodium..."
${USE_SUDO} make install

cd "${BUILDDIR}"

echo ""
echo "=========================================="
echo "Building liboqs 0.14.0..."
echo "=========================================="

OQS_VERSION="0.14.0"
OQS_URL="https://github.com/open-quantum-safe/liboqs/archive/refs/tags/${OQS_VERSION}.tar.gz"

echo "Downloading liboqs ${OQS_VERSION}..."
$DOWNLOAD "${OQS_URL}"
tar xzf "${OQS_VERSION}.tar.gz"
cd "liboqs-${OQS_VERSION}"
mkdir build && cd build

echo "Configuring liboqs..."
cmake ${CMAKE_GENERATOR} \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
    -DBUILD_SHARED_LIBS=OFF \
    -DOQS_BUILD_ONLY_LIB=ON \
    -DOQS_USE_OPENSSL=ON \
    ..

echo "Building liboqs..."
${BUILD_CMD}

echo "Installing liboqs..."
${USE_SUDO} ${BUILD_CMD} install

echo ""
echo "=========================================="
echo "Installation Complete!"
echo "=========================================="
echo ""
echo "Libraries installed to: ${PREFIX}"
echo ""
echo "To build ANUBIS-SPARK, run:"
echo ""
echo "  export ANUBIS_LIB_DIR=\"${PREFIX}/lib\""
echo "  alr build --release"
echo ""
echo "Or with static linking (recommended):"
echo ""
echo "  export ANUBIS_LIB_DIR=\"${PREFIX}/lib\""
echo "  export STATIC_LINK=true"
echo "  alr build --release"
echo ""

# Verify installation
echo "Verifying installation..."
if [ -f "${PREFIX}/lib/libsodium.a" ]; then
    echo "  libsodium.a: OK"
else
    echo "  libsodium.a: MISSING"
    exit 1
fi

if [ -f "${PREFIX}/lib/liboqs.a" ]; then
    echo "  liboqs.a: OK"
else
    echo "  liboqs.a: MISSING"
    exit 1
fi

echo ""
echo "All dependencies installed successfully!"
