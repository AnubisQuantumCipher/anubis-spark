#!/bin/bash
#===============================================================================
# ANUBIS-SPARK Bootstrap Script
# Complete local installation - nothing touches your system directories
#===============================================================================
#
# This script sets up a complete build environment inside the repository:
#   .tools/alire/         - Alire package manager
#   .tools/toolchain/     - GNAT compiler + gprbuild
#   .deps/                - libsodium 1.0.20 + liboqs 0.14.0
#
# Usage:
#   ./scripts/bootstrap.sh [OPTIONS]
#
# Options:
#   --help          Show this help message
#   --clean         Remove all local tools and deps, start fresh
#   --skip-build    Only install dependencies, don't build
#   --skip-deps     Skip building crypto libraries (use existing)
#   --skip-alire    Skip Alire installation (use system alire)
#   --verbose       Show detailed output
#
#===============================================================================

set -e

# Script location and repo root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Local installation directories
TOOLS_DIR="${REPO_ROOT}/.tools"
DEPS_DIR="${REPO_ROOT}/.deps"
ALIRE_DIR="${TOOLS_DIR}/alire"
ALIRE_SETTINGS="${TOOLS_DIR}/alire-settings"
ALIRE_CACHE="${TOOLS_DIR}/alire-cache"

# Versions
ALIRE_VERSION="2.0.1"
LIBSODIUM_VERSION="1.0.20"
LIBOQS_VERSION="0.14.0"

# Options
CLEAN=false
SKIP_BUILD=false
SKIP_DEPS=false
SKIP_ALIRE=false
VERBOSE=false

#-------------------------------------------------------------------------------
# Helper functions
#-------------------------------------------------------------------------------

log() {
    echo -e "\033[1;34m==>\033[0m $1"
}

log_success() {
    echo -e "\033[1;32m==>\033[0m $1"
}

log_warn() {
    echo -e "\033[1;33m==>\033[0m $1"
}

log_error() {
    echo -e "\033[1;31m==>\033[0m $1" >&2
}

die() {
    log_error "$1"
    exit 1
}

show_help() {
    cat << 'EOF'
ANUBIS-SPARK Bootstrap Script
==============================

Sets up a complete build environment locally within the repository.
Nothing is installed to system directories.

Usage: ./bootstrap [OPTIONS]

Options:
  --help          Show this help message
  --clean         Remove all local tools and deps, start fresh
  --skip-build    Only install dependencies, don't build the project
  --skip-deps     Skip building crypto libraries (use existing .deps/)
  --skip-alire    Skip Alire installation (use system alire if available)
  --verbose       Show detailed build output

What gets installed (all local to repo):
  .tools/alire/         Alire package manager binary
  .tools/alire-settings/ Alire configuration (isolated from ~/.config/alire)
  .tools/alire-cache/   Alire downloads and toolchain cache
  .deps/lib/            libsodium.a and liboqs.a static libraries
  .deps/include/        Header files

After bootstrap completes:
  source ./env.sh       Load the environment (optional, for manual builds)
  ./bin/anubis_main     Run the built binary

Examples:
  ./bootstrap                    # Full setup and build
  ./bootstrap --clean            # Clean reinstall
  ./bootstrap --skip-build       # Setup only, build later with 'make build'
  ./bootstrap --skip-alire       # Use system Alire (brew install alire)

EOF
    exit 0
}

#-------------------------------------------------------------------------------
# Parse arguments
#-------------------------------------------------------------------------------

while [[ $# -gt 0 ]]; do
    case $1 in
        --help|-h)
            show_help
            ;;
        --clean)
            CLEAN=true
            shift
            ;;
        --skip-build)
            SKIP_BUILD=true
            shift
            ;;
        --skip-deps)
            SKIP_DEPS=true
            shift
            ;;
        --skip-alire)
            SKIP_ALIRE=true
            shift
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        *)
            die "Unknown option: $1 (use --help for usage)"
            ;;
    esac
done

#-------------------------------------------------------------------------------
# Detect platform
#-------------------------------------------------------------------------------

detect_platform() {
    UNAME_S=$(uname -s)
    UNAME_M=$(uname -m)

    case "${UNAME_S}" in
        Linux)
            PLATFORM="linux"
            case "${UNAME_M}" in
                x86_64)  ARCH="x86_64" ;;
                aarch64) ARCH="aarch64" ;;
                *)       die "Unsupported Linux architecture: ${UNAME_M}" ;;
            esac
            ;;
        Darwin)
            PLATFORM="macos"
            case "${UNAME_M}" in
                x86_64)  ARCH="x86_64" ;;
                arm64)   ARCH="aarch64" ;;
                *)       die "Unsupported macOS architecture: ${UNAME_M}" ;;
            esac
            ;;
        *)
            die "Unsupported platform: ${UNAME_S}"
            ;;
    esac

    log "Detected platform: ${PLATFORM}-${ARCH}"
}

#-------------------------------------------------------------------------------
# Check prerequisites
#-------------------------------------------------------------------------------

check_prerequisites() {
    log "Checking prerequisites..."

    local missing=()

    # Required for building dependencies
    command -v cmake >/dev/null 2>&1 || missing+=("cmake")
    command -v make >/dev/null 2>&1 || missing+=("make")
    command -v gcc >/dev/null 2>&1 || command -v clang >/dev/null 2>&1 || missing+=("gcc or clang")

    # Required for downloading
    if ! command -v wget >/dev/null 2>&1 && ! command -v curl >/dev/null 2>&1; then
        missing+=("wget or curl")
    fi

    # Check for OpenSSL headers (required by liboqs)
    local ssl_found=false
    for dir in /usr/include/openssl /usr/local/include/openssl /opt/homebrew/include/openssl; do
        if [ -f "${dir}/ssl.h" ]; then
            ssl_found=true
            break
        fi
    done
    # Also check pkg-config
    if ! $ssl_found && command -v pkg-config >/dev/null 2>&1; then
        if pkg-config --exists openssl 2>/dev/null; then
            ssl_found=true
        fi
    fi
    if ! $ssl_found; then
        missing+=("OpenSSL development headers (libssl-dev on Ubuntu, openssl on macOS)")
    fi

    if [ ${#missing[@]} -gt 0 ]; then
        log_error "Missing prerequisites:"
        for item in "${missing[@]}"; do
            echo "  - $item"
        done
        echo ""
        echo "Install them with:"
        if [ "${PLATFORM}" = "linux" ]; then
            echo "  sudo apt-get install -y build-essential cmake libssl-dev wget"
        else
            echo "  xcode-select --install"
            echo "  brew install cmake openssl wget"
        fi
        exit 1
    fi

    log_success "All prerequisites satisfied"
}

#-------------------------------------------------------------------------------
# Clean existing installation
#-------------------------------------------------------------------------------

clean_installation() {
    if [ "$CLEAN" = true ]; then
        log "Cleaning existing installation..."
        rm -rf "${TOOLS_DIR}" "${DEPS_DIR}" "${REPO_ROOT}/obj" "${REPO_ROOT}/bin"
        rm -f "${REPO_ROOT}/env.sh"
        log_success "Clean complete"
    fi
}

#-------------------------------------------------------------------------------
# Download helper
#-------------------------------------------------------------------------------

download() {
    local url="$1"
    local output="$2"

    if command -v wget >/dev/null 2>&1; then
        wget -q --show-progress -O "$output" "$url"
    elif command -v curl >/dev/null 2>&1; then
        curl -L --progress-bar -o "$output" "$url"
    else
        die "No download tool available (wget or curl required)"
    fi
}

#-------------------------------------------------------------------------------
# Install Alire locally
#-------------------------------------------------------------------------------

install_alire() {
    if [ "$SKIP_ALIRE" = true ]; then
        if command -v alr >/dev/null 2>&1; then
            log "Using system Alire: $(which alr)"
            ALR_CMD="alr"
            return 0
        else
            die "--skip-alire specified but no system alire found"
        fi
    fi

    # Check if already installed locally
    if [ -x "${ALIRE_DIR}/bin/alr" ]; then
        log "Alire already installed locally"
        ALR_CMD="${ALIRE_DIR}/bin/alr"
        return 0
    fi

    log "Installing Alire ${ALIRE_VERSION} locally..."

    mkdir -p "${ALIRE_DIR}"

    local alire_url=""
    case "${PLATFORM}-${ARCH}" in
        linux-x86_64)
            alire_url="https://github.com/alire-project/alire/releases/download/v${ALIRE_VERSION}/alr-${ALIRE_VERSION}-bin-x86_64-linux.zip"
            ;;
        linux-aarch64)
            alire_url="https://github.com/alire-project/alire/releases/download/v${ALIRE_VERSION}/alr-${ALIRE_VERSION}-bin-aarch64-linux.zip"
            ;;
        macos-x86_64)
            alire_url="https://github.com/alire-project/alire/releases/download/v${ALIRE_VERSION}/alr-${ALIRE_VERSION}-bin-x86_64-macos.zip"
            ;;
        macos-aarch64)
            alire_url="https://github.com/alire-project/alire/releases/download/v${ALIRE_VERSION}/alr-${ALIRE_VERSION}-bin-aarch64-macos.zip"
            ;;
        *)
            die "No Alire binary available for ${PLATFORM}-${ARCH}"
            ;;
    esac

    local tmp_zip=$(mktemp)
    trap "rm -f ${tmp_zip}" EXIT

    log "Downloading Alire..."
    download "${alire_url}" "${tmp_zip}"

    log "Extracting Alire..."
    mkdir -p "${ALIRE_DIR}/bin"
    unzip -q -o "${tmp_zip}" -d "${ALIRE_DIR}"

    # Find and move the alr binary
    local alr_bin=$(find "${ALIRE_DIR}" -name "alr" -type f -perm +111 2>/dev/null | head -1)
    if [ -z "$alr_bin" ]; then
        alr_bin=$(find "${ALIRE_DIR}" -name "alr" -type f 2>/dev/null | head -1)
    fi

    if [ -n "$alr_bin" ] && [ "$alr_bin" != "${ALIRE_DIR}/bin/alr" ]; then
        mv "$alr_bin" "${ALIRE_DIR}/bin/alr"
        chmod +x "${ALIRE_DIR}/bin/alr"
    fi

    if [ ! -x "${ALIRE_DIR}/bin/alr" ]; then
        die "Failed to install Alire"
    fi

    ALR_CMD="${ALIRE_DIR}/bin/alr"
    log_success "Alire installed: ${ALR_CMD}"
}

#-------------------------------------------------------------------------------
# Setup Alire environment (isolated from system)
#-------------------------------------------------------------------------------

setup_alire_env() {
    log "Configuring isolated Alire environment..."

    mkdir -p "${ALIRE_SETTINGS}" "${ALIRE_CACHE}"

    # Set environment for isolated Alire
    export ALIRE_SETTINGS_DIR="${ALIRE_SETTINGS}"
    export ALIRE_CACHE_DIR="${ALIRE_CACHE}"
    export XDG_CONFIG_HOME="${TOOLS_DIR}/config"

    log_success "Alire environment configured (isolated from ~/.config/alire)"
}

#-------------------------------------------------------------------------------
# Install Ada toolchain via Alire
#-------------------------------------------------------------------------------

install_toolchain() {
    log "Installing Ada toolchain via Alire..."

    cd "${REPO_ROOT}"

    # Select toolchain non-interactively
    log "Selecting GNAT toolchain..."
    ${ALR_CMD} --non-interactive toolchain --select gnat_native gprbuild 2>/dev/null || true

    # Verify toolchain
    if ${ALR_CMD} exec -- gnatls --version >/dev/null 2>&1; then
        local gnat_version=$(${ALR_CMD} exec -- gnatls --version 2>/dev/null | head -1)
        log_success "Toolchain ready: ${gnat_version}"
    else
        log_warn "Toolchain installation may need manual selection"
        log "Run: ${ALR_CMD} toolchain --select"
    fi
}

#-------------------------------------------------------------------------------
# Build crypto dependencies
#-------------------------------------------------------------------------------

build_dependencies() {
    if [ "$SKIP_DEPS" = true ]; then
        if [ -f "${DEPS_DIR}/lib/libsodium.a" ] && [ -f "${DEPS_DIR}/lib/liboqs.a" ]; then
            log "Using existing dependencies in .deps/"
            return 0
        else
            log_warn "--skip-deps specified but .deps/ is incomplete, building anyway..."
        fi
    fi

    # Check if already built
    if [ -f "${DEPS_DIR}/lib/libsodium.a" ] && [ -f "${DEPS_DIR}/lib/liboqs.a" ]; then
        log "Dependencies already built in .deps/"
        return 0
    fi

    log "Building crypto dependencies (libsodium ${LIBSODIUM_VERSION}, liboqs ${LIBOQS_VERSION})..."

    mkdir -p "${DEPS_DIR}"
    local build_dir=$(mktemp -d)
    trap "rm -rf ${build_dir}" EXIT

    # Determine parallel jobs
    local jobs=$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

    #---------------------------------------------------------------------------
    # Build libsodium
    #---------------------------------------------------------------------------
    log "Building libsodium ${LIBSODIUM_VERSION}..."
    cd "${build_dir}"

    download "https://download.libsodium.org/libsodium/releases/libsodium-${LIBSODIUM_VERSION}.tar.gz" \
             "libsodium-${LIBSODIUM_VERSION}.tar.gz"

    tar xzf "libsodium-${LIBSODIUM_VERSION}.tar.gz"
    cd "libsodium-${LIBSODIUM_VERSION}"

    if [ "$VERBOSE" = true ]; then
        ./configure --prefix="${DEPS_DIR}" --enable-static --disable-shared --with-pic
        make -j${jobs}
    else
        ./configure --prefix="${DEPS_DIR}" --enable-static --disable-shared --with-pic >/dev/null 2>&1
        make -j${jobs} >/dev/null 2>&1
    fi
    make install >/dev/null 2>&1

    log_success "libsodium ${LIBSODIUM_VERSION} built"

    #---------------------------------------------------------------------------
    # Build liboqs
    #---------------------------------------------------------------------------
    log "Building liboqs ${LIBOQS_VERSION}..."
    cd "${build_dir}"

    download "https://github.com/open-quantum-safe/liboqs/archive/refs/tags/${LIBOQS_VERSION}.tar.gz" \
             "liboqs-${LIBOQS_VERSION}.tar.gz"

    tar xzf "liboqs-${LIBOQS_VERSION}.tar.gz"
    cd "liboqs-${LIBOQS_VERSION}"
    mkdir build && cd build

    local cmake_generator=""
    if command -v ninja >/dev/null 2>&1; then
        cmake_generator="-GNinja"
    fi

    if [ "$VERBOSE" = true ]; then
        cmake ${cmake_generator} \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX="${DEPS_DIR}" \
            -DBUILD_SHARED_LIBS=OFF \
            -DOQS_BUILD_ONLY_LIB=ON \
            -DOQS_USE_OPENSSL=ON \
            ..
        cmake --build . --parallel ${jobs}
    else
        cmake ${cmake_generator} \
            -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_INSTALL_PREFIX="${DEPS_DIR}" \
            -DBUILD_SHARED_LIBS=OFF \
            -DOQS_BUILD_ONLY_LIB=ON \
            -DOQS_USE_OPENSSL=ON \
            .. >/dev/null 2>&1
        cmake --build . --parallel ${jobs} >/dev/null 2>&1
    fi
    cmake --install . >/dev/null 2>&1

    log_success "liboqs ${LIBOQS_VERSION} built"

    #---------------------------------------------------------------------------
    # Verify installation
    #---------------------------------------------------------------------------
    if [ ! -f "${DEPS_DIR}/lib/libsodium.a" ]; then
        die "libsodium build failed"
    fi
    if [ ! -f "${DEPS_DIR}/lib/liboqs.a" ]; then
        die "liboqs build failed"
    fi

    log_success "All dependencies built successfully"
}

#-------------------------------------------------------------------------------
# Generate environment script
#-------------------------------------------------------------------------------

generate_env_script() {
    log "Generating env.sh..."

    cat > "${REPO_ROOT}/env.sh" << EOF
# ANUBIS-SPARK Environment
# Source this file to set up the build environment:
#   source ./env.sh

# Repository root
export ANUBIS_ROOT="${REPO_ROOT}"

# Crypto library path (libsodium, liboqs)
export ANUBIS_LIB_DIR="${DEPS_DIR}/lib"

# Alire settings (isolated from system)
export ALIRE_SETTINGS_DIR="${ALIRE_SETTINGS}"
export ALIRE_CACHE_DIR="${ALIRE_CACHE}"

# Add local Alire to PATH
if [ -d "${ALIRE_DIR}/bin" ]; then
    export PATH="${ALIRE_DIR}/bin:\$PATH"
fi

# Convenience alias
alias alr-local="${ALR_CMD:-alr}"

echo "ANUBIS-SPARK environment loaded"
echo "  ANUBIS_LIB_DIR=\$ANUBIS_LIB_DIR"
echo "  Alire: \$(which alr 2>/dev/null || echo '${ALR_CMD}')"
EOF

    log_success "Environment script created: env.sh"
}

#-------------------------------------------------------------------------------
# Build the project
#-------------------------------------------------------------------------------

build_project() {
    if [ "$SKIP_BUILD" = true ]; then
        log "Skipping build (--skip-build specified)"
        return 0
    fi

    log "Building ANUBIS-SPARK..."

    cd "${REPO_ROOT}"

    # Set library path
    export ANUBIS_LIB_DIR="${DEPS_DIR}/lib"

    # Build with Alire
    if [ "$VERBOSE" = true ]; then
        ${ALR_CMD} build --release
    else
        ${ALR_CMD} build --release 2>&1 | grep -E "(Compile|Link|Build|Error|error:)" || true
    fi

    # Fix RPATH on macOS
    if [ "${PLATFORM}" = "macos" ] && [ -x "${REPO_ROOT}/scripts/fix-rpath.sh" ]; then
        log "Fixing RPATH on macOS binaries..."
        for bin in "${REPO_ROOT}"/bin/*; do
            if [ -f "$bin" ] && [ -x "$bin" ]; then
                "${REPO_ROOT}/scripts/fix-rpath.sh" "$bin" >/dev/null 2>&1 || true
            fi
        done
    fi

    if [ ! -x "${REPO_ROOT}/bin/anubis_main" ]; then
        die "Build failed - binary not created"
    fi

    log_success "Build complete: bin/anubis_main"
}

#-------------------------------------------------------------------------------
# Run smoke tests
#-------------------------------------------------------------------------------

run_smoke_tests() {
    if [ "$SKIP_BUILD" = true ]; then
        return 0
    fi

    log "Running smoke tests..."

    cd "${REPO_ROOT}"

    # Version check
    if ! ./bin/anubis_main version >/dev/null 2>&1; then
        die "Smoke test failed: version command"
    fi

    # Self-test
    log "Running self-tests..."
    if ./bin/anubis_main test; then
        log_success "All self-tests passed!"
    else
        die "Self-tests failed"
    fi
}

#-------------------------------------------------------------------------------
# Print summary
#-------------------------------------------------------------------------------

print_summary() {
    echo ""
    echo "========================================"
    log_success "ANUBIS-SPARK Bootstrap Complete!"
    echo "========================================"
    echo ""
    echo "Installation summary:"
    echo "  Alire:      ${ALR_CMD:-${ALIRE_DIR}/bin/alr}"
    echo "  Deps:       ${DEPS_DIR}/lib/"
    echo "  Binary:     ${REPO_ROOT}/bin/anubis_main"
    echo ""
    echo "To load the environment in a new shell:"
    echo "  source ./env.sh"
    echo ""
    echo "To rebuild:"
    echo "  source ./env.sh && alr build --release"
    echo "  # or simply:"
    echo "  make build"
    echo ""
    echo "Run the binary:"
    echo "  ./bin/anubis_main version"
    echo "  ./bin/anubis_main --help"
    echo ""
}

#===============================================================================
# Main
#===============================================================================

main() {
    echo ""
    echo "=========================================="
    echo "  ANUBIS-SPARK Bootstrap"
    echo "=========================================="
    echo ""

    detect_platform
    check_prerequisites
    clean_installation

    # Create directories
    mkdir -p "${TOOLS_DIR}" "${DEPS_DIR}"

    install_alire
    setup_alire_env
    install_toolchain
    build_dependencies
    generate_env_script
    build_project
    run_smoke_tests
    print_summary
}

main "$@"
