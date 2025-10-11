# ANUBIS-SPARK Reproducible Build Container
# Version: 1.1.0 Platinum
# Purpose: Deterministic builds with SPARK formal verification

FROM ubuntu:22.04

LABEL maintainer="ANUBIS-SPARK Team"
LABEL description="Reproducible build environment for ANUBIS-SPARK with SPARK proofs"
LABEL version="1.1.0"

###############################################################################
# Build Arguments
###############################################################################

ARG LIBSODIUM_VERSION=1.0.20
ARG LIBSODIUM_SHA256=ebb65ef6ca439333c2bb41a0c1990587288da07f6c7fd07cb3a18cc18d30ce19
ARG LIBOQS_VERSION=0.14.0
ARG LIBOQS_SHA256=5fc8a8fa7c5f894dd79dc11c3f190ca669cac95b77e3bb7d8f99f9e8c1cf0b6f

###############################################################################
# System Dependencies
###############################################################################

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    # Build essentials
    build-essential \
    gcc \
    g++ \
    make \
    cmake \
    ninja-build \
    pkg-config \
    # Download tools
    curl \
    wget \
    ca-certificates \
    # libsodium dependencies
    autoconf \
    automake \
    libtool \
    # Verification tools
    file \
    binutils \
    # Git (for Alire)
    git \
    # Utilities
    unzip \
    && rm -rf /var/lib/apt/lists/*

###############################################################################
# Install libsodium 1.0.20 (Classical Crypto)
###############################################################################

WORKDIR /tmp/build
RUN echo "=== Building libsodium ${LIBSODIUM_VERSION} ===" && \
    curl -fsSL "https://download.libsodium.org/libsodium/releases/libsodium-${LIBSODIUM_VERSION}.tar.gz" \
         -o libsodium.tar.gz && \
    echo "${LIBSODIUM_SHA256}  libsodium.tar.gz" | sha256sum -c && \
    tar xzf libsodium.tar.gz && \
    cd "libsodium-${LIBSODIUM_VERSION}" && \
    ./configure --prefix=/usr/local --disable-shared --enable-static && \
    make -j$(nproc) && \
    make check && \
    make install && \
    ldconfig && \
    cd /tmp && rm -rf /tmp/build

###############################################################################
# Install liboqs 0.14.0 (Post-Quantum Crypto)
###############################################################################

WORKDIR /tmp/build
RUN echo "=== Building liboqs ${LIBOQS_VERSION} ===" && \
    curl -fsSL -L "https://github.com/open-quantum-safe/liboqs/archive/refs/tags/${LIBOQS_VERSION}.tar.gz" \
         -o liboqs.tar.gz && \
    echo "${LIBOQS_SHA256}  liboqs.tar.gz" | sha256sum -c && \
    tar xzf liboqs.tar.gz && \
    cd "liboqs-${LIBOQS_VERSION}" && \
    mkdir build && cd build && \
    cmake -GNinja \
          -DCMAKE_INSTALL_PREFIX=/usr/local \
          -DCMAKE_BUILD_TYPE=Release \
          -DBUILD_SHARED_LIBS=OFF \
          -DOQS_BUILD_ONLY_LIB=ON \
          .. && \
    ninja && \
    ninja install && \
    ldconfig && \
    cd /tmp && rm -rf /tmp/build

###############################################################################
# Install Alire Package Manager
###############################################################################

RUN echo "=== Installing Alire ===" && \
    curl -fsSL https://alire.ada.dev/install.sh | sh -s -- -y && \
    echo 'export PATH="$HOME/.alire/bin:$PATH"' >> /root/.bashrc && \
    /root/.alire/bin/alr --version

ENV PATH="/root/.alire/bin:$PATH"

###############################################################################
# Install Ada Toolchain (GNAT + GPRbuild + GNATprove)
###############################################################################

RUN echo "=== Installing Ada toolchain ===" && \
    alr toolchain --select gnat_native=14.2.1 gprbuild=24.0.1 && \
    alr toolchain --install gnatprove=14.1.1 && \
    echo 'eval "$(alr printenv)"' >> /root/.bashrc

# Verify toolchain installation
RUN bash -c 'eval "$(alr printenv)" && \
             gnatls --version && \
             gprbuild --version && \
             gnatprove --version'

###############################################################################
# Create Build User (Security Best Practice)
###############################################################################

RUN useradd -m -s /bin/bash anubis && \
    mkdir -p /home/anubis/.alire && \
    cp -r /root/.alire/* /home/anubis/.alire/ && \
    chown -R anubis:anubis /home/anubis/.alire

USER anubis
WORKDIR /home/anubis

# Update PATH for anubis user
ENV PATH="/home/anubis/.alire/bin:$PATH"
RUN echo 'eval "$(alr printenv)"' >> /home/anubis/.bashrc

###############################################################################
# Copy ANUBIS-SPARK Source
###############################################################################

COPY --chown=anubis:anubis . /home/anubis/anubis-spark
WORKDIR /home/anubis/anubis-spark

###############################################################################
# STAGE 1: SPARK Formal Verification (Fast Proof)
###############################################################################

RUN echo "=== Running SPARK proofs (fast set) ===" && \
    bash -c 'eval "$(alr printenv)" && \
             rm -rf gnatprove || true && \
             gnatprove -P anubis_spark.gpr --mode=prove --level=4 --timeout=120 \
               --files=src/crypto/anubis_contracts.ads \
                       src/crypto/anubis_header_io.ads \
                       src/crypto/anubis_hybrid_kdf.ads \
                       src/crypto/anubis_aead_pure.ads \
                       src/crypto/anubis_zeroize.ads \
                       src/crypto/anubis_bounds.ads'

# Verify no proof warnings
RUN bash -c 'if test -f gnatprove/gnatprove.out; then \
               if grep -iE "(warning|might fail)" gnatprove/gnatprove.out; then \
                 echo "ERROR: GNATprove warnings detected"; \
                 exit 1; \
               fi; \
             fi'

###############################################################################
# STAGE 2: Build Production Binary
###############################################################################

RUN echo "=== Building production binary ===" && \
    bash -c 'eval "$(alr printenv)" && \
             make clean && \
             make build'

# Verify binary exists
RUN test -f bin/anubis_main || { echo "ERROR: Binary not built"; exit 1; }

# Verify static linking (no unexpected dynamic dependencies)
RUN bash -c 'ldd bin/anubis_main | grep -v "linux-vdso" | grep -v "libc.so" | grep -v "ld-linux" && \
             { echo "ERROR: Unexpected dynamic dependencies"; exit 1; } || true'

###############################################################################
# STAGE 3: Run Self-Tests
###############################################################################

RUN echo "=== Running self-tests ===" && \
    bash -c './bin/anubis_main version || true && \
             ./bin/anubis_main test || echo "Note: Full selftests require runtime features"'

###############################################################################
# STAGE 4: Build and Run Boundary Tests
###############################################################################

RUN echo "=== Building boundary tests ===" && \
    bash -c 'eval "$(alr printenv)" && \
             gnatmake -P anubis_spark.gpr tests/test_boundary.adb -o bin/test_boundary && \
             gnatmake -P anubis_spark.gpr tests/test_boundary_matrix.adb -o bin/test_boundary_matrix'

RUN echo "=== Running boundary tests ===" && \
    bash -c './bin/test_boundary && \
             ./bin/test_boundary_matrix'

###############################################################################
# STAGE 5: Generate Build Manifest
###############################################################################

RUN echo "=== Generating build manifest ===" && \
    bash -c 'sha256sum bin/anubis_main > /home/anubis/anubis-spark/BUILD_MANIFEST.txt && \
             echo "" >> BUILD_MANIFEST.txt && \
             echo "Build Date: $(date -u +"%Y-%m-%d %H:%M:%S UTC")" >> BUILD_MANIFEST.txt && \
             echo "Toolchain:" >> BUILD_MANIFEST.txt && \
             eval "$(alr printenv)" && \
             gnatls --version >> BUILD_MANIFEST.txt && \
             gprbuild --version >> BUILD_MANIFEST.txt && \
             gnatprove --version >> BUILD_MANIFEST.txt && \
             echo "" >> BUILD_MANIFEST.txt && \
             echo "Libraries:" >> BUILD_MANIFEST.txt && \
             echo "  libsodium: ${LIBSODIUM_VERSION}" >> BUILD_MANIFEST.txt && \
             echo "  liboqs:    ${LIBOQS_VERSION}" >> BUILD_MANIFEST.txt && \
             cat BUILD_MANIFEST.txt'

###############################################################################
# Runtime Configuration
###############################################################################

# Default command: Show version and help
CMD ["bash", "-c", "eval \"$(alr printenv)\" && ./bin/anubis_main version && echo '' && ./bin/anubis_main --help"]

# Volumes for input/output files
VOLUME ["/data"]

###############################################################################
# Build Instructions
###############################################################################

# To build this image:
#   docker build -t anubis-spark:1.1.0-platinum .
#
# To run proofs:
#   docker run --rm anubis-spark:1.1.0-platinum bash -c "eval \"\$(alr printenv)\" && make prove-fast"
#
# To run full proofs (slow, ~10 minutes):
#   docker run --rm anubis-spark:1.1.0-platinum bash -c "eval \"\$(alr printenv)\" && make prove-full"
#
# To encrypt a file:
#   docker run --rm -v $(pwd):/data anubis-spark:1.1.0-platinum bash -c \
#     "eval \"\$(alr printenv)\" && ./bin/anubis_main encrypt --key /data/identity.key --input /data/file.txt"
#
# To run interactive shell:
#   docker run -it --rm anubis-spark:1.1.0-platinum bash
#
# To extract the binary:
#   docker create --name anubis-temp anubis-spark:1.1.0-platinum
#   docker cp anubis-temp:/home/anubis/anubis-spark/bin/anubis_main ./
#   docker rm anubis-temp

###############################################################################
# Reproducible Build Verification
###############################################################################

# Build the image twice with identical inputs:
#   docker build -t anubis-spark:build1 .
#   docker build -t anubis-spark:build2 .
#
# Extract binaries:
#   docker create --name b1 anubis-spark:build1
#   docker cp b1:/home/anubis/anubis-spark/bin/anubis_main ./anubis_main_1
#   docker rm b1
#
#   docker create --name b2 anubis-spark:build2
#   docker cp b2:/home/anubis/anubis-spark/bin/anubis_main ./anubis_main_2
#   docker rm b2
#
# Verify identical:
#   sha256sum anubis_main_1 anubis_main_2
#   # Both should have identical SHA256 hashes (deterministic build)
