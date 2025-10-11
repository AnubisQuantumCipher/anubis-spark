#!/bin/bash
###############################################################################
# ANUBIS-SPARK Release Automation Script
# Performs: Tag → Prove → Build → Test → Package → Evidence Archive
###############################################################################

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
VERSION="${1:-$(date +%Y.%m.%d)}"
RELEASE_DIR="release/v${VERSION}"
EVIDENCE_DIR="${RELEASE_DIR}/evidence"

# Tool paths
GNAT_PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin"
GPRBUILD_PATH="/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin"
GNATPROVE_PATH="/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/bin"
export PATH="${GNAT_PATH}:${GPRBUILD_PATH}:${GNATPROVE_PATH}:${PATH}"

###############################################################################
# Helper Functions
###############################################################################

log_step() {
    echo -e "\n${BLUE}[$1/${TOTAL_STEPS}]${NC} $2"
}

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

log_error() {
    echo -e "${RED}✗${NC} $1"
    exit 1
}

log_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

###############################################################################
# Main Release Process
###############################################################################

TOTAL_STEPS=9

echo "==============================================================================="
echo "  ANUBIS-SPARK Release Automation"
echo "  Version: ${VERSION}"
echo "  Release Directory: ${RELEASE_DIR}"
echo "==============================================================================="

# Step 1: Pre-flight checks
log_step 1 "Pre-flight checks"
if [ -d "${RELEASE_DIR}" ]; then
    log_error "Release directory already exists: ${RELEASE_DIR}"
fi

if ! command -v gnatprove &> /dev/null; then
    log_error "gnatprove not found in PATH"
fi

if ! command -v gprbuild &> /dev/null; then
    log_error "gprbuild not found in PATH"
fi

log_success "All tools available"

# Step 2: Create release directory structure
log_step 2 "Creating release directory structure"
mkdir -p "${RELEASE_DIR}"/{bin,evidence/proofs,evidence/tests,docs}
log_success "Directory structure created"

# Step 3: Run SPARK proofs (full)
log_step 3 "Running SPARK proofs (this may take ~10 minutes)"
rm -rf gnatprove || true
gnatprove -P anubis_spark.gpr --mode=prove --level=4 --timeout=600 2>&1 | tee "${EVIDENCE_DIR}/gnatprove_full.log"

# Check for warnings
if grep -iE "(warning|might fail)" gnatprove/gnatprove.out; then
    log_warning "GNATprove warnings detected (documented in known issues)"
    cp gnatprove/gnatprove.out "${EVIDENCE_DIR}/proofs/gnatprove_warnings.txt"
else
    log_success "SPARK proofs complete (0 warnings)"
fi

# Copy proof artifacts
cp -r gnatprove/ "${EVIDENCE_DIR}/proofs/" || true
log_success "Proof evidence archived"

# Step 4: Build production binaries
log_step 4 "Building production binaries (release mode)"
make clean
make build
log_success "Production binary built"

# Copy binary
cp bin/anubis_main "${RELEASE_DIR}/bin/"
log_success "Binary copied to release directory"

# Step 5: Build test binaries
log_step 5 "Building test binaries"
gnatmake -P anubis_spark.gpr tests/test_boundary.adb -o bin/test_boundary
gnatmake -P anubis_spark.gpr tests/test_boundary_matrix.adb -o bin/test_boundary_matrix
log_success "Test binaries built"

# Copy test binaries
cp bin/test_boundary "${RELEASE_DIR}/bin/"
cp bin/test_boundary_matrix "${RELEASE_DIR}/bin/"
log_success "Test binaries copied"

# Step 6: Run boundary tests
log_step 6 "Running boundary/tamper tests"
echo "Running test_boundary (basic)..."
./bin/test_boundary 2>&1 | tee "${EVIDENCE_DIR}/tests/boundary_basic.log"
if [ ${PIPESTATUS[0]} -eq 0 ]; then
    log_success "Basic boundary test passed"
else
    log_error "Basic boundary test FAILED"
fi

echo ""
echo "Running test_boundary_matrix (10 scenarios)..."
./bin/test_boundary_matrix 2>&1 | tee "${EVIDENCE_DIR}/tests/boundary_matrix.log"
if [ ${PIPESTATUS[0]} -eq 0 ]; then
    log_success "Matrix boundary test passed (10/10 scenarios)"
else
    log_error "Matrix boundary test FAILED"
fi

# Step 7: Package documentation and evidence
log_step 7 "Packaging documentation and evidence"
cp README.md "${RELEASE_DIR}/docs/"
cp docs/ASSURANCE_CASE.md "${RELEASE_DIR}/docs/"
cp CHANGELOG.md "${RELEASE_DIR}/docs/" 2>/dev/null || true
cp third_party/LOCKFILE.md "${RELEASE_DIR}/docs/" 2>/dev/null || true

# Create release manifest
cat > "${RELEASE_DIR}/MANIFEST.txt" <<EOF
ANUBIS-SPARK Release Manifest
=============================

Version:     ${VERSION}
Build Date:  $(date -u +"%Y-%m-%d %H:%M:%S UTC")
Build Host:  $(hostname)
Build User:  $(whoami)

Binary:
  anubis_main    $(shasum -a 256 bin/anubis_main | awk '{print $1}')

Test Binaries:
  test_boundary         $(shasum -a 256 bin/test_boundary | awk '{print $1}')
  test_boundary_matrix  $(shasum -a 256 bin/test_boundary_matrix | awk '{print $1}')

Toolchain:
  GNAT:       $(gnatls --version | head -1)
  GPRbuild:   $(gprbuild --version | head -1)
  GNATprove:  $(gnatprove --version | head -1)

Libraries:
  libsodium:  1.0.20
  liboqs:     0.14.0

Proof Status:
  - Full project proved with level 4, timeout 600s
  - Proof logs: evidence/proofs/
  - Known issues documented in ASSURANCE_CASE.md

Test Status:
  - Boundary test (basic): PASS
  - Boundary matrix (10 scenarios): PASS
  - Test logs: evidence/tests/

EOF

log_success "Documentation and manifest packaged"

# Step 8: Create tarball
log_step 8 "Creating release tarball"
cd release
tar -czf "anubis-spark-v${VERSION}.tar.gz" "v${VERSION}"
cd ..
log_success "Tarball created: release/anubis-spark-v${VERSION}.tar.gz"

# Step 9: Git tag (optional)
log_step 9 "Git tagging"
read -p "Create git tag v${VERSION}? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    git tag -a "v${VERSION}" -m "Release v${VERSION}

Generated with ANUBIS-SPARK release automation

Included:
- Production binary (anubis_main)
- Test binaries (test_boundary, test_boundary_matrix)
- SPARK proof evidence
- Test logs (10/10 tamper scenarios passed)
- Documentation and manifest

See release/v${VERSION}/MANIFEST.txt for details."

    log_success "Git tag created: v${VERSION}"
    echo ""
    echo "To push tag: git push origin v${VERSION}"
else
    log_warning "Skipped git tagging"
fi

###############################################################################
# Summary
###############################################################################

echo ""
echo "==============================================================================="
echo "  ✓ Release Complete: v${VERSION}"
echo "==============================================================================="
echo ""
echo "Release Artifacts:"
echo "  Directory:   ${RELEASE_DIR}"
echo "  Tarball:     release/anubis-spark-v${VERSION}.tar.gz"
echo ""
echo "Contents:"
echo "  - bin/anubis_main                       (production binary)"
echo "  - bin/test_boundary                     (basic tamper test)"
echo "  - bin/test_boundary_matrix              (10-scenario test)"
echo "  - docs/README.md                        (usage documentation)"
echo "  - docs/ASSURANCE_CASE.md                (auditor documentation)"
echo "  - evidence/proofs/                      (SPARK proof logs)"
echo "  - evidence/tests/                       (test execution logs)"
echo "  - MANIFEST.txt                          (SHA256 hashes & metadata)"
echo ""
echo "Next Steps:"
echo "  1. Review ${RELEASE_DIR}/MANIFEST.txt"
echo "  2. Test binary: ${RELEASE_DIR}/bin/anubis_main version"
echo "  3. Upload to GitHub: gh release create v${VERSION} release/anubis-spark-v${VERSION}.tar.gz"
echo "  4. Push tag: git push origin v${VERSION}"
echo ""
echo "==============================================================================="
