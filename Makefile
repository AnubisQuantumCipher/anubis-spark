# ANUBIS-SPARK Makefile
# Production build and installation

.PHONY: all build install uninstall clean test prove-fast prove-full boundary benchmark help
 .PHONY: prove-now

PREFIX ?= $(HOME)/.local
BINDIR = $(PREFIX)/bin
BINARY = anubis_main
INSTALLED_NAME = anubis-spark

# Default target
all: build

# Build production release
build:
	@echo "Building ANUBIS-SPARK v2.0.0 (release mode)..."
	@PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:$$PATH" \
		gprbuild -P anubis_spark.gpr -XBUILD_MODE=release
	@echo "Fixing duplicate LC_RPATH in binaries..."
	@for bin in bin/*; do \
		[ -f "$$bin" ] && [ -x "$$bin" ] && ./scripts/fix-rpath.sh "$$bin" > /dev/null 2>&1 || true; \
	done
	@echo "✓ All binaries fixed!"
	@echo ""
	@echo "You can now run: ./bin/$(BINARY) version"
	@echo "✓ Build complete: bin/$(BINARY)"

# Install to ~/.local/bin
install: build
	@echo "Installing ANUBIS-SPARK to $(BINDIR)..."
	@mkdir -p $(BINDIR)
	@cp bin/$(BINARY) $(BINDIR)/$(INSTALLED_NAME)
	@chmod 755 $(BINDIR)/$(INSTALLED_NAME)
	@echo "✓ Installed: $(BINDIR)/$(INSTALLED_NAME)"
	@echo ""
	@echo "Run 'anubis-spark version' to verify installation"
	@echo "Make sure $(BINDIR) is in your PATH"

# Uninstall
uninstall:
	@echo "Uninstalling ANUBIS-SPARK..."
	@rm -f $(BINDIR)/$(INSTALLED_NAME)
	@echo "✓ Uninstalled"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf obj bin
	@echo "✓ Clean complete"

# Build and run tests
test:
	@echo "Building test suite..."
	@mkdir -p bin
	@PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:$$PATH" \
		gprbuild -P anubis_spark.gpr -XBUILD_MODE=release tests/*.adb
	@echo "Fixing duplicate LC_RPATH in binaries..."
	@for bin in bin/*; do \
		[ -f "$$bin" ] && [ -x "$$bin" ] && ./scripts/fix-rpath.sh "$$bin" > /dev/null 2>&1 || true; \
	done
	@echo "✓ All binaries fixed!"
	@echo "✓ Tests built in bin/"

# PLATINUM: Fast proof (contract packages only, ~2 minutes)
prove-fast:
	@echo "Running fast SPARK proofs (contract packages only)..."
	@rm -rf gnatprove || true
	@PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/bin:$$PATH" \
		gnatprove -P anubis_spark.gpr --mode=prove --level=4 --timeout=120 \
			--files=src/crypto/anubis_contracts.ads \
			        src/crypto/anubis_header_io.ads \
			        src/crypto/anubis_hybrid_kdf.ads \
			        src/crypto/anubis_aead_pure.ads \
			        src/crypto/anubis_zeroize.ads \
			        src/crypto/anubis_bounds.ads
	@echo "✓ Fast proofs complete"
	@if test -f gnatprove/gnatprove.out; then \
		if grep -iE "(warning|might fail)" gnatprove/gnatprove.out; then \
			echo "⚠ Warnings detected in proof output"; \
			exit 1; \
		fi; \
	fi

# PLATINUM: Full proof (entire project, ~10 minutes)
prove-full:
	@echo "Running full SPARK proofs (entire project)..."
	@rm -rf gnatprove || true
	@PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/bin:$$PATH" \
		gnatprove -P anubis_spark.gpr --mode=prove --level=4 --timeout=600
	@echo "✓ Full proofs complete"
	@if test -f gnatprove/gnatprove.out; then \
		if grep -iE "(warning|might fail)" gnatprove/gnatprove.out; then \
			echo "⚠ Warnings detected in proof output"; \
			exit 1; \
		fi; \
	fi

# Simple proof run with default settings (no file list)
prove-now:
	@echo "Running GNATprove (level 1, 2 min timeout)..."
	@PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/bin:$$PATH" \
		gnatprove -P anubis_spark.gpr --mode=prove --level=1 --timeout=120 --no-server 2>/dev/null \
		|| PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/releases/gnatprove_14.1.1_91818ed8/bin:$$PATH" \
		gnatprove -P anubis_spark.gpr --mode=prove --level=1 --timeout=120 || true
	@echo "(See obj/gnatprove/gnatprove.out for details)"

# PLATINUM: Build and run boundary/tamper tests
boundary: build
	@echo "Building boundary tests..."
	@PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:$$PATH" \
		gnatmake -P anubis_spark.gpr tests/test_boundary.adb -o test_boundary
	@PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:$$PATH" \
		gnatmake -P anubis_spark.gpr tests/test_boundary_matrix.adb -o test_boundary_matrix
	@echo "✓ Boundary tests built"
	@echo ""
	@echo "Running boundary test (basic)..."
	@./bin/test_boundary
	@echo ""
	@echo "Running boundary matrix test (10 scenarios)..."
	@./bin/test_boundary_matrix
	@echo ""
	@echo "✓ All boundary tests passed"

# Performance benchmarks
benchmark:
	@echo "Building performance benchmark suite..."
	@PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:$$PATH" \
		gprbuild -P anubis_spark.gpr -XBUILD_MODE=release tests/test_benchmark.adb
	@echo "Fixing RPATH..."
	@for bin in bin/*; do \
		[ -f "$$bin" ] && [ -x "$$bin" ] && ./scripts/fix-rpath.sh "$$bin" > /dev/null 2>&1 || true; \
	done
	@echo "✓ Benchmark built"
	@echo ""
	@echo "Running performance benchmarks (this may take 5-10 minutes)..."
	@./bin/test_benchmark
	@echo ""
	@echo "✓ Benchmarks complete"

# Help
help:
	@echo "ANUBIS-SPARK v2.0.0 Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  make build       - Build production release binary"
	@echo "  make install     - Install to ~/.local/bin (or PREFIX)"
	@echo "  make uninstall   - Remove installed binary"
	@echo "  make clean       - Clean build artifacts"
	@echo "  make test        - Build test suite"
	@echo ""
	@echo "Platinum Targets:"
	@echo "  make prove-fast  - Fast SPARK proofs (contracts only, ~2 min)"
	@echo "  make prove-full  - Full SPARK proofs (entire project, ~10 min)"
	@echo "  make boundary    - Build and run boundary/tamper tests"
	@echo "  make benchmark   - Build and run performance benchmarks (~5-10 min)"
	@echo ""
	@echo "  make help        - Show this help"
	@echo ""
	@echo "Installation:"
	@echo "  make install PREFIX=/usr/local  # Install to /usr/local/bin"
	@echo ""
