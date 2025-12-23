# ANUBIS-SPARK Makefile
# Production build and installation

.PHONY: all build install uninstall clean test deps prove-fast prove-full prove-now boundary benchmark help

PREFIX ?= $(HOME)/.local
BINDIR = $(PREFIX)/bin
BINARY = anubis_main
INSTALLED_NAME = anubis-spark

# Detect OS
UNAME_S := $(shell uname -s)

# Default target
all: build

# Install dependencies from source (required for most Linux distros)
deps:
	@echo "Installing dependencies (libsodium 1.0.20, liboqs 0.14.0)..."
	@./scripts/install-deps.sh --prefix $(HOME)/anubis-deps
	@echo ""
	@echo "Now run: export ANUBIS_LIB_DIR=$(HOME)/anubis-deps/lib"
	@echo "Then:    make build"

# Build production release
build:
	@echo "Building ANUBIS-SPARK (release mode)..."
	@if command -v alr >/dev/null 2>&1; then \
		alr build --release; \
	else \
		echo "Error: Alire (alr) not found. Install from: https://alire.ada.dev"; \
		exit 1; \
	fi
ifeq ($(UNAME_S),Darwin)
	@echo "Fixing duplicate LC_RPATH in binaries (macOS)..."
	@for bin in bin/*; do \
		[ -f "$$bin" ] && [ -x "$$bin" ] && ./scripts/fix-rpath.sh "$$bin" > /dev/null 2>&1 || true; \
	done
	@echo "All binaries fixed!"
endif
	@echo ""
	@echo "Build complete: bin/$(BINARY)"
	@echo "Run: ./bin/$(BINARY) version"

# Install to ~/.local/bin
install: build
	@echo "Installing ANUBIS-SPARK to $(BINDIR)..."
	@mkdir -p $(BINDIR)
	@cp bin/$(BINARY) $(BINDIR)/$(INSTALLED_NAME)
	@chmod 755 $(BINDIR)/$(INSTALLED_NAME)
	@echo "Installed: $(BINDIR)/$(INSTALLED_NAME)"
	@echo ""
	@echo "Run 'anubis-spark version' to verify installation"
	@echo "Make sure $(BINDIR) is in your PATH"

# Uninstall
uninstall:
	@echo "Uninstalling ANUBIS-SPARK..."
	@rm -f $(BINDIR)/$(INSTALLED_NAME)
	@echo "Uninstalled"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf obj bin gnatprove
	@echo "Clean complete"

# Build and run self-tests
test: build
	@echo "Running self-tests..."
	@./bin/$(BINARY) test

# SPARK proof: fast (level 1, ~2-5 min)
prove-fast:
	@echo "Running fast SPARK proofs (level 1)..."
	@rm -rf gnatprove || true
	@if command -v alr >/dev/null 2>&1; then \
		alr exec -- gnatprove -P anubis_spark.gpr --mode=prove --level=1 --timeout=120; \
	else \
		echo "Error: Alire (alr) not found."; \
		exit 1; \
	fi
	@echo "Fast proofs complete"

# SPARK proof: full (level 4, ~10-20 min)
prove-full:
	@echo "Running full SPARK proofs (level 4)..."
	@rm -rf gnatprove || true
	@if command -v alr >/dev/null 2>&1; then \
		alr exec -- gnatprove -P anubis_spark.gpr --mode=prove --level=4 --timeout=600; \
	else \
		echo "Error: Alire (alr) not found."; \
		exit 1; \
	fi
	@echo "Full proofs complete"

# Simple proof run
prove-now:
	@echo "Running GNATprove (level 1, 2 min timeout)..."
	@if command -v alr >/dev/null 2>&1; then \
		alr exec -- gnatprove -P anubis_spark.gpr --mode=prove --level=1 --timeout=120 || true; \
	else \
		echo "Error: Alire (alr) not found."; \
		exit 1; \
	fi
	@echo "(See gnatprove/gnatprove.out for details)"

# Build and run boundary tests
boundary: build
	@echo "Building boundary tests..."
	@if command -v alr >/dev/null 2>&1; then \
		alr exec -- gnatmake -P anubis_spark.gpr tests/test_boundary.adb -o bin/test_boundary; \
		alr exec -- gnatmake -P anubis_spark.gpr tests/test_boundary_matrix.adb -o bin/test_boundary_matrix; \
	else \
		echo "Error: Alire (alr) not found."; \
		exit 1; \
	fi
ifeq ($(UNAME_S),Darwin)
	@for bin in bin/test_*; do \
		[ -f "$$bin" ] && ./scripts/fix-rpath.sh "$$bin" > /dev/null 2>&1 || true; \
	done
endif
	@echo "Boundary tests built"
	@echo ""
	@echo "Running boundary test (basic)..."
	@./bin/test_boundary
	@echo ""
	@echo "Running boundary matrix test (10 scenarios)..."
	@./bin/test_boundary_matrix
	@echo ""
	@echo "All boundary tests passed"

# Performance benchmarks
benchmark: build
	@echo "Building performance benchmark suite..."
	@if command -v alr >/dev/null 2>&1; then \
		alr exec -- gprbuild -P anubis_spark.gpr -XBUILD_MODE=release tests/test_benchmark.adb; \
	else \
		echo "Error: Alire (alr) not found."; \
		exit 1; \
	fi
ifeq ($(UNAME_S),Darwin)
	@for bin in bin/*; do \
		[ -f "$$bin" ] && ./scripts/fix-rpath.sh "$$bin" > /dev/null 2>&1 || true; \
	done
endif
	@echo "Benchmark built"
	@echo ""
	@echo "Running performance benchmarks (this may take 5-10 minutes)..."
	@./bin/test_benchmark
	@echo ""
	@echo "Benchmarks complete"

# Help
help:
	@echo "ANUBIS-SPARK Makefile"
	@echo ""
	@echo "Prerequisites:"
	@echo "  1. Install Alire: curl -fsSL https://alire.ada.dev/install.sh | sh"
	@echo "  2. Install deps:  make deps"
	@echo "  3. Set lib path:  export ANUBIS_LIB_DIR=\$$HOME/anubis-deps/lib"
	@echo ""
	@echo "Build Targets:"
	@echo "  make deps        - Build libsodium 1.0.20 & liboqs 0.14.0 from source"
	@echo "  make build       - Build production release binary"
	@echo "  make install     - Install to ~/.local/bin (or PREFIX)"
	@echo "  make uninstall   - Remove installed binary"
	@echo "  make clean       - Clean build artifacts"
	@echo "  make test        - Run self-tests"
	@echo ""
	@echo "SPARK Verification:"
	@echo "  make prove-fast  - Fast SPARK proofs (level 1, ~5 min)"
	@echo "  make prove-full  - Full SPARK proofs (level 4, ~20 min)"
	@echo "  make boundary    - Build and run boundary/tamper tests"
	@echo "  make benchmark   - Build and run performance benchmarks"
	@echo ""
	@echo "  make help        - Show this help"
	@echo ""
	@echo "Examples:"
	@echo "  make deps && export ANUBIS_LIB_DIR=\$$HOME/anubis-deps/lib && make install"
	@echo "  make install PREFIX=/usr/local"
	@echo ""
