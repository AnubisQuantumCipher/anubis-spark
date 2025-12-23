# ANUBIS-SPARK Makefile
# Production build and installation

.PHONY: all bootstrap build install uninstall clean test deps prove-fast prove-full prove-now boundary benchmark help

PREFIX ?= $(HOME)/.local
BINDIR = $(PREFIX)/bin
BINARY = anubis_main
INSTALLED_NAME = anubis-spark

# Local directories (created by bootstrap)
TOOLS_DIR := $(CURDIR)/.tools
DEPS_DIR := $(CURDIR)/.deps

# Detect OS
UNAME_S := $(shell uname -s)

# Default target
all: build

#===============================================================================
# Bootstrap - One command setup
#===============================================================================

# Full bootstrap: installs toolchain + deps + builds
bootstrap:
	@./bootstrap

# Bootstrap without building (just setup environment)
bootstrap-env:
	@./bootstrap --skip-build

# Clean bootstrap and start fresh
bootstrap-clean:
	@./bootstrap --clean

#===============================================================================
# Build targets
#===============================================================================

# Build production release (requires bootstrap or manual setup)
build:
	@echo "Building ANUBIS-SPARK (release mode)..."
	@if [ -f "$(CURDIR)/env.sh" ]; then \
		. "$(CURDIR)/env.sh" && alr build --release; \
	elif command -v alr >/dev/null 2>&1; then \
		if [ -n "$$ANUBIS_LIB_DIR" ]; then \
			alr build --release; \
		elif [ -d "$(DEPS_DIR)/lib" ]; then \
			ANUBIS_LIB_DIR="$(DEPS_DIR)/lib" alr build --release; \
		else \
			echo "Error: ANUBIS_LIB_DIR not set. Run './bootstrap' first or set manually."; \
			exit 1; \
		fi; \
	else \
		echo "Error: Alire (alr) not found. Run './bootstrap' first."; \
		exit 1; \
	fi
ifeq ($(UNAME_S),Darwin)
	@echo "Fixing duplicate LC_RPATH in binaries (macOS)..."
	@for bin in bin/*; do \
		[ -f "$$bin" ] && [ -x "$$bin" ] && ./scripts/fix-rpath.sh "$$bin" > /dev/null 2>&1 || true; \
	done
endif
	@echo ""
	@echo "Build complete: bin/$(BINARY)"
	@echo "Run: ./bin/$(BINARY) version"

# Install dependencies from source (standalone, without full bootstrap)
deps:
	@echo "Installing dependencies (libsodium 1.0.20, liboqs 0.14.0)..."
	@./scripts/install-deps.sh --prefix "$(DEPS_DIR)"
	@echo ""
	@echo "Dependencies installed to: $(DEPS_DIR)"
	@echo "Now run: export ANUBIS_LIB_DIR=$(DEPS_DIR)/lib"
	@echo "Then:    make build"

#===============================================================================
# Installation
#===============================================================================

# Install to ~/.local/bin (or PREFIX)
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

#===============================================================================
# Testing
#===============================================================================

# Build and run self-tests
test: build
	@echo "Running self-tests..."
	@./bin/$(BINARY) test

#===============================================================================
# SPARK Verification
#===============================================================================

# SPARK proof: fast (level 1, ~2-5 min)
prove-fast:
	@echo "Running fast SPARK proofs (level 1)..."
	@rm -rf gnatprove || true
	@if [ -f "$(CURDIR)/env.sh" ]; then \
		. "$(CURDIR)/env.sh" && alr exec -- gnatprove -P anubis_spark.gpr --mode=prove --level=1 --timeout=120; \
	elif command -v alr >/dev/null 2>&1; then \
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
	@if [ -f "$(CURDIR)/env.sh" ]; then \
		. "$(CURDIR)/env.sh" && alr exec -- gnatprove -P anubis_spark.gpr --mode=prove --level=4 --timeout=600; \
	elif command -v alr >/dev/null 2>&1; then \
		alr exec -- gnatprove -P anubis_spark.gpr --mode=prove --level=4 --timeout=600; \
	else \
		echo "Error: Alire (alr) not found."; \
		exit 1; \
	fi
	@echo "Full proofs complete"

# Simple proof run
prove-now:
	@echo "Running GNATprove (level 1, 2 min timeout)..."
	@if [ -f "$(CURDIR)/env.sh" ]; then \
		. "$(CURDIR)/env.sh" && alr exec -- gnatprove -P anubis_spark.gpr --mode=prove --level=1 --timeout=120 || true; \
	elif command -v alr >/dev/null 2>&1; then \
		alr exec -- gnatprove -P anubis_spark.gpr --mode=prove --level=1 --timeout=120 || true; \
	fi
	@echo "(See gnatprove/gnatprove.out for details)"

#===============================================================================
# Advanced Testing
#===============================================================================

# Build and run boundary tests
boundary: build
	@echo "Building boundary tests..."
	@if [ -f "$(CURDIR)/env.sh" ]; then \
		. "$(CURDIR)/env.sh" && alr exec -- gnatmake -P anubis_spark.gpr tests/test_boundary.adb -o bin/test_boundary; \
		. "$(CURDIR)/env.sh" && alr exec -- gnatmake -P anubis_spark.gpr tests/test_boundary_matrix.adb -o bin/test_boundary_matrix; \
	elif command -v alr >/dev/null 2>&1; then \
		alr exec -- gnatmake -P anubis_spark.gpr tests/test_boundary.adb -o bin/test_boundary; \
		alr exec -- gnatmake -P anubis_spark.gpr tests/test_boundary_matrix.adb -o bin/test_boundary_matrix; \
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
	@if [ -f "$(CURDIR)/env.sh" ]; then \
		. "$(CURDIR)/env.sh" && alr exec -- gprbuild -P anubis_spark.gpr -XBUILD_MODE=release tests/test_benchmark.adb; \
	elif command -v alr >/dev/null 2>&1; then \
		alr exec -- gprbuild -P anubis_spark.gpr -XBUILD_MODE=release tests/test_benchmark.adb; \
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

#===============================================================================
# Cleanup
#===============================================================================

# Clean build artifacts (keep tools and deps)
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf obj bin gnatprove
	@echo "Clean complete"

# Clean everything including local tools and deps
clean-all: clean
	@echo "Cleaning local tools and dependencies..."
	@rm -rf $(TOOLS_DIR) $(DEPS_DIR) env.sh
	@echo "Full clean complete"

#===============================================================================
# Help
#===============================================================================

help:
	@echo "ANUBIS-SPARK Makefile"
	@echo ""
	@echo "Quick Start (recommended):"
	@echo "  ./bootstrap              Full setup: toolchain + deps + build"
	@echo "  ./bin/anubis_main test   Run self-tests"
	@echo ""
	@echo "Bootstrap Targets:"
	@echo "  make bootstrap           Same as ./bootstrap"
	@echo "  make bootstrap-env       Setup without building"
	@echo "  make bootstrap-clean     Clean and reinstall everything"
	@echo ""
	@echo "Build Targets:"
	@echo "  make build               Build production release"
	@echo "  make deps                Build crypto libraries only"
	@echo "  make install             Install to ~/.local/bin"
	@echo "  make uninstall           Remove installed binary"
	@echo "  make test                Run self-tests"
	@echo ""
	@echo "SPARK Verification:"
	@echo "  make prove-fast          Fast proofs (level 1, ~5 min)"
	@echo "  make prove-full          Full proofs (level 4, ~20 min)"
	@echo "  make boundary            Run boundary/tamper tests"
	@echo "  make benchmark           Run performance benchmarks"
	@echo ""
	@echo "Cleanup:"
	@echo "  make clean               Remove build artifacts"
	@echo "  make clean-all           Remove everything (tools, deps, artifacts)"
	@echo ""
	@echo "Examples:"
	@echo "  ./bootstrap && ./bin/anubis_main version"
	@echo "  make install PREFIX=/usr/local"
	@echo ""
