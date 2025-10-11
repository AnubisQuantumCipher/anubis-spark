# ANUBIS-SPARK Makefile
# Production build and installation

.PHONY: all build install uninstall clean test help

PREFIX ?= $(HOME)/.local
BINDIR = $(PREFIX)/bin
BINARY = anubis_main
INSTALLED_NAME = anubis-spark

# Default target
all: build

# Build production release
build:
	@echo "Building ANUBIS-SPARK v1.1.0 (release mode)..."
	@PATH="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/bin:/Users/sicarii/.local/share/alire/toolchains/gprbuild_24.0.1_6f6b6658/bin:$$PATH" \
		gprbuild -P anubis_spark.gpr -XBUILD_MODE=release
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
	@echo "✓ Tests built in bin/"

# Help
help:
	@echo "ANUBIS-SPARK v1.1.0 Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  make build      - Build production release binary"
	@echo "  make install    - Install to ~/.local/bin (or PREFIX)"
	@echo "  make uninstall  - Remove installed binary"
	@echo "  make clean      - Clean build artifacts"
	@echo "  make test       - Build test suite"
	@echo "  make help       - Show this help"
	@echo ""
	@echo "Installation:"
	@echo "  make install PREFIX=/usr/local  # Install to /usr/local/bin"
	@echo ""
