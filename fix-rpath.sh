#!/bin/bash
# Fix duplicate LC_RPATH on macOS 15.4+ (Sequoia)
# This script removes duplicate RPATH entries that cause crashes on macOS 15.4+

RPATH_TO_REMOVE="/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib"

echo "Fixing duplicate LC_RPATH in binaries..."

for binary in bin/*; do
    if [ -f "$binary" ] && [ -x "$binary" ]; then
        echo "  Fixing: $binary"
        install_name_tool -delete_rpath "$RPATH_TO_REMOVE" "$binary" 2>/dev/null || true
    fi
done

echo "✓ All binaries fixed!"
echo ""
echo "You can now run: ./bin/anubis_main version"
