#!/usr/bin/env bash
#
# fix-rpath.sh - Remove duplicate RPATH entries on macOS
#
# macOS 15.0+ (Sequoia) enforces stricter RPATH duplicate checking.
# The Ada toolchain (GNAT/gprbuild) sometimes creates duplicate RPATH
# entries which cause dyld to abort with "duplicate LC_RPATH" errors.
#
# This script removes duplicate RPATH entries from the compiled binary.
#
# Usage:
#   ./scripts/fix-rpath.sh [binary_path]
#
# If no binary_path is provided, defaults to ./bin/anubis_main

set -euo pipefail

BINARY="${1:-./bin/anubis_main}"

if [[ ! -f "$BINARY" ]]; then
    echo "Error: Binary not found: $BINARY"
    echo "Usage: $0 [binary_path]"
    exit 1
fi

if [[ "$(uname -s)" != "Darwin" ]]; then
    echo "This script is only needed on macOS. Skipping RPATH fix."
    exit 0
fi

echo "Fixing duplicate RPATH entries in: $BINARY"
echo ""

# Get all RPATH entries
RPATHS=$(otool -l "$BINARY" | grep -A 2 LC_RPATH | grep "path " | awk '{print $2}')

# Find duplicates
DUPLICATES=$(echo "$RPATHS" | sort | uniq -d)

if [[ -z "$DUPLICATES" ]]; then
    echo "✓ No duplicate RPATH entries found. Binary is ready to use."
    exit 0
fi

echo "Found duplicate RPATH entries:"
echo "$DUPLICATES"
echo ""

# Remove duplicates (keeps first occurrence)
while IFS= read -r rpath; do
    if [[ -n "$rpath" ]]; then
        echo "Removing duplicate: $rpath"
        install_name_tool -delete_rpath "$rpath" "$BINARY" 2>/dev/null || {
            echo "  (already removed or not found)"
        }
    fi
done <<< "$DUPLICATES"

echo ""
echo "✓ RPATH fix complete!"
echo ""
echo "Verifying binary works:"
"$BINARY" version > /dev/null 2>&1 && {
    echo "✓ Binary runs successfully"
} || {
    echo "✗ Binary still has issues. Please check manually:"
    echo "  otool -l $BINARY | grep -A 2 LC_RPATH"
    exit 1
}
