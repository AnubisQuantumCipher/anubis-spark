#!/usr/bin/env bash
set -euo pipefail

root="${1:-.}"
id="${2:-$HOME/.anubis/id.key}"
label="${3:-migrated}"

echo "==> Scanning for legacy ANUB2 files under: $root"
mapfile -t files < <(find "$root" -type f -name "*.anubis" -print 2>/dev/null)

if (( ${#files[@]} == 0 )); then
  echo "No .anubis files found."
  exit 0
fi

legacy=()
for f in "${files[@]}"; do
  if head -c 5 "$f" | LC_ALL=C tr -cd '\11\12\15\40-\176' | grep -q '^ANUB2$'; then
    legacy+=("$f")
  fi
done

if (( ${#legacy[@]} == 0 )); then
  echo "No ANUB2 files detected."
  exit 0
fi

echo "\n==> Found ${#legacy[@]} ANUB2 files:"; printf ' - %s\n' "${legacy[@]}"

echo "\n==> Step 1: Decrypt with v1.x (manual)"
for c in "${legacy[@]}"; do
  plain="${c%.anubis}.decrypted"
  echo "# v1.x decrypt (example): anubis-spark-1.x decrypt --key <old.key> --input '$c' --output '$plain'"
done

echo "\n==> Step 2: Re-encrypt to ANUB3 with v2.0.0 (automatic)"
for c in "${legacy[@]}"; do
  plain="${c%.anubis}.decrypted"
  out="${plain}.anub3"
  if [[ -f "$plain" ]]; then
    echo "Converting '$plain' -> '$out'"
    anubis-spark convert --key "$id" --input "$plain" --output "$out" --label "$label" --force || {
      echo "ERROR: convert failed for $plain" >&2
    }
  else
    echo "SKIP: plaintext missing (did you decrypt with v1.x?): $plain"
  fi
done

echo "\nDone."
