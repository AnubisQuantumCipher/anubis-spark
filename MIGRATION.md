# Migration Guide: ANUB2 → ANUB3 (ANUBIS‑SPARK v2.0.0)

This document describes the safest upgrade path from legacy ANUB2 files and v1.x trust records to ANUB3 and v2.0.0.

## Summary

- Legacy trust records (v1.x) are treated as corrupt in v2.0.0 (no HMAC). Re‑approve identities with v2.0.0.
- Manual migration is required for ciphertexts:
  1) Decrypt legacy ANUB2 files with a v1.x binary you trust
  2) Re‑encrypt the plaintext with v2.0.0 using `convert`
- Auto‑conversion of ciphertext is intentionally not supported — it could hide incompatibilities and weaken assurances.

## Steps

### 1) Find legacy ANUB2 files

Use the helper script to locate legacy ciphertexts and stage conversions:

```bash
scripts/migrate_anub2.sh /absolute/path/to/ROOT /path/to/id.key migrated
```

What it does:
- Scans `ROOT` for `*.anubis`
- Prints v1.x decrypt commands you should run manually (one per file)
- If a `file.decrypted` exists next to a legacy ciphertext, it will re‑encrypt it to ANUB3 via:
  `anubis-spark convert --key /path/to/id.key --input file.decrypted --output file.decrypted.anub3 --label migrated --force`

### 2) Decrypt with v1.x

For each legacy ciphertext:

```bash
# Example (run with your v1.x binary)
anubis-spark-1.x decrypt --key old.key --input file.anubis --output file.decrypted
```

### 3) Re‑encrypt plaintext with v2.0.0

```bash
anubis-spark convert \
  --key /path/to/id.key \
  --input /absolute/path/to/file.decrypted \
  --output /absolute/path/to/file.decrypted.anub3 \
  --label migrated \
  --force
```

Notes:
- `convert` refuses ciphertext inputs (ANUB2 or ANUB3). It expects plaintext.
- Dual signatures (Ed25519 + ML‑DSA‑87) and signer metadata are applied in ANUB3.

## Trust: Re‑approve in v2.0.0

- v1.x trust records lack HMACs; v2.0.0 treats them as unreadable for security.
- Re‑approve each identity in v2.0.0:

```bash
# Example
anubis-spark trust approve --fingerprint <hex> [--operator <name>]
```

- Health check:

```bash
anubis-spark trust doctor
```

It warns if the local HMAC key `~/.anubis/trust/.hmac.key` lacks safe POSIX perms (should be 600).

## Chunk size control (optional)

64 MiB is the default chunk size. Override via `--chunk-size <bytes>`:

```bash
anubis-spark encrypt  --key id.key --input file --output file.anubis --chunk-size 33554432
anubis-spark convert  --key id.key --input plaintext --output out.anub3 --chunk-size 33554432
```

## Why manual migration?

- **Transparency**: You explicitly decrypt with v1.x, then re‑encrypt with v2.0.0 — no hidden conversions.
- **Security**: You confirm inputs are plaintext before re‑encryption; v2.0.0 applies dual signatures and trust metadata.
- **Auditability**: Each re‑encrypted file bears the signer label, timestamp, fingerprint, and mandatory signatures.

## Troubleshooting

- “Input appears to be ciphertext (ANUB2/ANUB3)” on convert:
  - Decrypt first (v1.x for ANUB2; v2.x for ANUB3) — then run convert on the plaintext.
- “Trust record unreadable/corrupt”:
  - Re‑approve signer fingerprints in v2.0.0; legacy trust entries lack HMAC and are rejected.
- “HMAC key perms are … expected 600”:
  - Fix with `chmod 600 ~/.anubis/trust/.hmac.key`.

