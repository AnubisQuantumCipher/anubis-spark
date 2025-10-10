ANUBIS-SPARK — status snapshot
================================

What it is: a SPARK/Ada hybrid, post-quantum file-encryption toolkit.
Classical crypto via libsodium, PQC via liboqs, with SPARK proofs around the Ada layer.
CLI entry: `src/anubis_main.adb`. Crypto/FFI lives under `src/crypto/**`.

Build (macOS/Apple Silicon today)
---------------------------------
```
# prereqs: GNAT ≥14, gprbuild, libsodium, liboqs (Homebrew)
alr exec -- gprbuild -P anubis_spark.gpr   # BUILD_MODE=release optional
export PATH="$PWD/bin:$PATH"
```

Note: `anubis_spark.gpr:39` points to `/opt/homebrew` static libs; adjust for other platforms.
Set `ANUBIS_LIB_DIR=/path/to/lib` to override the default for non-Homebrew layouts.
Use `ANUBIS_SOFT_ONLY=on` to force a “soft-only” static link of liboqs/libsodium (still falls back to system `libssl/libcrypto` on macOS). After building, run `scripts/check-soft-only.sh` to confirm no dynamic deps remain.

CLI quickstart
--------------
```
anubis_main keygen --output identity.key
anubis_main encrypt --input file --output file.anubis --key identity.key
anubis_main decrypt --input file.anubis --output file.dec --key identity.key
anubis_main test     # ML-KEM, ML-DSA, hybrid self-test
```

(Currently “self-encrypt”; recipient selection is WIP.)

Code map
--------
- `anubis_types.ads/.adb` — secure key wrappers (zeroizing, SPARK contracts)
- `anubis_types-classical.*` — libsodium FFI (guarded with pre/post)
- `anubis_types-pqc.*` — liboqs FFI + hybrid orchestration
- `anubis_types-file_crypto.adb` — file encrypt/decrypt (+ experimental streaming)
- `anubis_types-sss.*` — Shamir secret sharing
- `anubis_types-storage.*` — identity persistence
- `anubis_key_manager.*` — lightweight key manager

Tests & proofs
--------------
- `bin/test_minimal`, `bin/test_pqc` — FFI round-trips
- SPARK reports: `gnatprove_100percent.txt`, `SPARK_PROOF_SUMMARY.md`, `PROOF_CERTIFICATE.md`  
  (run with `gnatprove --level=2` per README)

🚩 Significant caveats (security relevant)
------------------------------------------
1. Streaming encrypt/decrypt currently disabled at runtime.  
   Large files fall back to in-memory handling until per-chunk nonces/tags are implemented.
2. Hybrid secret now stores both classical and PQ shares, but the API still assumes a single recipient.
3. Self-encryption only.  
   CLI uses one identity for sender+recipient; no multi-party yet.
4. Build portability.  
   GPR paths hard-coded for Homebrew; no Linux/Windows packaging yet.
5. Sample keys in repo are test artifacts — do not use in real ops.

Suggested next steps (priority order)
-------------------------------------
1. Streaming fix: implement per-chunk nonces/tags; persist and verify; add KATs.
2. Recipients on CLI: `encrypt --to alice.key [--to bob.key]`; verify signer on decrypt.
3. GPR portability: parameterize lib paths or declare Alire deps; add Linux build.
4. Sign/verify commands: expose hybrid signatures for files/manifests.
5. SPARK proofs: bump gnatprove level on file-crypto + key manager; add postconditions for “decrypt(encrypt)=id”.
6. Docs & packaging: INSTALL for Linux; signed release tarball.

tl;dr: the macOS build works, small-file encrypt/decrypt is good, PQC round-trips pass, and SPARK proof artifacts are in the tree. The streaming nonce/tag bug is the main blocker to large-file support — fix that first, then add multi-recipient CLI and portable builds.
