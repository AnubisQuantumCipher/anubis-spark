# ANUB3 Header Format Specification

## Overview

ANUB3 is the hybrid post-quantum encrypted file format used by Anubis-SPARK v2.0. It combines classical (X25519, Ed25519) and post-quantum (ML-KEM-1024, ML-DSA-87) cryptographic primitives to provide quantum-resistant confidentiality and authenticity.

## File Structure

```
┌────────────────────────────────────────────────────────────────┐
│                         ANUB3 HEADER                           │
│  (Fixed-size: Magic + Version + Metadata + Crypto Material)   │
├────────────────────────────────────────────────────────────────┤
│                      ENCRYPTED PAYLOAD                         │
│                    (Variable-length chunks)                    │
└────────────────────────────────────────────────────────────────┘
```

## Header Layout (Binary)

### Section 1: Magic and Version (10 bytes)

| Offset | Size | Field        | Value/Description              |
|--------|------|--------------|--------------------------------|
| 0      | 5    | Magic        | ASCII "ANUB3" (0x414E554233)   |
| 5      | 1    | Major Version| 0x02                           |
| 6      | 1    | Minor Version| 0x00                           |
| 7      | 1    | Patch Version| 0x00                           |
| 8      | 2    | Flags        | Reserved (0x0000)              |

**Total: 10 bytes**

### Section 2: Signer Metadata (152 bytes)

| Offset | Size | Field            | Description                           |
|--------|------|------------------|---------------------------------------|
| 10     | 64   | Signer Label     | UTF-8 string (printable 0x20-0x7E)    |
| 74     | 8    | Timestamp        | Unix timestamp (seconds since epoch)  |
| 82     | 64   | Fingerprint      | SHA-256 hash of signer's public keys  |
| 146    | 16   | Reserved         | Zero-filled, for future extensions    |

**Total: 152 bytes**

**Notes:**
- Signer Label: Human-readable identifier (e.g., "alice@example.com")
- Timestamp: Used for trust record auditing and signature validation
- Fingerprint: Computed as `SHA256(X25519_PK || ML_KEM_PK || Ed25519_PK || ML_DSA_PK)`

### Section 3: Key Encapsulation Material (1632 bytes)

| Offset | Size | Field               | Description                        |
|--------|------|---------------------|------------------------------------|
| 162    | 64   | X25519 Ciphertext   | ECDH ephemeral public key          |
| 226    | 1568 | ML-KEM Ciphertext   | Post-quantum KEM ciphertext        |

**Total: 1632 bytes**

**Key Derivation:**
1. X25519 ECDH produces 32-byte classical shared secret
2. ML-KEM-1024 decapsulation produces 32-byte PQ shared secret
3. Hybrid KDF: `master_key = HKDF-SHA256(x25519_secret || mlkem_secret, salt="ANUB3-KDF", info="master")`
4. Stream key derived: `stream_key = HKDF-SHA256(master_key, salt="ANUB3-STREAM", info=chunk_counter)`

### Section 4: Dual Signatures (4760 bytes)

| Offset | Size | Field               | Description                        |
|--------|------|---------------------|------------------------------------|
| 1794   | 64   | Ed25519 Signature   | Classical EdDSA signature          |
| 1858   | 4896 | ML-DSA-87 Signature | Post-quantum signature (NIST L5)   |

**Total: 4960 bytes**

**Signature Coverage:**
- All header fields (Sections 1-3)
- First 16 bytes of encrypted payload (binding header to content)
- Additional Authenticated Data (AAD) for XChaCha20-Poly1305

**Verification:**
- Both signatures MUST verify independently
- Either signature failure → authentication failure
- Trust policy applied to signer fingerprint

### Section 5: Padding and Alignment (18 bytes)

| Offset | Size | Field       | Description                  |
|--------|------|-------------|------------------------------|
| 6754   | 18   | Padding     | Zero-filled for alignment    |

**Total: 18 bytes**

## Complete Header Size

**Total ANUB3 Header: 6772 bytes**

```
  10  (Magic + Version)
+ 152  (Signer Metadata)
+ 1632 (Key Encapsulation)
+ 4960 (Dual Signatures)
+ 18   (Padding)
------
= 6772 bytes
```

## Encrypted Payload Format

### Streaming Encryption

The payload is encrypted using XChaCha20-Poly1305 AEAD in streaming mode:

```
┌──────────────────────┐
│   Chunk 0 (64 MB)    │ ← XChaCha20-Poly1305 with nonce = counter || 0x00...
├──────────────────────┤
│   Chunk 1 (64 MB)    │ ← XChaCha20-Poly1305 with nonce = counter || 0x01...
├──────────────────────┤
│   Chunk 2 (64 MB)    │ ← XChaCha20-Poly1305 with nonce = counter || 0x02...
├──────────────────────┤
│       ...            │
├──────────────────────┤
│ Final Chunk (<= 64MB)│ ← XChaCha20-Poly1305 with nonce = counter || 0xFF...
└──────────────────────┘
```

**Chunk Size:** Default 67,108,864 bytes (64 MiB), configurable via `--chunk-size`

**Nonce Construction:**
- 24-byte XChaCha20 nonce
- First 16 bytes: Random nonce from header KDF
- Last 8 bytes: Chunk counter (little-endian u64)

**Authentication:**
- Each chunk has 16-byte Poly1305 MAC appended
- MAC covers: AAD (header excerpt) + ciphertext chunk
- Final chunk may be shorter than chunk size

## Security Properties

### Confidentiality

1. **Classical Security:** X25519 ECDH + XChaCha20 → 128-bit classical security
2. **Post-Quantum Security:** ML-KEM-1024 → NIST Security Level 5 (256-bit PQ security)
3. **Hybrid Construction:** Even if quantum computers break X25519, ML-KEM-1024 protects confidentiality

### Authenticity

1. **Classical Signature:** Ed25519 → 128-bit classical signature security
2. **Post-Quantum Signature:** ML-DSA-87 → NIST Security Level 5 (256-bit PQ signature security)
3. **Hybrid Verification:** Both signatures must verify; compromise of one algorithm doesn't compromise authenticity
4. **Content Binding:** Signatures cover header + first payload chunk

### Trust Enforcement

1. **Fingerprint-Based Trust:** Signer identity bound to cryptographic fingerprint
2. **Trust States:** Pending, Approved, Denied
3. **Operator Tracking:** Trust decisions recorded with operator name and timestamp
4. **Immutable Trust Store:** SQLite database with write-ahead logging (WAL)

## Implementation Notes

### Header Construction (Encryption)

```ada
1. Generate ephemeral X25519 keypair
2. Generate ephemeral ML-KEM-1024 keypair
3. Perform X25519 ECDH with recipient's public key
4. Encapsulate ML-KEM shared secret with recipient's ML-KEM public key
5. Derive master key using hybrid KDF
6. Compute signer fingerprint
7. Construct header bytes (Sections 1-3)
8. Sign header with Ed25519 and ML-DSA-87
9. Write complete header to output file
10. Encrypt payload in streaming chunks
```

### Header Parsing (Decryption)

```ada
1. Read and validate magic bytes "ANUB3"
2. Parse version (must be 2.0.0)
3. Extract signer metadata (label, timestamp, fingerprint)
4. Read X25519 and ML-KEM ciphertexts
5. Verify Ed25519 signature
6. Verify ML-DSA-87 signature
7. Check trust policy for signer fingerprint
8. Perform X25519 ECDH with recipient's secret key
9. Decapsulate ML-KEM shared secret with recipient's ML-KEM secret key
10. Derive master key using hybrid KDF
11. Decrypt payload in streaming chunks
```

### Backward Compatibility

- **ANUB2 (v1.x):** Legacy format without dual signatures or trust system
- **Migration:** Use `anubis-spark convert` to re-encrypt ANUB2 → ANUB3
- **Detection:** Magic bytes distinguish formats ("ANUB2" vs "ANUB3")

## Test Vectors

### Example Header (Hexadecimal, first 256 bytes)

```
414E 5542 3302 0000 0000              # Magic "ANUB3" + version 2.0.0
616C 6963 6540 6578 616D 706C 652E   # Signer label "alice@example.com"
636F 6D00 0000 ...                   # (padded to 64 bytes)
0000 0001 8B2F 4E40                  # Timestamp (Unix seconds)
A3F2 7B91 4C8E ...                   # Fingerprint (SHA-256, 64 bytes)
0000 0000 ...                        # Reserved (16 bytes)
E7A1 2F3B ...                        # X25519 ciphertext (64 bytes)
9C4F 2A87 ...                        # ML-KEM ciphertext (1568 bytes)
... (signatures follow)
```

## References

1. NIST FIPS 203: Module-Lattice-Based Key-Encapsulation Mechanism (ML-KEM)
2. NIST FIPS 204: Module-Lattice-Based Digital Signature Algorithm (ML-DSA)
3. RFC 7748: Elliptic Curves for Security (X25519)
4. RFC 8032: Edwards-Curve Digital Signature Algorithm (EdDSA / Ed25519)
5. RFC 8439: ChaCha20 and Poly1305 for IETF Protocols
6. RFC 5869: HMAC-based Extract-and-Expand Key Derivation Function (HKDF)

## Changelog

- **v2.0.0 (ANUB3):** Initial specification with hybrid PQ + trust system
- **v1.x (ANUB2):** Legacy format (deprecated, migration recommended)

---

**Last Updated:** 2025-10-13
**Specification Version:** 2.0.0
**Author:** Anubis-SPARK Development Team
