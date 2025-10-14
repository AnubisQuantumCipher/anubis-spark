# ANUBIS-SPARK: LUKS2-Inspired Two-Kill Defense Architecture

**Date**: 2025-10-14
**Version**: v2.1.0-dev (ANUBISK3)
**Status**: Design Specification

---

## Executive Summary

ANUBIS-SPARK implements a **two-kill defense-in-depth architecture** inspired by LUKS2, where:

1. **Layer 1 (Passport)**: Argon2id + AES-256-XTS (passphrase-based symmetric encryption)
2. **Layer 2 (City)**: Quantum-resistant hybrid encryption (X25519+ML-KEM-1024, Ed25519+ML-DSA-87)

**Key Principle**: "An attacker must break BOTH layers to compromise data"

### Three Encryption Modes

1. **Identity Keystore (ANUBISK3)**: MANDATORY Argon2id + AES-XTS protection (no plaintext keys)
2. **File Mode A (Identity)**: Keys → Quantum hybrid encryption
3. **File Mode B (Passphrase)**: Passphrase → Argon2id + AES-XTS → Quantum hybrid wrapping

**Security Philosophy**: Everything encrypted by default. No plaintext keys. Ever.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│  USER                                                           │
│  ↓ provides passphrase                                         │
├─────────────────────────────────────────────────────────────────┤
│  LAYER 1: PASSPHRASE-BASED PROTECTION (LUKS2-inspired)         │
│  ├─ Argon2id SENSITIVE (1 GiB RAM, 4 iterations, 4 threads)   │
│  ├─ Derives 512-bit key                                        │
│  ├─ AES-256-XTS encryption (XTS uses 512-bit key)             │
│  └─ Multi-keyslot support (up to 8 passphrases)               │
├─────────────────────────────────────────────────────────────────┤
│  DECRYPTED PAYLOAD                                             │
│  ↓ (identity keys or master key)                              │
├─────────────────────────────────────────────────────────────────┤
│  LAYER 2: QUANTUM-RESISTANT HYBRID ENCRYPTION                  │
│  ├─ Classical: X25519 (ECDH) + Ed25519 (Signatures)           │
│  ├─ Post-Quantum: ML-KEM-1024 + ML-DSA-87                     │
│  ├─ Hybrid KEM: X25519 ⊕ ML-KEM-1024                          │
│  └─ Hybrid Signatures: Ed25519 + ML-DSA-87                    │
├─────────────────────────────────────────────────────────────────┤
│  ENCRYPTED FILE DATA                                           │
│  ├─ XChaCha20-Poly1305 AEAD (64 MB chunks)                    │
│  ├─ Per-chunk authentication                                   │
│  └─ AAD header binding                                         │
└─────────────────────────────────────────────────────────────────┘
```

---

## ANUBISK3 Format Specification

### Design Principles (LUKS2-inspired)

1. **Multi-keyslot support**: Up to 8 independent passphrases
2. **Key wrapping**: Master key encrypted by each keyslot independently
3. **Anti-forensic splitter**: Data diffused across keyslot to prevent partial recovery
4. **Metadata integrity**: JSON header with SHA-256 checksums
5. **Future-proof**: Versioned format with algorithm negotiation

### File Format

```
┌──────────────────────────────────────────────────────────────────┐
│  ANUBISK3 HEADER (2048 bytes)                                   │
│  ├─ Magic: "ANUBISK3" (8 bytes)                                 │
│  ├─ Version: 3 (4 bytes)                                        │
│  ├─ Header size: 2048 (4 bytes)                                 │
│  ├─ Keyslot count: 1-8 (4 bytes)                                │
│  ├─ Master key size: 32 bytes (4 bytes)                         │
│  ├─ Header checksum: SHA-256 (32 bytes)                         │
│  └─ Reserved: (1992 bytes for future use)                       │
├──────────────────────────────────────────────────────────────────┤
│  KEYSLOT 0 (4096 bytes)                                         │
│  ├─ Active flag: 1 = active, 0 = inactive (1 byte)             │
│  ├─ Argon2id parameters:                                        │
│  │  ├─ Memory: 1 GiB (4 bytes)                                  │
│  │  ├─ Iterations: 4 (4 bytes)                                  │
│  │  ├─ Parallelism: 4 (4 bytes)                                 │
│  │  └─ Salt: 32 bytes random                                    │
│  ├─ AF-splitter:                                                │
│  │  ├─ Stripes: 4000 (4 bytes)                                  │
│  │  └─ Hash: SHA-256                                            │
│  ├─ Encrypted master key: AES-256-XTS                           │
│  │  ├─ IV: 16 bytes                                             │
│  │  ├─ Ciphertext: 128,000 bytes (32 bytes × 4000 stripes)     │
│  │  └─ Auth tag: 16 bytes (HMAC-SHA256)                         │
│  └─ Metadata: creation time, label, etc. (64 bytes)            │
├──────────────────────────────────────────────────────────────────┤
│  KEYSLOT 1-7 (4096 bytes each, same structure)                 │
├──────────────────────────────────────────────────────────────────┤
│  IDENTITY KEYPAIR PAYLOAD (variable size)                       │
│  ├─ X25519 keypair (64 bytes)                                   │
│  ├─ ML-KEM-1024 keypair (3,168 bytes)                          │
│  ├─ Ed25519 keypair (96 bytes)                                  │
│  └─ ML-DSA-87 keypair (7,072 bytes)                            │
└──────────────────────────────────────────────────────────────────┘

Total size: 2048 + (8 × 4096) + 10,400 = 45,216 bytes (~44 KB)
```

---

## Layer 1: Passphrase Protection (LUKS2-Inspired)

### Argon2id Parameters (SENSITIVE Profile)

```ada
-- ANUBISK3 uses the most aggressive Argon2id parameters
Argon2id_Memory      : constant := 1_073_741_824;  -- 1 GiB RAM
Argon2id_Iterations  : constant := 4;              -- 4 iterations
Argon2id_Parallelism : constant := 4;              -- 4 threads
Argon2id_Salt_Length : constant := 32;             -- 256-bit salt
Argon2id_Key_Length  : constant := 64;             -- 512-bit key (for XTS)
```

**Security Properties**:
- **Memory-hard**: Defeats GPU/ASIC attacks (requires 1 GiB per attempt)
- **Time-hard**: 4 iterations ≈ 2-4 seconds per attempt
- **Parallel-hard**: 4 threads required (defeats simple parallelization)
- **Cost**: ~2-4 seconds per passphrase attempt on modern hardware

### AES-256-XTS Encryption

**XTS Mode** (XEX-based Tweaked CodeBook mode with ciphertext Stealing):
- **Key size**: 512 bits (256 bits for encryption + 256 bits for tweaking)
- **Tweak**: Sector/block number (prevents block reordering)
- **Properties**: Length-preserving, no ciphertext expansion

```ada
-- XTS encryption (libsodium fallback: XChaCha20-Poly1305)
procedure AES_XTS_Encrypt (
   Plaintext  : in     Byte_Array;
   Key_512    : in     Byte_Array;  -- 64 bytes (512 bits)
   Tweak      : in     Unsigned_64; -- Sector number
   Ciphertext : out    Byte_Array
) with
   Pre  => Plaintext'Length > 0 and then
           Key_512'Length = 64 and then
           Ciphertext'Length = Plaintext'Length,
   Post => Ciphertext'Length = Plaintext'Length;
```

### Anti-Forensic (AF) Information Splitter

Inspired by LUKS's AF splitter, diffuses master key across 4000 stripes:

```ada
-- AF-Splitter: Diffuse master key to prevent partial recovery
procedure AF_Split (
   Master_Key    : in     Byte_Array;  -- 32 bytes
   Salt          : in     Byte_Array;  -- 32 bytes
   Stripe_Count  : in     Natural;     -- 4000
   Split_Data    : out    Byte_Array   -- 128,000 bytes (32 × 4000)
) with
   Pre  => Master_Key'Length = 32 and then
           Salt'Length = 32 and then
           Stripe_Count = 4000 and then
           Split_Data'Length = Master_Key'Length * Stripe_Count,
   Post => Split_Data'Length = Master_Key'Length * Stripe_Count;

-- AF-Merge: Recover master key from split data
procedure AF_Merge (
   Split_Data    : in     Byte_Array;  -- 128,000 bytes
   Salt          : in     Byte_Array;  -- 32 bytes
   Stripe_Count  : in     Natural;     -- 4000
   Master_Key    : out    Byte_Array;  -- 32 bytes
   Success       : out    Boolean
) with
   Pre  => Split_Data'Length = 128_000 and then
           Salt'Length = 32 and then
           Stripe_Count = 4000 and then
           Master_Key'Length = 32,
   Post => (if Success then not Is_All_Zero (Master_Key)
            else Is_All_Zero (Master_Key));
```

**Security Properties**:
- **Diffusion**: Master key spread across 128 KB
- **All-or-nothing**: Must recover ALL stripes to get master key
- **Anti-forensics**: Partial disk recovery yields nothing

### Multi-Keyslot System

```ada
type Keyslot_ID is range 0 .. 7;  -- 8 keyslots

-- Keyslot structure
type Keyslot is record
   Active       : Boolean := False;
   Salt         : Byte_Array (1 .. 32);
   AF_Split_Data : Byte_Array (1 .. 128_000);  -- 4000 stripes × 32 bytes
   IV           : Byte_Array (1 .. 16);
   Auth_Tag     : Byte_Array (1 .. 16);
   Created_At   : Unsigned_64;
   Label        : String (1 .. 64);  -- "primary", "backup", etc.
end record;

-- ANUBISK3 keystore
type ANUBISK3_Keystore is record
   Magic        : String (1 .. 8) := "ANUBISK3";
   Version      : Unsigned_32 := 3;
   Keyslot_Count : Natural range 0 .. 8;
   Keyslots     : array (Keyslot_ID) of Keyslot;
   Identity     : Identity_Keypair;  -- Encrypted payload
end record;
```

**Keyslot Operations**:
```ada
-- Add new passphrase to unused keyslot
procedure Keyslot_Add (
   Keystore       : in out ANUBISK3_Keystore;
   New_Passphrase : in     String;
   Slot_Label     : in     String;
   Success        : out    Boolean
) with
   Pre  => New_Passphrase'Length >= 12 and then
           Slot_Label'Length > 0,
   Post => (if Success then Keyslot_Count_Increased (Keystore)
            else Keyslot_Count_Unchanged (Keystore));

-- Remove passphrase from keyslot
procedure Keyslot_Remove (
   Keystore    : in out ANUBISK3_Keystore;
   Slot_ID     : in     Keyslot_ID;
   Success     : out    Boolean
) with
   Pre  => Keystore.Keyslots (Slot_ID).Active,
   Post => (if Success then not Keystore.Keyslots (Slot_ID).Active
            else Keystore.Keyslots (Slot_ID).Active);

-- Change existing passphrase
procedure Keyslot_Change (
   Keystore       : in out ANUBISK3_Keystore;
   Old_Passphrase : in     String;
   New_Passphrase : in     String;
   Success        : out    Boolean
) with
   Pre  => Old_Passphrase'Length >= 12 and then
           New_Passphrase'Length >= 12,
   Post => (if Success then Master_Key_Unchanged (Keystore)
            else Keystore_Unchanged (Keystore));
```

---

## Layer 2: Quantum-Resistant Hybrid Encryption

### Mode A: Identity-Based File Encryption (Current System)

```
User provides passphrase
  ↓ Argon2id (1 GiB, 4 iter)
  ↓ AES-256-XTS decryption
  ↓ Identity keypair unlocked
  ↓
File encryption with hybrid PQ keys
  ├─ Ephemeral X25519 + ML-KEM-1024 key exchange
  ├─ Hybrid secret = X25519_secret ⊕ ML_KEM_secret
  ├─ HKDF derives encryption key
  ├─ XChaCha20-Poly1305 AEAD (streaming)
  └─ Hybrid signature: Ed25519 + ML-DSA-87
```

**Security**: Two-kill at keystore level, quantum-resistant at file level

### Mode B: Passphrase-Based File Encryption (NEW)

```
User provides file passphrase
  ↓ Argon2id (1 GiB, 4 iter)
  ↓ Derives 512-bit master key
  ↓ AF-Split (4000 stripes)
  ↓ AES-256-XTS encryption of AF-split data
  ↓
THEN hybrid quantum wrapping:
  ├─ Encrypted AF-split data becomes "file payload"
  ├─ Ephemeral X25519 + ML-KEM-1024 key exchange
  ├─ Hybrid secret encrypts the passphrase-encrypted data
  ├─ XChaCha20-Poly1305 AEAD (streaming)
  └─ Hybrid signature: Ed25519 + ML-DSA-87
```

**Security**: THREE-kill defense:
1. Passphrase (Argon2id + AES-XTS)
2. Quantum-resistant KEM (X25519 + ML-KEM-1024)
3. Hybrid signatures (Ed25519 + ML-DSA-87)

**Attack Surface**:
- Attacker must break: Argon2id AND AES-256 AND X25519 AND ML-KEM-1024
- Or break: Argon2id AND AES-256 AND (Ed25519 OR ML-DSA-87) signature forgery

---

## CLI Interface

### Identity Keystore (Mandatory Passphrase)

```bash
# Generate identity (MANDATORY passphrase)
anubis-spark keygen --output identity.key
# Prompts: Enter passphrase (min 12 chars):
# Creates: ANUBISK3 keystore with 1 active keyslot

# List keyslots
anubis-spark keyslot list --keystore identity.key
# Output:
#   Slot 0: [ACTIVE] primary (created: 2025-10-14 08:00:00)
#   Slot 1: [inactive]
#   ...

# Add backup passphrase
anubis-spark keyslot add --keystore identity.key --label backup
# Prompts: Enter current passphrase:
# Prompts: Enter new passphrase:
# Creates: Second active keyslot

# Remove keyslot
anubis-spark keyslot remove --keystore identity.key --slot 1
# Prompts: Enter passphrase for verification:

# Change passphrase
anubis-spark keyslot change --keystore identity.key --slot 0
# Prompts: Enter old passphrase:
# Prompts: Enter new passphrase:
```

### File Encryption Modes

```bash
# Mode A: Identity-based (quantum-resistant)
anubis-spark encrypt --key identity.key --input secret.txt
# Prompts: Enter keystore passphrase:
# Uses: Argon2id → AES-XTS → unlocks identity → quantum hybrid encryption

# Mode B: Passphrase-based (triple-kill)
anubis-spark encrypt --passphrase --input secret.txt
# Prompts: Enter file passphrase:
# Prompts: Confirm passphrase:
# Uses: Argon2id → AES-XTS → AF-split → THEN quantum hybrid wrapping
# Creates: secret.txt.anubis (no identity key needed for decryption)

# Decrypt Mode A (identity)
anubis-spark decrypt --key identity.key --input secret.txt.anubis
# Prompts: Enter keystore passphrase:

# Decrypt Mode B (passphrase)
anubis-spark decrypt --passphrase --input secret.txt.anubis
# Prompts: Enter file passphrase:
```

---

## Security Analysis

### Threat Model

| Attack Vector | ANUBISK3 Defense | Security Margin |
|--------------|------------------|-----------------|
| **Passphrase brute-force** | Argon2id (1 GiB, 4 iter) ≈ 2-4 sec/attempt | ~10^12 attempts for 12-char passphrase |
| **GPU/ASIC cracking** | Memory-hard (1 GiB per attempt) | Infeasible at scale |
| **Quantum computer** | ML-KEM-1024 + ML-DSA-87 (NIST Level 5) | 256-bit quantum security |
| **Partial key recovery** | AF-splitter (4000 stripes) | All-or-nothing |
| **Cold boot attack** | Passphrase-locked (no plaintext keys in memory) | Keys zeroized immediately |
| **Forensic recovery** | AF-splitter diffusion | 128 KB required (all stripes) |
| **Side-channel** | Constant-time Argon2id, AES-XTS | Timing-resistant |

### Two-Kill Guarantee

**For identity keystores**:
- Attacker must break: **Argon2id AND AES-256**

**For identity-based files**:
- Attacker must break: **Argon2id AND AES-256** (keystore)
- THEN: **X25519 AND ML-KEM-1024** (file encryption)

**For passphrase-based files**:
- Attacker must break: **Argon2id AND AES-256** (file passphrase)
- THEN: **X25519 AND ML-KEM-1024** (quantum wrapping)

**Attack must succeed on ALL layers** - breaking one is insufficient.

---

## Implementation Plan

### Phase 1: AES-XTS Bindings
1. Create Ada FFI bindings for OpenSSL EVP_CIPHER (AES-XTS)
2. Fallback: Use XChaCha20-Poly1305 (already available via libsodium)
3. Add elaborate contracts proving length preservation

### Phase 2: ANUBISK3 Format
1. Define keyslot structure
2. Implement AF-splitter (diffusion algorithm)
3. Create header parsing/serialization
4. Add elaborate contracts for keyslot operations

### Phase 3: CLI Integration
1. Make passphrases MANDATORY for keygen
2. Add `keyslot` subcommands (list/add/remove/change)
3. Add `--passphrase` flag for file encryption
4. Update all help text

### Phase 4: Migration
1. Detect ANUBISK2 (old format) and refuse to load
2. Provide migration tool: `anubis-spark migrate --input old.key --output new.key`
3. Document migration process

### Phase 5: Documentation
1. Update README with LUKS2 architecture
2. Create ANUBISK3_SPECIFICATION.md
3. Update security documentation
4. Add examples and best practices

---

## Elaborate Contracts (PLATINUM+ Level)

### Keyslot Operations

```ada
-- ELABORATE: Keyslot addition contract
procedure Keyslot_Add (
   Keystore       : in out ANUBISK3_Keystore;
   New_Passphrase : in     String;
   Slot_Label     : in     String;
   Success        : out    Boolean
) with
   Pre  => -- ELABORATE PRECONDITION: All requirements stated
           New_Passphrase'Length >= 12 and then  -- Minimum passphrase length
           New_Passphrase'Length <= 256 and then -- Maximum length (reasonable)
           Slot_Label'Length > 0 and then
           Slot_Label'Length <= 64 and then
           Keystore.Keyslot_Count < 8,           -- At least one slot free
   Global => null,  -- FRAME: No side effects
   Post => -- COMPREHENSIVE POSTCONDITION: All outcomes proven
           (if Success then
               -- On success: New keyslot added, master key unchanged
               (Keystore.Keyslot_Count = Keystore.Keyslot_Count'Old + 1 and then
                Master_Key_Unchanged (Keystore) and then
                Keyslot_Active (Keystore, New_Slot_ID) and then
                Keyslot_Label_Set (Keystore, New_Slot_ID, Slot_Label))
            else
               -- On failure: Keystore unchanged
               (Keystore = Keystore'Old)),
   Contract_Cases => (
      -- SUCCESS: Slot added successfully
      Success and Keystore.Keyslot_Count < 8 =>
         (Keystore.Keyslot_Count = Keystore.Keyslot_Count'Old + 1 and then
          Master_Key_Unchanged (Keystore)),
      -- FAILURE: No free slots
      not Success and Keystore.Keyslot_Count = 8 =>
         (Keystore = Keystore'Old),
      -- FAILURE: Argon2id failed
      not Success =>
         (Keystore = Keystore'Old)
   );
```

### AF-Splitter Contracts

```ada
-- ELABORATE: AF-Split contract proving diffusion
procedure AF_Split (
   Master_Key    : in     Byte_Array;
   Salt          : in     Byte_Array;
   Stripe_Count  : in     Natural;
   Split_Data    : out    Byte_Array
) with
   Pre  => -- ELABORATE: All requirements for diffusion
           Master_Key'Length = 32 and then
           Salt'Length = 32 and then
           Stripe_Count = 4000 and then
           Split_Data'Length = Master_Key'Length * Stripe_Count and then
           not Is_All_Zero (Master_Key) and then  -- Master key has entropy
           not Is_All_Zero (Salt),                -- Salt has entropy
   Global => null,  -- FRAME: No side effects
   Post => -- COMPREHENSIVE: Prove diffusion properties
           Split_Data'Length = Master_Key'Length * Stripe_Count and then
           not Is_All_Zero (Split_Data) and then  -- Output has entropy
           -- Prove diffusion: split data differs from master key
           (for all I in Master_Key'Range =>
              Split_Data (I) /= Master_Key (I)) and then
           -- Prove spread: master key not directly present in output
           not Contains_Substring (Split_Data, Master_Key),
   Contract_Cases => (
      Master_Key'Length = 32 and Stripe_Count = 4000 =>
         (Split_Data'Length = 128_000 and then
          not Is_All_Zero (Split_Data))
   );
```

---

## Future Enhancements

### Phase 2 Features
1. **Hardware Security Module (HSM) support**: Store master key in TPM/YubiKey
2. **Biometric unlocking**: Fingerprint + passphrase (two-factor)
3. **Key escrow**: M-of-N threshold recovery (Shamir Secret Sharing)
4. **Audit logging**: Tamper-evident log of keyslot operations

### Phase 3 Features
1. **LUKS2 container support**: Full-disk encryption integration
2. **Network key servers**: Remote keyslot management
3. **Hardware acceleration**: AES-NI, ARM Crypto Extensions
4. **FIDO2/WebAuthn**: Hardware token authentication

---

## Conclusion

ANUBIS-SPARK's LUKS2-inspired architecture provides **defense-in-depth** through:

1. **Mandatory passphrase protection** (no plaintext keys)
2. **Multi-keyslot support** (up to 8 independent passphrases)
3. **Anti-forensic diffusion** (128 KB all-or-nothing recovery)
4. **Quantum-resistant wrapping** (ML-KEM-1024 + ML-DSA-87)
5. **Elaborate contracts** (PLATINUM+ verification)

**Security Guarantee**: "An attacker must break Argon2id AND AES-256 AND (X25519 OR ML-KEM-1024) to compromise data"

**User Experience**: Simple, secure, with sensible defaults.

---

**Prepared By**: LUKS2 Integration Initiative
**Date**: 2025-10-14
**Version**: v2.1.0-dev
**Status**: Design Specification - Ready for Implementation
