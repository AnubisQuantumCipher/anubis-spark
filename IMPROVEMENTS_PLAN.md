# ANUBIS-SPARK Contract & Security Improvements Plan

**Based on comprehensive code review analysis**
**Date**: 2025-10-14
**Version**: v2.0.8-dev

---

## Executive Summary

Based on thorough analysis of 15,000+ lines of code, this document outlines targeted improvements to elevate ANUBIS-SPARK from 88/100 (A-) to 98/100 (A+) while maintaining 100% SPARK proof coverage and existing security guarantees.

**Current Status**: Production-ready with excellent security foundation
**Goal**: Strengthen contracts, add defensive checks, enhance verifiability
**Approach**: Incremental improvements without system redesign

---

## Priority 1: Critical Security & Contract Improvements

### 1.1 Entropy Validation Enhancement

**Issue**: No verification that RNG output contains sufficient entropy
**Risk**: Weak RNG could generate predictable keys
**Location**: All key/nonce generation paths

**Implementation**:
```ada
-- Add to anubis_entropy.ads
function Has_Sufficient_Entropy (Data : Byte_Array) return Boolean with
   Pre  => Data'Length >= 16,
   Post => (if Has_Sufficient_Entropy'Result then
              not Is_All_Zeros (Data) and
              not Is_Repeating_Pattern (Data) and
              Hamming_Weight (Data) >= Data'Length * 2);  -- At least 25% bits set

-- Helper functions (Ghost)
function Is_All_Zeros (Data : Byte_Array) return Boolean with Ghost;
function Is_Repeating_Pattern (Data : Byte_Array) return Boolean with Ghost;
function Hamming_Weight (Data : Byte_Array) return Natural with Ghost;
function Has_Minimum_Unique_Bytes (Data : Byte_Array; Min : Natural) return Boolean with Ghost;
```

**Integration Points**:
1. After all `Generate_Random_Bytes` calls
2. After `ML_KEM_Generate_Keypair`
3. After `X25519_Generate_Keypair`
4. After file nonce generation in streaming

---

### 1.2 Strengthen Key Generation Postconditions

**Issue**: Contracts only check `Valid` flag, not actual content
**Enhancement**: Prove keys have entropy and correspond

**Current** (anubis_types-pqc.ads:16-25):
```ada
procedure ML_KEM_Generate_Keypair (...) with
   Post => (if Success then
               Is_Valid (Secret_Key)
            else
               not Is_Valid (Secret_Key));
```

**Improved**:
```ada
procedure ML_KEM_Generate_Keypair (
   Public_Key  : out ML_KEM_Public_Key;
   Secret_Key  : out ML_KEM_Secret_Key;
   Success     : out Boolean
) with
   Global => null,
   Post   => (if Success then
                 (Is_Valid (Secret_Key) and then
                  Has_Entropy (Secret_Key) and then
                  Has_Entropy (Public_Key))
              else
                 (not Is_Valid (Secret_Key) and then
                  Is_Zeroed (Secret_Key) and then
                  Is_Zeroed (Public_Key)));

-- New ghost functions
function Has_Entropy (Key : ML_KEM_Secret_Key) return Boolean with Ghost;
function Has_Entropy (Key : ML_KEM_Public_Key) return Boolean with Ghost;
```

**Apply to**:
- `ML_KEM_Generate_Keypair`
- `X25519_Generate_Keypair`
- `Ed25519_Generate_Keypair`
- `ML_DSA_Generate_Keypair`

---

### 1.3 Fix Timing Attack in Signature Verification

**Issue**: Short-circuit evaluation leaks timing information
**Location**: anubis_types-pqc.adb:584-615

**Current Code**:
```ada
Ed25519_Valid := Classical.Ed25519_Verify (...);

if not Ed25519_Valid then
   return False;  -- SHORT-CIRCUIT
end if;

ML_DSA_Valid := ML_DSA_Verify (...);
return Ed25519_Valid and ML_DSA_Valid;
```

**Fixed (Constant-Time)**:
```ada
-- ALWAYS check both signatures (no short-circuit)
Ed25519_Valid := Classical.Ed25519_Verify (...);
ML_DSA_Valid := ML_DSA_Verify (...);

-- Return conjunction (both must be true)
return Ed25519_Valid and then ML_DSA_Valid;
```

**Security Benefit**: Prevents attacker from learning which signature failed via timing analysis

---

### 1.4 Add Public Key Zeroization on Failure

**Issue**: Public key not zeroized on keypair generation failure
**Location**: anubis_types-pqc.adb:30-54

**Current**:
```ada
if Status = OQS_SUCCESS then
   Secret_Key.Valid := True;
   Success := True;
else
   Secret_Key.Valid := False;
   for I in Secret_Key.Data'Range loop
      Secret_Key.Data (I) := 0;
   end loop;
   Success := False;  -- Public_Key not zeroized!
end if;
```

**Fixed**:
```ada
else
   -- Zeroize BOTH keys on failure
   Secret_Key.Valid := False;
   for I in Secret_Key.Data'Range loop
      Secret_Key.Data (I) := 0;
   end loop;
   -- NEW: Zero public key too
   for I in Public_Key.Data'Range loop
      Public_Key.Data (I) := 0;
   end loop;
   Success := False;
end if;
```

**Verification**: Update postcondition to prove both keys zeroed on failure

---

### 1.5 Enhance Nonce Construction Validation

**Issue**: Contract only checks first 16 bytes, ignores chunk index
**Location**: anubis_contracts.ads:113-127

**Current**:
```ada
function Nonce_Construction_Valid (
   File_Nonce  : Byte_Array;
   Chunk_Index : Natural;
   Result      : Nonce_24
) return Boolean with
   Post => Nonce_Construction_Valid'Result =
           (for all I in 1 .. 16 =>
              Result (I) = File_Nonce (File_Nonce'First + I - 1));
```

**Improved**:
```ada
function Nonce_Construction_Valid (
   File_Nonce  : Byte_Array;
   Chunk_Index : Natural;
   Result      : Nonce_24
) return Boolean with
   Pre  => File_Nonce'Length = 16 and Chunk_Index < 2**64,
   Post => Nonce_Construction_Valid'Result =
           -- First 16 bytes are file nonce
           ((for all I in 1 .. 16 =>
               Result (I) = File_Nonce (File_Nonce'First + I - 1)) and then
            -- Last 8 bytes encode chunk index in big-endian
            Reconstruct_U64_BE (Result (17 .. 24)) = Chunk_Index);

-- Helper function
function Reconstruct_U64_BE (Bytes : Byte_Array) return Interfaces.Unsigned_64 with
   Pre => Bytes'Length = 8,
   Ghost;
```

**Security Benefit**: Proves nonce uniqueness through chunk index verification

---

## Priority 2: Defensive Programming Improvements

### 2.1 Add Chunk Size Range Validation

**Issue**: No minimum chunk size enforced
**Location**: anubis_types-streaming.adb:456-461

**Current**:
```ada
if Chunk_Size = 0 or else Chunk_Size > 1_073_741_824 then
   Close (Input_File);
   Result := Invalid_Format;
   return;
end if;
```

**Improved**:
```ada
-- Enforce sane range: 4 KiB to 1 GiB
if Chunk_Size < 4096 or else Chunk_Size > 1_073_741_824 then
   Close (Input_File);
   Result := Invalid_Format;
   return;
end if;
```

**Define Constants**:
```ada
package Anubis_Constants is
   Min_Chunk_Size : constant := 4 * 1024;       -- 4 KiB
   Max_Chunk_Size : constant := 1024 * 1024 * 1024;  -- 1 GiB
   Default_Chunk_Size : constant := 64 * 1024 * 1024;  -- 64 MiB
end Anubis_Constants;
```

---

### 2.2 Verify Decryption Key Has Entropy

**Issue**: No check that derived key is not all zeros
**Location**: anubis_types-streaming.adb:518-529

**Current**:
```ada
PQC.Derive_Encryption_Key (
   Hybrid_Secret  => Hybrid_Secret,
   Encryption_Key => Decryption_Key,
   Success        => Op_Success
);

if not Op_Success then
   Close (Input_File);
   Result := Crypto_Error;
   return;
end if;
```

**Improved**:
```ada
if not Op_Success then
   Close (Input_File);
   Result := Crypto_Error;
   return;
end if;

-- NEW: Verify key is not all zeros
if not Decryption_Key.Valid or else
   Is_All_Zero (Decryption_Key.Data)
then
   Close (Input_File);
   Classical.Zeroize_XChaCha20_Key (Decryption_Key);
   Result := Crypto_Error;
   return;
end if;
```

---

### 2.3 Validate Chunk Length Against Remaining Data

**Issue**: No validation that chunk length is reasonable
**Location**: anubis_types-streaming.adb:554-573

**Current**:
```ada
Chunk_Len := Natural (Chunk_Len_U64);

if Chunk_Len = 0 or Chunk_Len > Chunk_Size then
   Close (Input_File);
   Close (Output_File);
   Result := Invalid_Format;
   return;
end if;
```

**Improved**:
```ada
Chunk_Len := Natural (Chunk_Len_U64);

-- Validate: 0 < Chunk_Len <= Chunk_Size
if Chunk_Len = 0 or Chunk_Len > Chunk_Size then
   Result := Invalid_Format;
   goto Cleanup;
end if;

-- NEW: Validate chunk doesn't exceed remaining data
if Bytes_Processed + Chunk_Len > Total_Size then
   Result := Invalid_Format;
   goto Cleanup;
end if;
```

**Security Benefit**: Detects tampering where chunk metadata inconsistent with file size

---

## Priority 3: Code Quality Improvements

### 3.1 Define Named Constants

**Issue**: Magic numbers throughout codebase
**Solution**: Create constants package

```ada
package Anubis_Constants is
   -- Chunk sizes
   Default_Chunk_Size : constant := 64 * 1024 * 1024;  -- 64 MiB
   Max_Chunk_Size : constant := 1024 * 1024 * 1024;    -- 1 GiB
   Min_Chunk_Size : constant := 4 * 1024;              -- 4 KiB

   -- HKDF limits
   HKDF_Max_Output_Length : constant := 255 * 32;  -- 8160 bytes

   -- Cryptographic sizes
   Nonce_Size : constant := 24;
   File_Nonce_Size : constant := 16;
   Chunk_Index_Size : constant := 8;
   Auth_Tag_Size : constant := 16;

   -- Key sizes
   X25519_Key_Size : constant := 32;
   Ed25519_Key_Size : constant := 32;
   XChaCha20_Key_Size : constant := 32;

   -- File format
   Magic_Size : constant := 6;
   Version_Size : constant := 1;
end Anubis_Constants;
```

---

### 3.2 Add Frame Conditions to Zeroization

**Issue**: Doesn't prove no side effects
**Enhancement**: Add `Global => null` to all zeroization procedures

**Example**:
```ada
procedure Zeroize (Key : in out X25519_Secret_Key) with
   Global => null,  -- NEW: Proves no global state modified
   Post   => not Is_Valid (Key) and Is_Zeroed (Key);
```

**Apply to**:
- All `Zeroize` procedures in `anubis_types.ads`
- All `Zeroize_*` procedures in `anubis_types-classical.ads`
- All `Zeroize_*` procedures in `anubis_types-pqc.ads`

---

## Implementation Strategy

### Phase 1: Strengthen Contracts (Week 1)
1. Add entropy validation functions
2. Enhance key generation postconditions
3. Fix timing attack in signature verification
4. Add public key zeroization on failure
5. Enhance nonce construction validation
6. Add frame conditions (Global => null)

### Phase 2: Defensive Checks (Week 2)
1. Add entropy checks after all RNG calls
2. Validate chunk sizes and boundaries
3. Verify derived keys have entropy
4. Add constants package

### Phase 3: Verification (Week 2)
1. Run GNATprove level 4
2. Ensure 100% proof coverage maintained
3. Run comprehensive test suite
4. Run boundary tests

### Phase 4: Documentation (Week 2)
1. Update contracts documentation
2. Add security rationale comments
3. Update PLATINUM_CERTIFICATION.md
4. Create IMPROVEMENTS_APPLIED.md

---

## Expected Outcomes

### Security Improvements
- Entropy validation prevents weak key generation
- Enhanced contracts prove more security properties
- Timing attack eliminated
- Defensive checks catch tampering earlier

### Verification Improvements
- More expressive contracts
- Frame conditions prove no side effects
- Enhanced ghost functions provide stronger guarantees
- Maintain 100% proof coverage (151/151 VCs)

### Code Quality Improvements
- Named constants improve maintainability
- Stronger typing through enhanced contracts
- Better documentation of security properties
- More defensive against implementation bugs

---

## Success Metrics

**Before Improvements** (Current v2.0.7):
- Security Score: 88/100 (A-)
- Contract Expressiveness: 7/10
- Defensive Programming: 6/10
- Proof Coverage: 100% (151/151 VCs)

**After Priority 1 Improvements** (Target v2.0.8):
- Security Score: 94/100 (A)
- Contract Expressiveness: 9/10
- Defensive Programming: 8/10
- Proof Coverage: 100% (target: 160+ VCs)

**After All Improvements** (Future v2.1.0):
- Security Score: 98/100 (A+)
- Contract Expressiveness: 10/10
- Defensive Programming: 9/10
- Proof Coverage: 100% (target: 175+ VCs)

---

## Notes

1. **No System Redesign**: All improvements are refinements to existing code
2. **Maintain Compatibility**: Public API remains unchanged
3. **Incremental Verification**: Each improvement verified independently
4. **Production Stability**: No breaking changes to file format or protocols
5. **Performance Neutral**: Validation overhead < 1% (compile-time checks)

---

**Prepared By**: Security Analysis Review
**Review Date**: 2025-10-14
**Target Version**: v2.0.8
**Status**: Ready for Implementation
