# ANUBIS-SPARK API Reference

Complete developer API documentation for all cryptographic operations.

## Table of Contents

- [Package Overview](#package-overview)
- [Anubis_Types](#anubis_types)
- [Anubis_Types.PQC](#anubis_typespqc)
- [OQS_Common](#oqs_common)
- [OQS_KEM_ML_KEM](#oqs_kem_ml_kem)
- [OQS_SIG_ML_DSA](#oqs_sig_ml_dsa)
- [Usage Examples](#usage-examples)
- [Error Handling](#error-handling)
- [Best Practices](#best-practices)

---

## Package Overview

### Package Hierarchy

```
Anubis_Types (SPARK Mode: On)
├── Type definitions (public)
├── Validity checking (public)
└── Anubis_Types.PQC (child package)
    ├── Specification (SPARK Mode: On)
    └── Body (SPARK Mode: Off, C FFI)

OQS_Common (C FFI)
├── Library initialization
├── Memory operations
└── Status codes

OQS_KEM_ML_KEM (C FFI)
└── ML-KEM-1024 operations

OQS_SIG_ML_DSA (C FFI)
└── ML-DSA-87 operations
```

---

## Anubis_Types

**File:** `src/crypto/anubis_types.ads`, `anubis_types.adb`
**SPARK Mode:** On
**Purpose:** Foundational type system for cryptographic operations

### Types

#### Byte and Byte_Array

```ada
type Byte is new Interfaces.Unsigned_8;
type Byte_Array is array (Positive range <>) of Byte;
```

**Description:** Basic unsigned 8-bit type for binary data.

**Usage:**
```ada
Message : constant Byte_Array := (72, 101, 108, 108, 111);  -- "Hello"
```

---

#### ML-KEM-1024 Types

##### ML_KEM_Public_Key

```ada
type ML_KEM_Public_Key is record
   Data : Byte_Array (1 .. ML_KEM_1024_PUBLIC_KEY_SIZE);  -- 1,568 bytes
end record;
```

**Purpose:** ML-KEM-1024 public key for encapsulation.
**Size:** 1,568 bytes
**Shareable:** Yes (public)
**Storage:** Can be stored/transmitted in plaintext

---

##### ML_KEM_Secret_Key

```ada
type ML_KEM_Secret_Key is record
   Data  : Byte_Array (1 .. ML_KEM_1024_SECRET_KEY_SIZE);  -- 3,168 bytes
   Valid : Boolean := False;
end record with Volatile;
```

**Purpose:** ML-KEM-1024 secret key for decapsulation.
**Size:** 3,168 bytes
**Shareable:** No (secret)
**Storage:** Must be encrypted at rest
**Volatile:** Prevents compiler optimization of zeroization

**Validity Checking:**
```ada
function Is_Valid (Key : ML_KEM_Secret_Key) return Boolean;
```

**Zeroization:**
```ada
procedure Zeroize (Key : in out ML_KEM_Secret_Key) with
   Post => not Is_Valid (Key);
```

---

##### ML_KEM_Ciphertext

```ada
type ML_KEM_Ciphertext is record
   Data : Byte_Array (1 .. ML_KEM_1024_CIPHERTEXT_SIZE);  -- 1,568 bytes
end record;
```

**Purpose:** ML-KEM-1024 encapsulated ciphertext.
**Size:** 1,568 bytes
**Shareable:** Yes (public)
**Contains:** Encrypted shared secret

---

##### ML_KEM_Shared_Secret

```ada
type ML_KEM_Shared_Secret is record
   Data  : Byte_Array (1 .. ML_KEM_1024_SHARED_SECRET_SIZE);  -- 32 bytes
   Valid : Boolean := False;
end record with Volatile;
```

**Purpose:** ML-KEM-1024 shared secret derived from encapsulation/decapsulation.
**Size:** 32 bytes
**Shareable:** No (secret)
**Zeroization:** Required after use

---

#### ML-DSA-87 Types

##### ML_DSA_Public_Key

```ada
type ML_DSA_Public_Key is record
   Data : Byte_Array (1 .. ML_DSA_87_PUBLIC_KEY_SIZE);  -- 2,592 bytes
end record;
```

**Purpose:** ML-DSA-87 public key for signature verification.
**Size:** 2,592 bytes
**Shareable:** Yes (public)

---

##### ML_DSA_Secret_Key

```ada
type ML_DSA_Secret_Key is record
   Data  : Byte_Array (1 .. ML_DSA_87_SECRET_KEY_SIZE);  -- 4,896 bytes
   Valid : Boolean := False;
end record with Volatile;
```

**Purpose:** ML-DSA-87 secret key for signing.
**Size:** 4,896 bytes
**Shareable:** No (secret)
**Storage:** Must be encrypted at rest

---

##### ML_DSA_Signature

```ada
type ML_DSA_Signature is record
   Data : Byte_Array (1 .. ML_DSA_87_SIGNATURE_SIZE);  -- 4,627 bytes
end record;
```

**Purpose:** ML-DSA-87 digital signature.
**Size:** 4,627 bytes
**Shareable:** Yes (public)
**Contains:** Cryptographic signature over message

---

#### Classical Crypto Types (Architecture)

##### X25519_Public_Key, X25519_Secret_Key

```ada
type X25519_Public_Key is record
   Data : Byte_Array (1 .. 32);
end record;

type X25519_Secret_Key is record
   Data  : Byte_Array (1 .. 32);
   Valid : Boolean := False;
end record with Volatile;
```

**Purpose:** Curve25519 ECDH key exchange.
**Status:** Architecture only (implementation pending)

---

##### Ed25519_Public_Key, Ed25519_Secret_Key

```ada
type Ed25519_Public_Key is record
   Data : Byte_Array (1 .. 32);
end record;

type Ed25519_Secret_Key is record
   Data  : Byte_Array (1 .. 32);
   Valid : Boolean := False;
end record with Volatile;
```

**Purpose:** Ed25519 digital signatures.
**Status:** Architecture only (implementation pending)

---

##### XChaCha20_Key

```ada
type XChaCha20_Key is record
   Data  : Byte_Array (1 .. 32);
   Valid : Boolean := False;
end record with Volatile;
```

**Purpose:** XChaCha20-Poly1305 AEAD encryption key.
**Size:** 32 bytes (256-bit)
**Status:** Architecture only (implementation pending)

---

#### Hybrid Types

##### Hybrid_Shared_Secret

```ada
type Hybrid_Shared_Secret is record
   Classical_Secret : Byte_Array (1 .. 32);
   PQ_Secret        : Byte_Array (1 .. 32);
   Valid            : Boolean := False;
end record with Volatile;
```

**Purpose:** Combined classical + post-quantum shared secret.
**Derivation:** HKDF(X25519_Shared || ML_KEM_Shared, context, 32)
**Security:** Attacker must break BOTH algorithms

---

## Anubis_Types.PQC

**File:** `src/crypto/anubis_types-pqc.ads`, `anubis_types-pqc.adb`
**SPARK Mode:** Spec: On, Body: Off
**Purpose:** SPARK-safe wrapper for liboqs post-quantum operations

### ML-KEM-1024 Operations

#### ML_KEM_Generate_Keypair

```ada
procedure ML_KEM_Generate_Keypair (
   Public_Key  : out ML_KEM_Public_Key;
   Secret_Key  : out ML_KEM_Secret_Key;
   Success     : out Boolean
) with
   Global => null,
   Post   => (if Success then Is_Valid (Secret_Key)
              else not Is_Valid (Secret_Key));
```

**Description:** Generate a fresh ML-KEM-1024 keypair.

**Parameters:**
- `Public_Key` (out): Generated public key (1,568 bytes)
- `Secret_Key` (out): Generated secret key (3,168 bytes), marked valid if successful
- `Success` (out): True if generation succeeded, False otherwise

**Postconditions:**
- If `Success = True`, `Secret_Key.Valid = True`
- If `Success = False`, `Secret_Key` is zeroized and `Secret_Key.Valid = False`

**Entropy:** Uses OS/hardware RNG via liboqs

**Example:**
```ada
declare
   Public  : ML_KEM_Public_Key;
   Secret  : ML_KEM_Secret_Key;
   Success : Boolean;
begin
   ML_KEM_Generate_Keypair (Public, Secret, Success);

   if not Success then
      raise Crypto_Error with "ML-KEM keypair generation failed";
   end if;

   -- Use keys...

   -- Cleanup
   Zeroize_ML_KEM_Secret (Secret);
end;
```

---

#### ML_KEM_Encapsulate

```ada
procedure ML_KEM_Encapsulate (
   Recipient_Public_Key : in     ML_KEM_Public_Key;
   Ciphertext           : out    ML_KEM_Ciphertext;
   Shared_Secret        : out    ML_KEM_Shared_Secret;
   Success              : out    Boolean
) with
   Post => (if Success then Is_Valid (Shared_Secret)
            else not Is_Valid (Shared_Secret));
```

**Description:** Encapsulate a shared secret for a recipient's public key.

**Parameters:**
- `Recipient_Public_Key` (in): Recipient's public key
- `Ciphertext` (out): Encapsulated ciphertext (1,568 bytes)
- `Shared_Secret` (out): Generated shared secret (32 bytes)
- `Success` (out): True if encapsulation succeeded

**Postconditions:**
- If successful, `Shared_Secret.Valid = True`
- If failed, `Shared_Secret` is zeroized

**Usage:** Sender generates random shared secret, encrypts it for recipient

**Example:**
```ada
ML_KEM_Encapsulate (
   Recipient_Public_Key => Alice_Public,
   Ciphertext           => CT,
   Shared_Secret        => Bob_Secret,
   Success              => OK
);

-- Send CT to Alice
-- Use Bob_Secret to derive encryption key
```

---

#### ML_KEM_Decapsulate

```ada
procedure ML_KEM_Decapsulate (
   Ciphertext    : in     ML_KEM_Ciphertext;
   Secret_Key    : in     ML_KEM_Secret_Key;
   Shared_Secret : out    ML_KEM_Shared_Secret;
   Success       : out    Boolean
) with
   Pre  => Is_Valid (Secret_Key),
   Post => (if Success then Is_Valid (Shared_Secret)
            else not Is_Valid (Shared_Secret));
```

**Description:** Decapsulate a shared secret using recipient's secret key.

**Parameters:**
- `Ciphertext` (in): Encapsulated ciphertext received from sender
- `Secret_Key` (in): Recipient's secret key
- `Shared_Secret` (out): Decapsulated shared secret (32 bytes)
- `Success` (out): True if decapsulation succeeded

**Preconditions:**
- `Secret_Key.Valid = True` (SPARK enforces)

**Postconditions:**
- If successful, `Shared_Secret.Valid = True` and matches sender's secret
- If failed, `Shared_Secret` is zeroized

**Example:**
```ada
-- Alice receives CT from Bob
ML_KEM_Decapsulate (
   Ciphertext    => CT,
   Secret_Key    => Alice_Secret,
   Shared_Secret => Alice_Shared,
   Success       => OK
);

-- Alice_Shared now equals Bob_Secret (if successful)
```

---

### ML-DSA-87 Operations

#### ML_DSA_Generate_Keypair

```ada
procedure ML_DSA_Generate_Keypair (
   Public_Key  : out ML_DSA_Public_Key;
   Secret_Key  : out ML_DSA_Secret_Key;
   Success     : out Boolean
) with
   Global => null,
   Post   => (if Success then Is_Valid (Secret_Key)
              else not Is_Valid (Secret_Key));
```

**Description:** Generate a fresh ML-DSA-87 keypair.

**Parameters:**
- `Public_Key` (out): Generated public key (2,592 bytes)
- `Secret_Key` (out): Generated secret key (4,896 bytes)
- `Success` (out): True if generation succeeded

**Example:**
```ada
ML_DSA_Generate_Keypair (Public, Secret, Success);
```

---

#### ML_DSA_Sign

```ada
procedure ML_DSA_Sign (
   Message        : in     Byte_Array;
   Secret_Key     : in     ML_DSA_Secret_Key;
   Signature      : out    ML_DSA_Signature;
   Success        : out    Boolean
) with
   Pre => Is_Valid (Secret_Key);
```

**Description:** Sign a message with ML-DSA-87.

**Parameters:**
- `Message` (in): Arbitrary-length message to sign
- `Secret_Key` (in): Signer's secret key
- `Signature` (out): Generated signature (4,627 bytes)
- `Success` (out): True if signing succeeded

**Preconditions:**
- `Secret_Key.Valid = True`

**Security:**
- Signature covers entire message (no truncation)
- Deterministic signing (no RNG failures)

**Example:**
```ada
declare
   Message : constant Byte_Array := (72, 101, 108, 108, 111);  -- "Hello"
   Sig     : ML_DSA_Signature;
   Success : Boolean;
begin
   ML_DSA_Sign (
      Message    => Message,
      Secret_Key => My_Secret_Key,
      Signature  => Sig,
      Success    => Success
   );

   if not Success then
      raise Crypto_Error with "Signing failed";
   end if;

   -- Transmit Message and Sig to recipient
end;
```

---

#### ML_DSA_Verify

```ada
function ML_DSA_Verify (
   Message     : Byte_Array;
   Signature   : ML_DSA_Signature;
   Public_Key  : ML_DSA_Public_Key
) return Boolean;
```

**Description:** Verify an ML-DSA-87 signature.

**Parameters:**
- `Message` (in): Message that was allegedly signed
- `Signature` (in): Signature to verify
- `Public_Key` (in): Signer's public key

**Returns:**
- `True` if signature is valid
- `False` if signature is invalid (tampered message or wrong key)

**Security:**
- **Always verify before trusting message**
- Constant-time verification (no timing leaks)

**Example:**
```ada
if ML_DSA_Verify (Message, Sig, Alice_Public_Key) then
   Put_Line ("Signature valid, message authentic");
   -- Proceed with message processing
else
   Put_Line ("Signature invalid, message tampered or wrong key");
   raise Security_Error with "Invalid signature";
end if;
```

---

### Utility Operations

#### Secrets_Match (Constant-Time Comparison)

```ada
function Secrets_Match (
   Secret_A : ML_KEM_Shared_Secret;
   Secret_B : ML_KEM_Shared_Secret
) return Boolean;
```

**Description:** Constant-time comparison of two shared secrets.

**Parameters:**
- `Secret_A`, `Secret_B`: Shared secrets to compare

**Returns:**
- `True` if secrets are identical
- `False` if secrets differ

**Security:**
- Uses `OQS_MEM_secure_bcmp` (constant-time, no early exit)
- Resistant to timing attacks
- Always compares all bytes

**Usage:** Testing that encapsulation and decapsulation produce the same secret.

**Example:**
```ada
-- Bob encapsulates
ML_KEM_Encapsulate (Alice_Public, CT, Bob_Secret, OK);

-- Alice decapsulates
ML_KEM_Decapsulate (CT, Alice_Secret, Alice_Secret_Derived, OK);

-- Verify they match (should always be true if successful)
if not Secrets_Match (Bob_Secret, Alice_Secret_Derived) then
   raise Crypto_Error with "Shared secrets do not match!";
end if;
```

---

#### Zeroization

##### Zeroize_Shared_Secret

```ada
procedure Zeroize_Shared_Secret (
   Secret : in out ML_KEM_Shared_Secret
) with
   Post => not Is_Valid (Secret);
```

**Description:** Securely zeroize a shared secret.

**Security:**
- Uses `OQS_MEM_cleanse` (cannot be optimized away)
- Volatile type prevents reordering
- SPARK proves zeroization occurs

**Example:**
```ada
-- Use shared secret
Derive_Encryption_Key (Shared_Secret, Enc_Key, OK);

-- Immediately zeroize after derivation
Zeroize_Shared_Secret (Shared_Secret);
```

---

##### Zeroize_ML_KEM_Secret

```ada
procedure Zeroize_ML_KEM_Secret (
   Secret_Key : in out ML_KEM_Secret_Key
) with
   Post => not Is_Valid (Secret_Key);
```

**Description:** Securely zeroize ML-KEM-1024 secret key.

**Usage:** When rotating keys or destroying identity.

---

##### Zeroize_ML_DSA_Secret

```ada
procedure Zeroize_ML_DSA_Secret (
   Secret_Key : in out ML_DSA_Secret_Key
) with
   Post => not Is_Valid (Secret_Key);
```

**Description:** Securely zeroize ML-DSA-87 secret key.

---

### Hybrid Operations (Partial Implementation)

#### Hybrid_Encapsulate

```ada
procedure Hybrid_Encapsulate (
   X25519_Public  : in     X25519_Public_Key;
   ML_KEM_Public  : in     ML_KEM_Public_Key;
   X25519_Ephemeral_Secret : out X25519_Secret_Key;
   Ciphertext     : out    ML_KEM_Ciphertext;
   Hybrid_Secret  : out    Hybrid_Shared_Secret;
   Success        : out    Boolean
);
```

**Description:** Hybrid key encapsulation (classical + post-quantum).

**Current Status:** Partial (ML-KEM only, X25519 pending)

**Full Implementation:**
1. Perform X25519 ECDH → Classical_Shared
2. Perform ML-KEM-1024 Encapsulation → PQ_Shared
3. Combine with HKDF: `HKDF(Classical || PQ, "anubis-hybrid-kem", 32)`

---

#### Derive_Encryption_Key

```ada
procedure Derive_Encryption_Key (
   Hybrid_Secret : in     Hybrid_Shared_Secret;
   Encryption_Key : out    XChaCha20_Key;
   Success        : out    Boolean
);
```

**Description:** Derive XChaCha20 encryption key from hybrid shared secret.

**Current Status:** Placeholder (uses first 32 bytes of PQ secret)

**Full Implementation:** HKDF-SHA256 with context string

---

## OQS_Common

**File:** `src/crypto/liboqs/oqs_common.ads`
**Purpose:** Common liboqs functions and types

### Types

#### OQS_STATUS

```ada
type OQS_STATUS is new Interfaces.C.int;

OQS_SUCCESS : constant OQS_STATUS := 0;
OQS_ERROR   : constant OQS_STATUS := -1;
OQS_EXTERNAL_LIB_ERROR_OPENSSL : constant OQS_STATUS := 50;
```

**Description:** Return status from liboqs functions.

---

### Procedures

#### OQS_init

```ada
procedure OQS_init with
   Import, Convention => C, External_Name => "OQS_init";
```

**Description:** Initialize liboqs library (prefetch OpenSSL objects).

**Usage:** Call once at program startup (before any crypto operations).

**Example:**
```ada
OQS_init;
-- Now safe to call ML-KEM, ML-DSA functions
```

---

#### OQS_destroy

```ada
procedure OQS_destroy with
   Import, Convention => C, External_Name => "OQS_destroy";
```

**Description:** Cleanup and free prefetched OpenSSL objects.

**Usage:** Call once at program exit (after all crypto operations).

**Example:**
```ada
-- Program exit
OQS_destroy;
```

---

#### OQS_MEM_cleanse

```ada
procedure OQS_MEM_cleanse (
   ptr : System.Address;
   len : Interfaces.C.size_t
) with
   Import, Convention => C, External_Name => "OQS_MEM_cleanse";
```

**Description:** Securely zero memory (protected against compiler optimization).

**Parameters:**
- `ptr`: Address of memory to zeroize
- `len`: Number of bytes to zeroize

**Security:** Cannot be optimized away (unlike `memset`)

**Usage:** Internal (used by `Zeroize_*` procedures)

---

#### OQS_MEM_secure_bcmp

```ada
function OQS_MEM_secure_bcmp (
   a   : System.Address;
   b   : System.Address;
   len : Interfaces.C.size_t
) return Interfaces.C.int with
   Import, Convention => C, External_Name => "OQS_MEM_secure_bcmp";
```

**Description:** Constant-time memory comparison.

**Parameters:**
- `a`, `b`: Addresses of memory to compare
- `len`: Number of bytes to compare

**Returns:**
- `0` if memory regions are equal
- `1` if memory regions differ

**Security:** Constant-time (no early exit, timing-attack resistant)

**Usage:** Internal (used by `Secrets_Match`)

---

## OQS_KEM_ML_KEM

**File:** `src/crypto/liboqs/oqs_kem_ml_kem.ads`
**Purpose:** ML-KEM-1024 C FFI bindings

### Constants

```ada
ML_KEM_1024_LENGTH_PUBLIC_KEY      : constant := 1_568;
ML_KEM_1024_LENGTH_SECRET_KEY      : constant := 3_168;
ML_KEM_1024_LENGTH_CIPHERTEXT      : constant := 1_568;
ML_KEM_1024_LENGTH_SHARED_SECRET   : constant := 32;
```

---

### Functions

#### OQS_KEM_ml_kem_1024_keypair

```ada
function OQS_KEM_ml_kem_1024_keypair (
   public_key : System.Address;
   secret_key : System.Address
) return OQS_STATUS with
   Import, Convention => C,
   External_Name => "OQS_KEM_ml_kem_1024_keypair";
```

**Description:** C FFI binding for ML-KEM-1024 keypair generation.

**Usage:** Internal (called by `ML_KEM_Generate_Keypair`)

---

## OQS_SIG_ML_DSA

**File:** `src/crypto/liboqs/oqs_sig_ml_dsa.ads`
**Purpose:** ML-DSA-87 C FFI bindings

### Constants

```ada
ML_DSA_87_LENGTH_PUBLIC_KEY  : constant := 2_592;
ML_DSA_87_LENGTH_SECRET_KEY  : constant := 4_896;
ML_DSA_87_LENGTH_SIGNATURE   : constant := 4_627;
```

---

### Functions

Similar C FFI bindings for ML-DSA-87 operations (see file for details).

---

## Usage Examples

### Complete ML-KEM-1024 Exchange

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.PQC; use Anubis_Types.PQC;
with OQS_Common; use OQS_Common;

procedure ML_KEM_Example is
   -- Alice's keypair
   Alice_Public  : ML_KEM_Public_Key;
   Alice_Secret  : ML_KEM_Secret_Key;

   -- Bob's ephemeral secret and ciphertext
   Bob_Ciphertext : ML_KEM_Ciphertext;
   Bob_Secret     : ML_KEM_Shared_Secret;

   -- Alice's derived secret
   Alice_Secret_Derived : ML_KEM_Shared_Secret;

   Success : Boolean;
begin
   -- Initialize liboqs
   OQS_init;

   -- 1. Alice generates keypair
   ML_KEM_Generate_Keypair (Alice_Public, Alice_Secret, Success);
   if not Success then
      raise Crypto_Error with "Alice keypair generation failed";
   end if;
   Put_Line ("✓ Alice generated keypair");

   -- 2. Alice sends Alice_Public to Bob (out-of-band, public channel OK)

   -- 3. Bob encapsulates (generates random shared secret)
   ML_KEM_Encapsulate (
      Recipient_Public_Key => Alice_Public,
      Ciphertext           => Bob_Ciphertext,
      Shared_Secret        => Bob_Secret,
      Success              => Success
   );
   if not Success then
      raise Crypto_Error with "Bob encapsulation failed";
   end if;
   Put_Line ("✓ Bob encapsulated shared secret");

   -- 4. Bob sends Bob_Ciphertext to Alice (public channel OK)

   -- 5. Alice decapsulates (derives same shared secret)
   ML_KEM_Decapsulate (
      Ciphertext    => Bob_Ciphertext,
      Secret_Key    => Alice_Secret,
      Shared_Secret => Alice_Secret_Derived,
      Success       => Success
   );
   if not Success then
      raise Crypto_Error with "Alice decapsulation failed";
   end if;
   Put_Line ("✓ Alice decapsulated shared secret");

   -- 6. Verify secrets match
   if Secrets_Match (Bob_Secret, Alice_Secret_Derived) then
      Put_Line ("✓ Shared secrets match!");
   else
      raise Crypto_Error with "Shared secrets DO NOT MATCH!";
   end if;

   -- 7. Use shared secret to derive encryption key
   --    (e.g., HKDF, XChaCha20-Poly1305)

   -- 8. Secure cleanup
   Zeroize_Shared_Secret (Bob_Secret);
   Zeroize_Shared_Secret (Alice_Secret_Derived);
   Zeroize_ML_KEM_Secret (Alice_Secret);
   Put_Line ("✓ Secrets zeroized");

   -- Cleanup liboqs
   OQS_destroy;
end ML_KEM_Example;
```

---

### Complete ML-DSA-87 Sign/Verify

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.PQC; use Anubis_Types.PQC;
with OQS_Common; use OQS_Common;

procedure ML_DSA_Example is
   -- Alice's keypair
   Alice_Public  : ML_DSA_Public_Key;
   Alice_Secret  : ML_DSA_Secret_Key;

   -- Message to sign
   Message : constant Byte_Array := (72, 101, 108, 108, 111);  -- "Hello"

   -- Signature
   Sig : ML_DSA_Signature;

   Success : Boolean;
begin
   OQS_init;

   -- 1. Alice generates keypair
   ML_DSA_Generate_Keypair (Alice_Public, Alice_Secret, Success);
   if not Success then
      raise Crypto_Error with "Keypair generation failed";
   end if;
   Put_Line ("✓ Alice generated signing keypair");

   -- 2. Alice signs message
   ML_DSA_Sign (
      Message    => Message,
      Secret_Key => Alice_Secret,
      Signature  => Sig,
      Success    => Success
   );
   if not Success then
      raise Crypto_Error with "Signing failed";
   end if;
   Put_Line ("✓ Alice signed message");

   -- 3. Alice sends Message + Sig + Alice_Public to Bob

   -- 4. Bob verifies signature
   if ML_DSA_Verify (Message, Sig, Alice_Public) then
      Put_Line ("✓ Signature verified! Message is authentic.");
   else
      Put_Line ("✗ Signature INVALID! Message tampered or wrong key.");
      raise Security_Error with "Invalid signature";
   end if;

   -- 5. Test: Tampered message should fail verification
   declare
      Tampered_Message : Byte_Array := Message;
   begin
      Tampered_Message (1) := Tampered_Message (1) + 1;  -- Flip one bit

      if ML_DSA_Verify (Tampered_Message, Sig, Alice_Public) then
         raise Crypto_Error with "BUG: Tampered message verified!";
      else
         Put_Line ("✓ Tampered message correctly rejected");
      end if;
   end;

   -- 6. Cleanup
   Zeroize_ML_DSA_Secret (Alice_Secret);
   Put_Line ("✓ Secret key zeroized");

   OQS_destroy;
end ML_DSA_Example;
```

---

## Error Handling

### Success Indicators

All cryptographic operations use an `out Boolean` parameter:

```ada
procedure Operation (...; Success : out Boolean);
```

**Always check `Success` before using outputs:**

```ada
ML_KEM_Generate_Keypair (Public, Secret, Success);
if not Success then
   raise Crypto_Error with "Operation failed";
end if;
-- Safe to use Public and Secret here
```

---

### Failure Behavior

**On failure:**
- All secret outputs are zeroized
- `Valid` flag set to `False`
- `Success` parameter set to `False`

**SPARK guarantees:**
- Postconditions ensure zeroization
- No uninitialized variables

---

## Best Practices

### 1. Always Initialize liboqs

```ada
-- Program start
OQS_init;

-- Crypto operations
...

-- Program end
OQS_destroy;
```

---

### 2. Check Validity Before Use

```ada
-- ✅ GOOD
procedure Decrypt (Key : in ML_KEM_Secret_Key) with
   Pre => Is_Valid (Key);  -- SPARK enforces this!

-- ❌ BAD
procedure Decrypt (Key : in ML_KEM_Secret_Key);
-- No guarantee key is valid
```

---

### 3. Zeroize Secrets Immediately After Use

```ada
-- ✅ GOOD
Derive_Key (Shared_Secret, Encryption_Key, OK);
Zeroize_Shared_Secret (Shared_Secret);  -- Immediately!

-- ❌ BAD
Derive_Key (Shared_Secret, Encryption_Key, OK);
-- ... many operations ...
Zeroize_Shared_Secret (Shared_Secret);  -- Too late, exposed in memory
```

---

### 4. Use Constant-Time Comparison

```ada
-- ✅ GOOD
if Secrets_Match (Secret_A, Secret_B) then

-- ❌ BAD
if Secret_A.Data = Secret_B.Data then  -- Timing leak!
```

---

### 5. Verify Signatures Before Trusting Messages

```ada
-- ✅ GOOD
if ML_DSA_Verify (Message, Sig, Public_Key) then
   Process_Message (Message);  -- Safe
else
   raise Security_Error;
end if;

-- ❌ BAD
Process_Message (Message);  -- Trust without verification
ML_DSA_Verify (Message, Sig, Public_Key);  -- Too late!
```

---

### 6. Handle All Error Paths

```ada
-- ✅ GOOD
ML_KEM_Decapsulate (CT, Secret, Shared, Success);
if not Success then
   Log_Error ("Decapsulation failed");
   return;  -- Abort operation
end if;
-- Continue with Shared

-- ❌ BAD
ML_KEM_Decapsulate (CT, Secret, Shared, Success);
-- Continue regardless of Success value
Use_Secret (Shared);  -- Might be invalid!
```

---

## References

- **NIST FIPS 203 (ML-KEM):** https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.203.pdf
- **NIST FIPS 204 (ML-DSA):** https://nvlpubs.nist.gov/nistpubs/fips/nist.fips.204.pdf
- **liboqs Documentation:** https://github.com/open-quantum-safe/liboqs/wiki
- **SPARK User's Guide:** https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/

---

**Last Updated:** 2025-10-10
**API Version:** 1.0 (Phase 1)
**Implementation Status:** ML-KEM-1024 and ML-DSA-87 complete
