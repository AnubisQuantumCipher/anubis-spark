# ANUBIS-SPARK API Reference

This document provides a comprehensive reference for the ANUBIS-SPARK Ada API, designed for developers using the library through Alire.

## Package Overview

ANUBIS-SPARK is organized into several packages, each providing specific functionality:

```ada
Anubis_Types                    -- Core types and constants
├── Anubis_Types.Classical      -- Classical cryptography (X25519, Ed25519)
├── Anubis_Types.PQC           -- Post-quantum cryptography (ML-KEM, ML-DSA)
├── Anubis_Types.Storage       -- Key management and storage
├── Anubis_Types.Streaming     -- File encryption/decryption
└── Anubis_Key_Manager         -- Advanced key lifecycle management
```

## Core Types (`Anubis_Types`)

### Basic Types

```ada
package Anubis_Types is
   pragma Pure;
   pragma SPARK_Mode (On);

   -- Byte array for cryptographic data
   type Byte_Array is array (Positive range <>) of Interfaces.Unsigned_8;
   
   -- Fixed-size arrays for keys and nonces
   subtype Key_256 is Byte_Array (1 .. 32);      -- 256-bit keys
   subtype Key_512 is Byte_Array (1 .. 64);      -- 512-bit keys
   subtype Nonce_96 is Byte_Array (1 .. 12);     -- 96-bit nonces
   subtype Nonce_192 is Byte_Array (1 .. 24);    -- 192-bit nonces (XChaCha20)
   
   -- Authentication tags
   subtype Auth_Tag is Byte_Array (1 .. 16);     -- Poly1305 MAC
   
   -- Result codes for operations
   type Result_Code is (
      Success,           -- Operation completed successfully
      IO_Error,          -- File I/O error
      Crypto_Error,      -- Cryptographic operation failed
      Invalid_Format,    -- Invalid file format
      Auth_Failed        -- Authentication tag verification failed
   );
end Anubis_Types;
```

## Key Management (`Anubis_Types.Storage`)

### Identity Type

```ada
package Anubis_Types.Storage is
   pragma SPARK_Mode (On);

   -- Hybrid identity containing both classical and post-quantum keys
   type Identity_Type is private;
   
   -- Identity status
   type Identity_Status is (Invalid, Valid, Expired, Destroyed);
   
   -- Key generation
   procedure Generate_Identity (Identity : out Identity_Type)
   with
      Post => Is_Valid (Identity);
   
   -- Identity validation
   function Is_Valid (Identity : Identity_Type) return Boolean;
   
   function Get_Status (Identity : Identity_Type) return Identity_Status;
   
   -- File operations
   procedure Save_Identity (
      Identity : in Identity_Type;
      Filename : in String
   ) with
      Pre => Is_Valid (Identity) and Filename'Length > 0;
   
   procedure Load_Identity (
      Identity : out Identity_Type;
      Filename : in String;
      Success  : out Boolean
   ) with
      Pre => Filename'Length > 0;
   
   -- Secure destruction
   procedure Zeroize_Identity (Identity : in out Identity_Type)
   with
      Post => Get_Status (Identity) = Destroyed;

end Anubis_Types.Storage;
```

### Usage Example

```ada
with Anubis_Types.Storage;

procedure Key_Management_Example is
   Identity : Anubis_Types.Storage.Identity_Type;
   Success  : Boolean;
begin
   -- Generate new identity
   Anubis_Types.Storage.Generate_Identity (Identity);
   
   -- Save to file
   Anubis_Types.Storage.Save_Identity (Identity, "my_key.key");
   
   -- Load from file
   Anubis_Types.Storage.Load_Identity (Identity, "my_key.key", Success);
   
   if Success and then Anubis_Types.Storage.Is_Valid (Identity) then
      -- Use identity for encryption/decryption
      null;
   end if;
   
   -- Secure cleanup
   Anubis_Types.Storage.Zeroize_Identity (Identity);
end Key_Management_Example;
```

## File Encryption (`Anubis_Types.Streaming`)

### Core Procedures

```ada
package Anubis_Types.Streaming is
   pragma SPARK_Mode (On);

   -- File encryption with streaming AEAD
   procedure Encrypt_File (
      Identity_Key : in Anubis_Types.Storage.Identity_Type;
      Input_File   : in String;
      Output_File  : in String;
      Result       : out Result_Code;
      Chunk_Size   : in Positive := 67_108_864  -- 64 MB default
   ) with
      Pre => Anubis_Types.Storage.Is_Valid (Identity_Key)
         and Input_File'Length > 0
         and Output_File'Length > 0
         and Input_File /= Output_File;
   
   -- File decryption with integrity verification
   procedure Decrypt_File (
      Identity_Key : in Anubis_Types.Storage.Identity_Type;
      Input_File   : in String;
      Output_File  : in String;
      Result       : out Result_Code
   ) with
      Pre => Anubis_Types.Storage.Is_Valid (Identity_Key)
         and Input_File'Length > 0
         and Output_File'Length > 0
         and Input_File /= Output_File;
   
   -- Memory-based encryption (for smaller data)
   procedure Encrypt_Data (
      Identity_Key : in Anubis_Types.Storage.Identity_Type;
      Plaintext    : in Byte_Array;
      Ciphertext   : out Byte_Array;
      Result       : out Result_Code
   ) with
      Pre => Anubis_Types.Storage.Is_Valid (Identity_Key)
         and Plaintext'Length > 0
         and Ciphertext'Length >= Plaintext'Length + 1686;  -- Overhead
   
   -- Memory-based decryption
   procedure Decrypt_Data (
      Identity_Key : in Anubis_Types.Storage.Identity_Type;
      Ciphertext   : in Byte_Array;
      Plaintext    : out Byte_Array;
      Result       : out Result_Code
   ) with
      Pre => Anubis_Types.Storage.Is_Valid (Identity_Key)
         and Ciphertext'Length > 1686;  -- Minimum size with headers

end Anubis_Types.Streaming;
```

### Usage Example

```ada
with Ada.Text_IO;
with Anubis_Types.Storage;
with Anubis_Types.Streaming;

procedure File_Encryption_Example is
   Identity : Anubis_Types.Storage.Identity_Type;
   Result   : Anubis_Types.Streaming.Result_Code;
begin
   -- Generate or load identity
   Anubis_Types.Storage.Generate_Identity (Identity);
   
   -- Encrypt file
   Anubis_Types.Streaming.Encrypt_File (
      Identity_Key => Identity,
      Input_File   => "document.pdf",
      Output_File  => "document.pdf.anubis",
      Result       => Result
   );
   
   case Result is
      when Anubis_Types.Streaming.Success =>
         Ada.Text_IO.Put_Line ("Encryption successful!");
         
      when Anubis_Types.Streaming.IO_Error =>
         Ada.Text_IO.Put_Line ("File I/O error");
         
      when Anubis_Types.Streaming.Crypto_Error =>
         Ada.Text_IO.Put_Line ("Cryptographic error");
         
      when others =>
         Ada.Text_IO.Put_Line ("Unexpected error");
   end case;
   
   -- Decrypt file
   if Result = Anubis_Types.Streaming.Success then
      Anubis_Types.Streaming.Decrypt_File (
         Identity_Key => Identity,
         Input_File   => "document.pdf.anubis",
         Output_File  => "document_decrypted.pdf",
         Result       => Result
      );
   end if;
   
   -- Cleanup
   Anubis_Types.Storage.Zeroize_Identity (Identity);
end File_Encryption_Example;
```

## Classical Cryptography (`Anubis_Types.Classical`)

### Key Types

```ada
package Anubis_Types.Classical is
   pragma SPARK_Mode (On);

   -- X25519 key exchange
   subtype X25519_Private_Key is Byte_Array (1 .. 32);
   subtype X25519_Public_Key is Byte_Array (1 .. 32);
   subtype X25519_Shared_Secret is Byte_Array (1 .. 32);
   
   -- Ed25519 signatures
   subtype Ed25519_Private_Key is Byte_Array (1 .. 32);
   subtype Ed25519_Public_Key is Byte_Array (1 .. 32);
   subtype Ed25519_Signature is Byte_Array (1 .. 64);
   
   -- Key generation
   procedure Generate_X25519_Keypair (
      Private_Key : out X25519_Private_Key;
      Public_Key  : out X25519_Public_Key
   );
   
   procedure Generate_Ed25519_Keypair (
      Private_Key : out Ed25519_Private_Key;
      Public_Key  : out Ed25519_Public_Key
   );
   
   -- Key exchange
   procedure X25519_Compute_Shared_Secret (
      Private_Key   : in X25519_Private_Key;
      Public_Key    : in X25519_Public_Key;
      Shared_Secret : out X25519_Shared_Secret;
      Success       : out Boolean
   );
   
   -- Digital signatures
   procedure Ed25519_Sign (
      Private_Key : in Ed25519_Private_Key;
      Message     : in Byte_Array;
      Signature   : out Ed25519_Signature
   );
   
   function Ed25519_Verify (
      Public_Key : in Ed25519_Public_Key;
      Message    : in Byte_Array;
      Signature  : in Ed25519_Signature
   ) return Boolean;

end Anubis_Types.Classical;
```

## Post-Quantum Cryptography (`Anubis_Types.PQC`)

### ML-KEM-1024 (Key Encapsulation)

```ada
package Anubis_Types.PQC is
   pragma SPARK_Mode (On);

   -- ML-KEM-1024 types
   subtype ML_KEM_Private_Key is Byte_Array (1 .. 3168);
   subtype ML_KEM_Public_Key is Byte_Array (1 .. 1568);
   subtype ML_KEM_Ciphertext is Byte_Array (1 .. 1568);
   subtype ML_KEM_Shared_Secret is Byte_Array (1 .. 32);
   
   -- ML-DSA-87 types
   subtype ML_DSA_Private_Key is Byte_Array (1 .. 4864);
   subtype ML_DSA_Public_Key is Byte_Array (1 .. 2592);
   subtype ML_DSA_Signature is Byte_Array (1 .. 4627);
   
   -- Key generation
   procedure Generate_ML_KEM_Keypair (
      Private_Key : out ML_KEM_Private_Key;
      Public_Key  : out ML_KEM_Public_Key;
      Success     : out Boolean
   );
   
   procedure Generate_ML_DSA_Keypair (
      Private_Key : out ML_DSA_Private_Key;
      Public_Key  : out ML_DSA_Public_Key;
      Success     : out Boolean
   );
   
   -- Key encapsulation
   procedure ML_KEM_Encapsulate (
      Public_Key    : in ML_KEM_Public_Key;
      Ciphertext    : out ML_KEM_Ciphertext;
      Shared_Secret : out ML_KEM_Shared_Secret;
      Success       : out Boolean
   );
   
   procedure ML_KEM_Decapsulate (
      Private_Key   : in ML_KEM_Private_Key;
      Ciphertext    : in ML_KEM_Ciphertext;
      Shared_Secret : out ML_KEM_Shared_Secret;
      Success       : out Boolean
   );
   
   -- Digital signatures
   procedure ML_DSA_Sign (
      Private_Key : in ML_DSA_Private_Key;
      Message     : in Byte_Array;
      Signature   : out ML_DSA_Signature;
      Success     : out Boolean
   );
   
   function ML_DSA_Verify (
      Public_Key : in ML_DSA_Public_Key;
      Message    : in Byte_Array;
      Signature  : in ML_DSA_Signature
   ) return Boolean;

end Anubis_Types.PQC;
```

## Advanced Key Management (`Anubis_Key_Manager`)

### Key Lifecycle Management

```ada
package Anubis_Key_Manager is
   pragma SPARK_Mode (On);

   type Key_Manager_Type is private;
   
   type Key_Status is (Active, Expired, Destroyed);
   
   -- Manager operations
   procedure Create_Manager (
      Manager  : out Key_Manager_Type;
      Identity : in Anubis_Types.Storage.Identity_Type
   ) with
      Pre => Anubis_Types.Storage.Is_Valid (Identity);
   
   function Get_Status (Manager : Key_Manager_Type) return Key_Status;
   
   function Get_Usage_Count (Manager : Key_Manager_Type) return Natural;
   
   -- Key rotation
   procedure Rotate_Keys (
      Manager     : in out Key_Manager_Type;
      New_Identity : out Anubis_Types.Storage.Identity_Type;
      Success     : out Boolean
   ) with
      Pre => Get_Status (Manager) = Active;
   
   -- Automatic expiration
   procedure Check_Expiration (
      Manager : in out Key_Manager_Type;
      Max_Age : in Duration := 7776000.0  -- 90 days
   );
   
   procedure Check_Usage_Limit (
      Manager   : in out Key_Manager_Type;
      Max_Usage : in Natural := 1_000_000
   );
   
   -- Secure destruction
   procedure Destroy_Manager (Manager : in out Key_Manager_Type)
   with
      Post => Get_Status (Manager) = Destroyed;

end Anubis_Key_Manager;
```

## Error Handling

### Result Codes

All operations return `Result_Code` values:

```ada
type Result_Code is (
   Success,           -- Operation completed successfully
   IO_Error,          -- File I/O error (permissions, disk space, etc.)
   Crypto_Error,      -- Cryptographic operation failed
   Invalid_Format,    -- Invalid file format or corrupted data
   Auth_Failed        -- Authentication tag verification failed
);
```

### Exception Safety

ANUBIS-SPARK is designed to be exception-safe:
- All procedures use `out` parameters for results instead of exceptions
- SPARK verification ensures no runtime errors
- Automatic cleanup of sensitive data on all exit paths

## Performance Considerations

### Memory Usage
- **Constant Memory**: 64 MB regardless of file size
- **Stack Usage**: Minimal due to streaming design
- **Key Storage**: ~12 KB per identity (hybrid keys)

### Throughput
- **Encryption**: 20-25 MB/s (typical)
- **Decryption**: 13-18 MB/s (typical)
- **Chunk Size**: 64 MB default (configurable)

### Optimization Tips

```ada
-- Use release mode for production
pragma Optimize (Time);

-- Large chunk sizes for better performance
Anubis_Types.Streaming.Encrypt_File (
   Identity_Key => Identity,
   Input_File   => "large_file.bin",
   Output_File  => "large_file.bin.anubis",
   Result       => Result,
   Chunk_Size   => 134_217_728  -- 128 MB chunks
);
```

## Security Best Practices

### Key Management
1. **Generate fresh keys** for each application/user
2. **Store keys securely** with proper file permissions (600)
3. **Use key rotation** for long-term deployments
4. **Implement secure backup** using Shamir Secret Sharing

### File Handling
1. **Verify Result_Code** after every operation
2. **Use secure deletion** for temporary files
3. **Validate file integrity** before processing
4. **Implement proper logging** for audit trails

### Production Deployment
1. **Use release builds** (`alr build --release`)
2. **Verify SPARK proofs** before deployment
3. **Test with representative data** sizes
4. **Monitor performance** and memory usage

## SPARK Verification

### Proof Levels
- **Stone**: Valid SPARK subset ✓
- **Bronze**: Flow analysis ✓
- **Silver**: Absence of runtime errors ✓
- **Gold**: Integrity properties ✓
- **Platinum**: Functional correctness ✓

### Verification Commands

```bash
# Full verification (requires GNATprove)
gnatprove -P anubis_spark.gpr --level=4 --report=all

# Quick check
gnatprove -P anubis_spark.gpr --mode=check

# Generate proof report
gnatprove -P anubis_spark.gpr --report=provers
```

## Examples Repository

Complete examples are available in the [examples directory](https://github.com/AnubisQuantumCipher/anubis-spark/tree/main/examples):

- **Basic Encryption**: Simple file encryption/decryption
- **Key Management**: Advanced key lifecycle management
- **Streaming**: Large file processing
- **Integration**: Using with other Ada libraries

---

For more information, see the [complete documentation](https://github.com/AnubisQuantumCipher/anubis-spark) and [SPARK certification details](PLATINUM_CERTIFICATION.md).
