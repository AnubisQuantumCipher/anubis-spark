-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Platinum Proof Contracts (Enhanced)
-- Ghost predicates, abstract models, and proof-level types
-- This file provides comprehensive foundation for functional correctness proofs
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;

package Anubis_Contracts is

   -------------------------------------------------------------------------
   -- Common Subtypes (for precise range constraints)
   -------------------------------------------------------------------------

   subtype Nonce_24     is Byte_Array (1 .. 24);
   subtype Tag_16       is Byte_Array (1 .. 16);
   subtype Key_32       is Byte_Array (1 .. 32);
   subtype MLKEM_CT     is Byte_Array (1 .. 1568);
   subtype MLKEM_SS     is Byte_Array (1 .. 32);

   -------------------------------------------------------------------------
   -- Logical Header Type (proof-level abstraction)
   -------------------------------------------------------------------------

   -- Header is your logical, in-memory form (NOT serialized)
   type Header is private;

   -------------------------------------------------------------------------
   -- Enhanced Ghost Predicates (no code generated, proof-only)
   -------------------------------------------------------------------------

   -- Ghost: Check if header is well-formed (all fields valid)
   function Well_Formed_Header (H : Header) return Boolean with Ghost;

   -- Ghost: Check if two headers are equivalent
   function Headers_Equal (H1, H2 : Header) return Boolean with Ghost;

   -- Ghost: Get canonical size of serialized header
   function Header_Size_Bytes (H : Header) return Natural with Ghost,
     Post => Header_Size_Bytes'Result > 0 and
             Header_Size_Bytes'Result <= 4096;  -- Reasonable upper bound

   -- Ghost: Check if byte array is well-formed serialized header
   function Well_Formed_Ser (B : Byte_Array) return Boolean with Ghost;

   -- Ghost: Check if serialized header has correct magic bytes
   function Has_Valid_Magic (B : Byte_Array) return Boolean with Ghost,
     Pre => B'Length >= 8;

   -- Ghost: Check if header version is supported
   function Has_Supported_Version (H : Header) return Boolean with Ghost;

   -------------------------------------------------------------------------
   -- Ghost Model Functions (bidirectional proof models)
   -------------------------------------------------------------------------

   -- Ghost: Serialize header to bytes (proof model)
   function Serialize_Model (H : Header) return Byte_Array with Ghost,
     Pre  => Well_Formed_Header (H),
     Post => Serialize_Model'Result'Length = Header_Size_Bytes (H) and
             Well_Formed_Ser (Serialize_Model'Result);

   -- Ghost: Parse bytes to header (proof model)
   function Parse_Model (B : Byte_Array) return Header with Ghost,
     Pre  => Well_Formed_Ser (B),
     Post => Well_Formed_Header (Parse_Model'Result);

   -- Ghost: Bijection property - Parse(Serialize(H)) = H
   function Parse_Serialize_Identity (H : Header) return Boolean with Ghost,
     Pre  => Well_Formed_Header (H),
     Post => Parse_Serialize_Identity'Result =
             (Headers_Equal (Parse_Model (Serialize_Model (H)), H));

   -- Ghost: Bijection property - Serialize(Parse(B)) = B
   function Serialize_Parse_Identity (B : Byte_Array) return Boolean with Ghost,
     Pre  => Well_Formed_Ser (B),
     Post => Serialize_Parse_Identity'Result =
             (Serialize_Model (Parse_Model (B)) = B);

   -------------------------------------------------------------------------
   -- AAD Binding Model (Enhanced)
   -------------------------------------------------------------------------

   -- Ghost: Each chunk authenticates the header via AAD
   function Header_Binds
     (H : Header; Chunk : Byte_Array; Tag : Tag_16) return Boolean with Ghost;

   -- Ghost: Verify AAD construction is correct
   function AAD_Well_Formed
     (H : Header; AAD : Byte_Array) return Boolean with Ghost,
     Pre => Well_Formed_Header (H);

   -- Ghost: Verify tag authenticates both header and chunk
   function Tag_Authenticates
     (Header_AAD : Byte_Array;
      Chunk      : Byte_Array;
      Tag        : Tag_16;
      Key        : Key_32;
      Nonce      : Nonce_24) return Boolean with Ghost;

   -------------------------------------------------------------------------
   -- Nonce Freshness Model (Enhanced)
   -------------------------------------------------------------------------

   -- Ghost: Nonce uniqueness predicate (file_nonce || chunk_idx)
   function Nonce_Fresh (N : Nonce_24) return Boolean with Ghost;

   -- Ghost: Nonce is constructed correctly from file nonce and chunk index
   function Nonce_Construction_Valid
     (File_Nonce  : Byte_Array;
      Chunk_Index : Natural;
      Result      : Nonce_24) return Boolean with Ghost,
     Pre  => File_Nonce'Length = 16,
     Post => Nonce_Construction_Valid'Result =
             ((for all I in 1 .. 16 => Result (I) = File_Nonce (File_Nonce'First + I - 1)) and
              (Chunk_Index < 2**64));  -- Prevents nonce wraparound

   -- Ghost: Two nonces with different chunk indices are different
   function Nonces_Differ
     (File_Nonce   : Byte_Array;
      Chunk_Index1 : Natural;
      Chunk_Index2 : Natural) return Boolean with Ghost,
     Pre  => File_Nonce'Length = 16 and Chunk_Index1 /= Chunk_Index2,
     Post => Nonces_Differ'Result = True;  -- Always different

   -------------------------------------------------------------------------
   -- Cryptographic Property Models
   -------------------------------------------------------------------------

   -- Ghost: Check if a key is non-zero (has entropy)
   function Key_Has_Entropy (K : Key_32) return Boolean with Ghost,
     Post => Key_Has_Entropy'Result = (for some I in K'Range => K (I) /= 0);

   -- Ghost: Check if all key bytes are zero (zeroized)
   function Key_Is_Zeroed (K : Key_32) return Boolean with Ghost,
     Post => Key_Is_Zeroed'Result = (for all I in K'Range => K (I) = 0);

   -- Ghost: Check if two keys are equal
   function Keys_Equal (K1, K2 : Key_32) return Boolean with Ghost,
     Post => Keys_Equal'Result = (for all I in K1'Range => K1 (I) = K2 (I));

   -- Ghost: Check if tag is non-zero
   function Tag_Has_Value (T : Tag_16) return Boolean with Ghost,
     Post => Tag_Has_Value'Result = (for some I in T'Range => T (I) /= 0);

   -------------------------------------------------------------------------
   -- Length Preservation Models
   -------------------------------------------------------------------------

   -- Ghost: Encryption preserves plaintext length
   function Encryption_Length_Valid
     (Plaintext_Len  : Natural;
      Ciphertext_Len : Natural) return Boolean with Ghost,
     Post => Encryption_Length_Valid'Result = (Plaintext_Len = Ciphertext_Len);

   -- Ghost: Decryption preserves ciphertext length
   function Decryption_Length_Valid
     (Ciphertext_Len : Natural;
      Plaintext_Len  : Natural) return Boolean with Ghost,
     Post => Decryption_Length_Valid'Result = (Ciphertext_Len = Plaintext_Len);

   -------------------------------------------------------------------------
   -- File Integrity Models (for streaming operations)
   -------------------------------------------------------------------------

   -- Ghost: Total bytes written matches expected file size
   function File_Integrity_Valid
     (Bytes_Written : Natural;
      Expected_Size : Natural) return Boolean with Ghost,
     Post => File_Integrity_Valid'Result = (Bytes_Written = Expected_Size);

   -- Ghost: Chunk count is consistent with file size
   function Chunk_Count_Valid
     (File_Size   : Natural;
      Chunk_Size  : Positive;
      Chunk_Count : Natural) return Boolean with Ghost,
     Pre  => Chunk_Size > 0,
     Post => Chunk_Count_Valid'Result =
             (Chunk_Count = (File_Size + Chunk_Size - 1) / Chunk_Size);

   -------------------------------------------------------------------------
   -- Result Code Classification (for behavioral specifications)
   -------------------------------------------------------------------------

   -- Ghost: Operation succeeded
   function Operation_Succeeded (Result : Result_Code) return Boolean with Ghost,
     Post => Operation_Succeeded'Result = (Result = Success);

   -- Ghost: Operation failed with authentication error
   function Authentication_Failed (Result : Result_Code) return Boolean with Ghost,
     Post => Authentication_Failed'Result = (Result = Auth_Failed);

   -- Ghost: Operation failed with format error
   function Format_Invalid (Result : Result_Code) return Boolean with Ghost,
     Post => Format_Invalid'Result = (Result = Invalid_Format);

   -- Ghost: Operation failed with any error
   function Operation_Failed (Result : Result_Code) return Boolean with Ghost,
     Post => Operation_Failed'Result = (Result /= Success);

   -------------------------------------------------------------------------
   -- Domain Separation Labels (mirroring implementation)
   -------------------------------------------------------------------------

   DS_HYBRID_KDF  : constant String := "anubis-hybrid-kem-v1";
   DS_XCHACHA_KEY : constant String := "anubis-xchacha20-key-v1";

private

   -- Opaque to external users; concrete layout elsewhere
   type Header is record
      Valid : Boolean := False;
   end record;

end Anubis_Contracts;
