-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Platinum Proof Contracts
-- Ghost predicates, common subtypes, and proof-level types
-- This file provides the foundation for functional correctness proofs
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
   -- Ghost Predicates (no code generated, proof-only)
   -------------------------------------------------------------------------

   -- Ghost: Check if header is well-formed
   function Well_Formed_Header (H : Header) return Boolean with Ghost;

   -- Ghost: Get canonical size of serialized header
   function Header_Size_Bytes (H : Header) return Natural with Ghost;

   -- Ghost: Check if byte array is well-formed serialized header
   function Well_Formed_Ser (B : Byte_Array) return Boolean with Ghost;

   -------------------------------------------------------------------------
   -- Ghost Model Functions (bidirectional proof models)
   -------------------------------------------------------------------------

   -- Ghost: Serialize header to bytes (proof model)
   function Serialize_Model (H : Header) return Byte_Array with Ghost;

   -- Ghost: Parse bytes to header (proof model)
   function Parse_Model (B : Byte_Array) return Header with Ghost;

   -------------------------------------------------------------------------
   -- AAD Binding Model
   -------------------------------------------------------------------------

   -- Ghost: Each chunk authenticates the header via AAD
   function Header_Binds
     (H : Header; Chunk : Byte_Array; Tag : Tag_16) return Boolean with Ghost;

   -------------------------------------------------------------------------
   -- Nonce Freshness Model
   -------------------------------------------------------------------------

   -- Ghost: Nonce uniqueness predicate (file_nonce || chunk_idx)
   function Nonce_Fresh (N : Nonce_24) return Boolean with Ghost;

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
