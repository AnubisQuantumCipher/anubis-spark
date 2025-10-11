-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Header Serialize/Parse Contracts (Enhanced)
-- Proves: Parse(Serialize(H)) = H (round-trip identity)
-- Proves: Serialize(Parse(B)) = B (bijection)
-- Proves: Serialization is deterministic and canonical
-- Proves: All header fields preserved through round-trip
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Contracts; use Anubis_Contracts;

package Anubis_Header_IO is

   -------------------------------------------------------------------------
   -- Serialize: Header → Bytes (Enhanced)
   -------------------------------------------------------------------------

   -- Serialize a well-formed header to canonical bytes
   -- Proves: Output is well-formed serialized header
   -- Proves: Length is deterministic (function of header)
   -- Proves: Round-trip identity holds
   function Serialize (H : Header) return Byte_Array with
     Global  => null,
     Pre     =>
       Well_Formed_Header (H) and then
       Has_Supported_Version (H),
     Post    =>
       -- Output is well-formed serialized header
       Well_Formed_Ser (Serialize'Result) and then

       -- Length is correct and deterministic
       Serialize'Result'Length = Header_Size_Bytes (H) and then
       Serialize'Result'Length > 0 and then
       Serialize'Result'Length <= 4096 and then

       -- Magic bytes are present
       Has_Valid_Magic (Serialize'Result) and then

       -- Round-trip identity: Parse(Serialize(H)) = H
       Headers_Equal (Parse (Serialize'Result), H) and then

       -- Matches the abstract model
       Serialize'Result = Serialize_Model (H),

     Contract_Cases =>
       -- Case 1: Valid header → successful serialization
       (Well_Formed_Header (H) and Has_Supported_Version (H) =>
          Well_Formed_Ser (Serialize'Result) and
          Serialize'Result'Length = Header_Size_Bytes (H));

   -------------------------------------------------------------------------
   -- Parse: Bytes → Header (Enhanced)
   -------------------------------------------------------------------------

   -- Parse bytes into a header
   -- Proves: Output is well-formed header
   -- Proves: Bijection property holds
   -- Proves: Version is supported
   function Parse (B : Byte_Array) return Header with
     Global  => null,
     Pre     =>
       Well_Formed_Ser (B) and then
       B'Length > 0 and then
       B'Length <= 4096 and then
       Has_Valid_Magic (B),
     Post    =>
       -- Output is well-formed header
       Well_Formed_Header (Parse'Result) and then

       -- Version is supported
       Has_Supported_Version (Parse'Result) and then

       -- Bijection: Serialize(Parse(B)) = B
       Serialize (Parse'Result) = B and then

       -- Matches the abstract model
       Headers_Equal (Parse'Result, Parse_Model (B)),

     Contract_Cases =>
       -- Case 1: Valid serialized header → successful parse
       (Well_Formed_Ser (B) and Has_Valid_Magic (B) =>
          Well_Formed_Header (Parse'Result) and
          Has_Supported_Version (Parse'Result));

   -------------------------------------------------------------------------
   -- Lemma: Serialization is Deterministic (Ghost)
   -------------------------------------------------------------------------

   -- Proves that serializing the same header twice produces identical output
   -- This is important for authentication tag verification
   procedure Lemma_Serialize_Deterministic
     (H : Header) with
     Ghost,
     Global => null,
     Pre    => Well_Formed_Header (H) and Has_Supported_Version (H),
     Post   =>
       (declare
          B1 : constant Byte_Array := Serialize (H);
          B2 : constant Byte_Array := Serialize (H);
        begin
          B1 = B2);

   -------------------------------------------------------------------------
   -- Lemma: Round-Trip Preserves All Fields (Ghost)
   -------------------------------------------------------------------------

   -- Proves that Parse(Serialize(H)) preserves all header fields exactly
   procedure Lemma_RoundTrip_Preserves_Fields
     (H : Header) with
     Ghost,
     Global => null,
     Pre    => Well_Formed_Header (H) and Has_Supported_Version (H),
     Post   =>
       (declare
          B : constant Byte_Array := Serialize (H);
          H2 : constant Header := Parse (B);
        begin
          Headers_Equal (H, H2) and
          Well_Formed_Header (H2) and
          Has_Supported_Version (H2));

   -------------------------------------------------------------------------
   -- Lemma: Bijection Properties (Ghost)
   -------------------------------------------------------------------------

   -- Proves both directions of the bijection
   procedure Lemma_Bijection
     (H : Header;
      B : Byte_Array) with
     Ghost,
     Global => null,
     Pre    =>
       Well_Formed_Header (H) and then
       Has_Supported_Version (H) and then
       Well_Formed_Ser (B) and then
       Has_Valid_Magic (B),
     Post   =>
       -- Forward direction: Parse(Serialize(H)) = H
       Headers_Equal (Parse (Serialize (H)), H) and then
       -- Backward direction: Serialize(Parse(B)) = B
       Serialize (Parse (B)) = B;

end Anubis_Header_IO;
