-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Header Serialize/Parse Contracts
-- Proves: Parse(Serialize(H)) = H (round-trip identity)
-- Proves: Serialize(Parse(B)) = B (bijection)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Contracts; use Anubis_Contracts;

package Anubis_Header_IO is

   -------------------------------------------------------------------------
   -- Serialize: Header → Bytes
   -------------------------------------------------------------------------

   -- Serialize a well-formed header to canonical bytes
   function Serialize (H : Header) return Byte_Array with
     Global  => null,
     Pre     => Well_Formed_Header (H),
     Post    =>
       Well_Formed_Ser (Serialize'Result) and
       Serialize'Result'Length = Header_Size_Bytes (H) and
       -- Round-trip identity: Parse(Serialize(H)) = H
       Parse (Serialize'Result) = H;

   -------------------------------------------------------------------------
   -- Parse: Bytes → Header
   -------------------------------------------------------------------------

   -- Parse bytes into a header
   function Parse (B : Byte_Array) return Header with
     Global  => null,
     Pre     => Well_Formed_Ser (B),
     Post    => Well_Formed_Header (Parse'Result) and
                -- Bijection: Serialize(Parse(B)) = B
                Serialize (Parse'Result) = B;

end Anubis_Header_IO;
