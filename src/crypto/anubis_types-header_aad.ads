-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Header AAD Binding
-- Computes authenticated additional data (AAD) from file header
-- This binds every chunk's AEAD operation to the header, preventing tampering
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;

package Anubis_Types.Header_AAD is

   -------------------------------------------------------------------------
   -- AAD Computation
   -------------------------------------------------------------------------

   -- Compute AAD hash from canonical header bytes (excluding signatures)
   -- Output: 32-byte BLAKE2b-256 hash passed as AEAD AAD per chunk
   function Compute_Header_AAD (
      Header_Preamble : Byte_Array
   ) return Byte_Array with
      Pre  => Header_Preamble'Length > 0,
      Post => Compute_Header_AAD'Result'Length = 32;

   -------------------------------------------------------------------------
   -- Ghost Functions for Verification
   -------------------------------------------------------------------------

   -- Ghost: Verify AAD is deterministic (same inputs → same output)
   function AAD_Is_Deterministic (
      Header_A, Header_B : Byte_Array;
      AAD_A, AAD_B       : Byte_Array
   ) return Boolean is
      (if Header_A = Header_B then AAD_A = AAD_B)
   with Ghost;

end Anubis_Types.Header_AAD;
