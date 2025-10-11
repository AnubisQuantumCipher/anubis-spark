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

   -- Compute AAD hash from canonical header fields
   -- Header binding: Magic || Version || File_Nonce16 || Chunk_Size || Total_Size
   -- Output: 32-byte BLAKE2b-256 hash
   -- This hash is passed as AAD to every chunk's AEAD operation
   function Compute_Header_AAD (
      File_Nonce16   : Byte_Array;  -- length = 16
      Chunk_Size     : Natural;     -- in bytes
      Total_Size     : Natural      -- total file size in bytes
   ) return Byte_Array with
      Pre  => File_Nonce16'Length = 16,
      Post => Compute_Header_AAD'Result'Length = 32;

   -------------------------------------------------------------------------
   -- Ghost Functions for Verification
   -------------------------------------------------------------------------

   -- Ghost: Verify AAD is deterministic (same inputs → same output)
   function AAD_Is_Deterministic (
      Nonce_A, Nonce_B   : Byte_Array;
      Chunk_A, Chunk_B   : Natural;
      Total_A, Total_B   : Natural;
      AAD_A, AAD_B       : Byte_Array
   ) return Boolean is
      (if Nonce_A = Nonce_B and Chunk_A = Chunk_B and Total_A = Total_B
       then AAD_A = AAD_B)
   with Ghost;

end Anubis_Types.Header_AAD;
