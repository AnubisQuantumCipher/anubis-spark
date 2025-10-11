-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Zeroization Contracts
-- Proves: All key bytes are zero after zeroization
-- Proves: Validity flags are cleared
-- Covers: All secret key types
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Contracts; use Anubis_Contracts;

package Anubis_Zeroize is

   -------------------------------------------------------------------------
   -- Zeroize Key_32 (generic 32-byte key)
   -------------------------------------------------------------------------

   -- Proves: All bytes zeroed AND validity flag cleared
   procedure Zeroize_Key (Key : in out Key_32; Valid_Flag : in out Boolean) with
     Global  => null,
     Depends => (Key => Key, Valid_Flag => Valid_Flag),
     Post    =>
       -- All bytes must be zero
       (for all I in Key'Range => Key (I) = 0) and then
       -- Validity flag must be false
       (Valid_Flag = False);

end Anubis_Zeroize;
