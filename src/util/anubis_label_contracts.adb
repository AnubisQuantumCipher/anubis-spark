-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Label Contracts Harness (Implementation)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types;              use Anubis_Types;
with Anubis_Types.Storage;      use Anubis_Types.Storage;
with Anubis_Trust.Logic;

package body Anubis_Label_Contracts is

   function Roundtrip_Source (Source : String) return Boolean is
      L : Signer_Label := Make_Label (Source);
   begin
      return Label_To_String (L) = Source;
   end Roundtrip_Source;

   function Roundtrip_Label (L : Signer_Label) return Boolean is
   begin
      return Make_Label (Label_To_String (L)) = L;
   end Roundtrip_Label;

end Anubis_Label_Contracts;

