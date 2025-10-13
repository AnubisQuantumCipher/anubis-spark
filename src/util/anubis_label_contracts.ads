-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Label Contracts Harness (Specification)
-- Demonstrates bijectivity-style round-trips for Make_Label/Label_To_String
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);
pragma Pure;

with Anubis_Types;              use Anubis_Types;
with Anubis_Types.Storage;      use Anubis_Types.Storage;
with Anubis_Trust.Logic;

package Anubis_Label_Contracts is

   -- For valid inputs, Label_To_String(Make_Label(Source)) = Source
   function Roundtrip_Source (Source : String) return Boolean with
     Pre  => Is_Valid_Label_Input (Source),
     Post => Roundtrip_Source'Result and then
             Label_To_String (Make_Label (Source)) = Source;

   -- For valid label buffers, Make_Label(Label_To_String(L)) = L
   function Roundtrip_Label (L : Signer_Label) return Boolean with
     Pre  => Anubis_Trust.Logic.Label_Buffer_Is_Valid (L),
     Post => Roundtrip_Label'Result and then
             Make_Label (Label_To_String (L)) = L;

end Anubis_Label_Contracts;

