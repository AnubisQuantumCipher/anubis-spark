-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Key Lifecycle Manager Implementation
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Key_Manager is

   procedure Initialize (Key : out Managed_Key) is
   begin
      Key.Key_Material := (others => 0);
      Key.Length := 0;
      Key.Purpose := Encryption;
      Key.Status := Uninitialized;
      Key.Usage_Count := 0;
      Key.Policy := Default_Rotation;
      Key.Valid := False;
   end Initialize;

   procedure Create_Managed_Key (
      Key_Data   : in     Byte_Array;
      Purpose    : in     Key_Purpose;
      Policy     : in     Rotation_Policy;
      Managed    : out    Managed_Key;
      Success    : out    Boolean
   ) is
   begin
      Initialize (Managed);

      if Key_Data'Length > Managed.Key_Material'Length then
         Success := False;
         return;
      end if;

      -- Copy key material
      for I in Key_Data'Range loop
         pragma Loop_Invariant
           (for all J in Key_Data'First .. I - 1 =>
              Managed.Key_Material (J - Key_Data'First + 1) = Key_Data (J));
         pragma Loop_Variant (Increases => I);
         declare
            Dest_Index : constant Positive := I - Key_Data'First + 1;
         begin
            pragma Assert (Dest_Index in Managed.Key_Material'Range);
            Managed.Key_Material (Dest_Index) := Key_Data (I);
         end;
      end loop;

      Managed.Length := Key_Data'Length;
      Managed.Purpose := Purpose;
      Managed.Policy := Policy;
      Managed.Status := Active;
      Managed.Usage_Count := 0;
      Managed.Valid := True;

      Success := True;
   end Create_Managed_Key;

   function Needs_Rotation (Key : Managed_Key) return Boolean is
   begin
      if not Key.Policy.Enabled then
         return False;
      end if;

      if Key.Policy.Usage_Based_Count > 0 and then
         Key.Usage_Count >= Key.Policy.Usage_Based_Count
      then
         return True;
      end if;

      return False;
   end Needs_Rotation;

   function Get_Key_Status (Key : Managed_Key) return Key_Status is
      (Key.Status);

   function Get_Usage_Count (Key : Managed_Key) return Natural is
      (Key.Usage_Count);

   procedure Record_Usage (Key : in out Managed_Key) is
   begin
      if Key.Status = Active and Key.Usage_Count < Natural'Last then
         Key.Usage_Count := Key.Usage_Count + 1;
      end if;
   end Record_Usage;

   procedure Expire_Key (Key : in out Managed_Key) is
   begin
      Key.Status := Expired;
   end Expire_Key;

   procedure Destroy_Key (Key : in out Managed_Key) is
   begin
      -- Zeroize key material
      for I in Key.Key_Material'Range loop
         pragma Loop_Invariant (for all J in Key.Key_Material'First .. I - 1 =>
                                   Key.Key_Material (J) = 0);
         pragma Loop_Variant (Increases => I);
         Key.Key_Material (I) := 0;
      end loop;

      Key.Length := 0;
      Key.Usage_Count := 0;
      Key.Status := Destroyed;
      Key.Valid := False;
   end Destroy_Key;

end Anubis_Key_Manager;
