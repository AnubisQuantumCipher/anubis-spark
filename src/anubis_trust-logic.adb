-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Trust Record Logic (Implementation)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Strings; use Ada.Strings;

package body Anubis_Trust.Logic is

   function Char_Is_Printable (C : Character) return Boolean is
      Code : constant Natural := Character'Pos (C);
   begin
      return Code >= 16#20# and then Code <= 16#7E#;
   end Char_Is_Printable;

   function Operator_Input_Is_Valid (Source : String) return Boolean is
      Length_OK : constant Boolean := Source'Length <= Operator_Max_Length;
      All_Printable : Boolean := True;
   begin
      if not Length_OK then
         return False;
      end if;

      for C of Source loop
         if not Char_Is_Printable (C) then
            All_Printable := False;
            exit;
         end if;
      end loop;

      return All_Printable;
   end Operator_Input_Is_Valid;

   function Normalize_Operator (Source : String) return String is
      Buffer : String (1 .. Operator_Max_Length) := (others => ' ');
      Length : Natural := 0;
   begin
      -- Keep printable ASCII characters only, respecting maximum length
      for C of Source loop
         exit when Length = Operator_Max_Length;
         if Char_Is_Printable (C) then
            Length := Length + 1;
            Buffer (Length) := C;
         end if;
      end loop;

      if Length = 0 then
         return "";
      end if;

      -- Trim leading and trailing spaces.
      declare
         Start  : Positive := 1;
         Finish : Natural := Length;
      begin
         while Start <= Length and then Buffer (Start) = ' ' loop
            pragma Loop_Invariant (Start in 1 .. Length + 1);
            pragma Loop_Variant (Increases => Start);
            Start := Start + 1;
         end loop;

         if Start > Length then
            return "";
         end if;

         while Finish >= Start and then Buffer (Finish) = ' ' loop
            pragma Loop_Invariant (Finish >= Start - 1 and Finish <= Length);
            pragma Loop_Variant (Decreases => Finish);
            Finish := Finish - 1;
         end loop;

         return Buffer (Start .. Finish);
      end;
   end Normalize_Operator;

   function Label_Input_Is_Valid (Source : String) return Boolean is
      Length_OK : constant Boolean := Source'Length <= SIGNER_LABEL_SIZE;
      All_Printable : Boolean := True;
   begin
      if not Length_OK then
         return False;
      end if;

      for C of Source loop
         if not Char_Is_Printable (C) then
            All_Printable := False;
            exit;
         end if;
      end loop;

      return All_Printable;
   end Label_Input_Is_Valid;

   function Canonical_Label_String (Label : Signer_Label) return String is
      Last_Index : Natural := 0;
   begin
      for I in Label'Range loop
         if Label (I) = 0 then
            exit;
         else
            Last_Index := I;
         end if;
      end loop;

      if Last_Index = 0 then
         return "";
      else
         declare
            Len    : constant Natural := Last_Index;
            Result : String (1 .. Len);
         begin
            for I in 1 .. Len loop
               Result (I) := Character'Val (Integer (Label (Label'First + I - 1)));
            end loop;
            return Result;
         end;
      end if;
   end Canonical_Label_String;

   function Label_Buffer_Is_Valid (Label : Signer_Label) return Boolean is
      Seen_Terminator : Boolean := False;
   begin
      for I in Label'Range loop
         if Seen_Terminator then
            if Label (I) /= 0 then
               return False;
            end if;
         else
            if Label (I) = 0 then
               Seen_Terminator := True;
            elsif not Char_Is_Printable (Character'Val (Integer (Label (I)))) then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end Label_Buffer_Is_Valid;

end Anubis_Trust.Logic;
