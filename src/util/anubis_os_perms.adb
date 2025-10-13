-------------------------------------------------------------------------------
-- ANUBIS-SPARK: OS Permissions Helper (POSIX macOS/Linux)
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Anubis_OS_Perms is

   function C_Stat_Mode (Path : chars_ptr) return int;
   pragma Import (C, C_Stat_Mode, "anubis_stat_mode");

   function Mode_600 (Path : String; Mode_Out : out Natural) return Boolean is
      C_Path : chars_ptr := New_String (Path);
      M      : int := -1;
   begin
      begin
         M := C_Stat_Mode (C_Path);
      exception
         when others =>
            M := -1;
      end;
      Free (C_Path);
      if M < 0 then
         Mode_Out := 0;
         return False;
      end if;
      Mode_Out := Natural (M);
      return M = 8#600#;  -- octal 0600
   end Mode_600;

   function Format_Octal_Mode (Mode : Natural) return String is
      -- Convert a Natural (e.g., 420) to octal without leading 0 prefix (e.g., "644").
      Tmp  : Natural := Mode;
      Buf  : String (1 .. 12);
      I    : Natural := Buf'Last;
   begin
      if Tmp = 0 then
         return "0";
      end if;
      while Tmp > 0 loop
         declare
            D : Natural := Tmp mod 8;
         begin
            Buf (I) := Character'Val (Integer (Character'Pos ('0')) + Integer (D));
         end;
         if I = Buf'First then
            exit;
         end if;
         I := I - 1;
         Tmp := Tmp / 8;
      end loop;
      return Buf (I .. Buf'Last);
   end Format_Octal_Mode;

end Anubis_OS_Perms;
