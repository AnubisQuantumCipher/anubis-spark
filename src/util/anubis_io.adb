-------------------------------------------------------------------------------
-- ANUBIS-SPARK: I/O Contracts (Implementation)
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Directories;

package body Anubis_IO is

   function Nonempty_Path (Path : String) return Boolean is
   begin
      return Path'Length > 0;
   end Nonempty_Path;

   function Can_Read (Path : String) return Boolean is
      F : File_Type;
   begin
      begin
         Open (F, In_File, Path);
      exception
         when others =>
            return False;
      end;

      Close (F);
      return True;
   end Can_Read;

   function Not_Exists (Path : String) return Boolean is
   begin
      return not Ada.Directories.Exists (Path);
   end Not_Exists;

   procedure Require_Readable (Path : String) is
   begin
      if not Can_Read (Path) then
         raise Constraint_Error with "Path not readable: " & Path;
      end if;
   end Require_Readable;

   procedure Require_Not_Exists (Path : String) is
   begin
      if Ada.Directories.Exists (Path) then
         raise Constraint_Error with "Path already exists: " & Path;
      end if;
   end Require_Not_Exists;

end Anubis_IO;

