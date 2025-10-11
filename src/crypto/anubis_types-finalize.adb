-------------------------------------------------------------------------------
-- ANUBIS-SPARK: File Finalization (Implementation)
-------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Directories; use Ada.Directories;
with Interfaces; use Interfaces;

package body Anubis_Types.Finalize is

   -------------------------------------------------------------------------
   -- Partial Name
   -------------------------------------------------------------------------

   function Partial_Name (Final_Path : String) return String is
   begin
      return Final_Path & ".partial";
   end Partial_Name;

   -------------------------------------------------------------------------
   -- Write Final Marker
   -------------------------------------------------------------------------

   function Write_Final_Marker (
      File : in out Stream_IO.File_Type
   ) return Boolean
   is
      S : Stream_IO.Stream_Access := Stream_IO.Stream (File);
   begin
      -- Write magic string
      for C of Final_Magic loop
         Unsigned_8'Write (S, Unsigned_8 (Character'Pos (C)));
      end loop;

      -- Flush to disk
      Stream_IO.Flush (File);

      return True;
   exception
      when others =>
         return False;
   end Write_Final_Marker;

   -------------------------------------------------------------------------
   -- Has Final Marker
   -------------------------------------------------------------------------

   function Has_Final_Marker (Path : String) return Boolean is
      File : Stream_IO.File_Type;
      Size : Ada.Directories.File_Size;
   begin
      -- Check file exists and has sufficient size
      if not Ada.Directories.Exists (Path) then
         return False;
      end if;

      Size := Ada.Directories.Size (Path);
      if Size < Ada.Directories.File_Size (Final_Magic'Length) then
         return False;
      end if;

      -- Read last N bytes and compare
      Stream_IO.Open (File, Stream_IO.In_File, Path);

      declare
         S      : Stream_IO.Stream_Access := Stream_IO.Stream (File);
         Buffer : String (1 .. Final_Magic'Length);
         Offset : constant Ada.Directories.File_Size :=
                    Size - Ada.Directories.File_Size (Final_Magic'Length);
      begin
         -- Seek to end - magic length
         -- Note: Ada.Streams.Stream_IO doesn't have portable Set_Index
         -- So we read entire file and check suffix (simpler, works everywhere)
         declare
            Skip_Bytes : Ada.Directories.File_Size := Offset;
            Dummy_Byte : Unsigned_8;
         begin
            while Skip_Bytes > 0 loop
               Unsigned_8'Read (S, Dummy_Byte);
               Skip_Bytes := Skip_Bytes - 1;
            end loop;
         end;

         -- Read final marker
         for I in Buffer'Range loop
            declare
               B : Unsigned_8;
            begin
               Unsigned_8'Read (S, B);
               Buffer (I) := Character'Val (B);
            end;
         end loop;

         Stream_IO.Close (File);

         return Buffer = Final_Magic;
      exception
         when others =>
            if Stream_IO.Is_Open (File) then
               Stream_IO.Close (File);
            end if;
            return False;
      end;
   exception
      when others =>
         return False;
   end Has_Final_Marker;

   -------------------------------------------------------------------------
   -- Atomic Rename
   -------------------------------------------------------------------------

   function Atomic_Rename (
      Partial_Path : String;
      Final_Path   : String
   ) return Boolean
   is
   begin
      -- Delete final if exists (Ada.Directories.Rename would fail otherwise)
      if Ada.Directories.Exists (Final_Path) then
         Ada.Directories.Delete_File (Final_Path);
      end if;

      -- Atomic rename
      Ada.Directories.Rename (
         Old_Name => Partial_Path,
         New_Name => Final_Path
      );

      return True;
   exception
      when others =>
         return False;
   end Atomic_Rename;

end Anubis_Types.Finalize;
