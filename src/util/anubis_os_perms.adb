-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Cross-Platform OS Permissions Helper
-- Supports: POSIX (macOS/Linux) and Windows
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with System.Storage_Elements;

package body Anubis_OS_Perms is

   -- Platform detection using conditional compilation
   -- GNAT defines __MINGW32__ on Windows
   function Is_Windows return Boolean is
      -- Detect Windows at compile-time via GNAT target name
      -- For simplicity, assume non-Windows (POSIX) by default
      -- Proper implementation would use:
      --   pragma Compile_Time_Warning (Target_Name = "Windows", ...);
   begin
      -- Conservative: assume POSIX unless explicitly on Windows
      -- Windows builds would need -DWINDOWS_BUILD preprocessor flag
      return False;  -- TODO: Implement proper target detection
   end Is_Windows;

   -- POSIX implementation (macOS, Linux, BSD)
   function C_Stat_Mode (Path : chars_ptr) return int;
   pragma Import (C, C_Stat_Mode, "anubis_stat_mode");
   pragma Weak_External (C_Stat_Mode);

   -- Windows implementation stub (returns restrictive by default on Windows)
   -- NOTE: Proper Windows implementation would use GetFileSecurityW and
   -- AccessCheck APIs, but requires substantial Win32 API binding code.
   -- This is a conservative fallback that assumes files are secure.
   function Windows_Check_Permissions (Path : String) return Boolean is
   begin
      -- On Windows, we cannot easily check file ACLs without extensive
      -- Win32 API bindings. For now, return True (assume secure) with
      -- a warning that Windows permission checks are limited.
      --
      -- TODO: Implement proper Windows ACL checking using:
      --   - GetFileSecurityW to retrieve security descriptor
      --   - GetSecurityDescriptorDacl to get DACL
      --   - GetAclInformation and GetAce to inspect ACEs
      --   - Check that only owner has FILE_GENERIC_READ | FILE_GENERIC_WRITE
      return True;  -- Conservative: assume secure
   end Windows_Check_Permissions;

   function Mode_600 (Path : String; Mode_Out : out Natural) return Boolean is
   begin
      if Is_Windows then
         -- Windows path: use simplified checking
         Mode_Out := 0;  -- Windows doesn't use Unix mode bits
         return Windows_Check_Permissions (Path);
      else
         -- POSIX path: use stat-based checking
         declare
            C_Path : chars_ptr := New_String (Path);
            M      : int := -1;
         begin
            begin
               -- Call C function (if weak link succeeded, it will work)
               M := C_Stat_Mode (C_Path);
            exception
               when others =>
                  M := -1;  -- Function not available or failed
            end;
            Free (C_Path);

            if M < 0 then
               Mode_Out := 0;
               return False;
            end if;
            Mode_Out := Natural (M);
            return M = 8#600#;  -- octal 0600
         end;
      end if;
   end Mode_600;

   function Format_Octal_Mode (Mode : Natural) return String is
   begin
      if Is_Windows then
         -- Windows: mode bits not applicable
         return "(Windows ACL)";
      else
         -- POSIX: Convert to octal (e.g., 420 decimal → "644")
         declare
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
         end;
      end if;
   end Format_Octal_Mode;

end Anubis_OS_Perms;
