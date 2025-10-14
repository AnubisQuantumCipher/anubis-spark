-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Cross-Platform OS Permissions Helper
-- Supports: POSIX (macOS/Linux). Windows path is a placeholder that
-- returns a conservative success (no ACL inspection). Permission
-- checks are effectively POSIX-only in this release.
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Interfaces.C; use Interfaces.C;

package Anubis_OS_Perms is

   -- Return True if file has restrictive permissions (owner-only read/write).
   -- On POSIX: checks for mode 0600 (rw-------)
   -- On Windows: checks that file is not world-readable and owner has access
   -- Mode_Out receives numeric mode (octal on POSIX, access bits on Windows).
   function Mode_600 (Path : String; Mode_Out : out Natural) return Boolean;

   -- Format numeric mode as human-readable string.
   -- On POSIX: formats as octal (e.g., "600")
   -- On Windows: formats as access description
   function Format_Octal_Mode (Mode : Natural) return String;

   -- Returns True if running on Windows platform
   function Is_Windows return Boolean;

end Anubis_OS_Perms;
