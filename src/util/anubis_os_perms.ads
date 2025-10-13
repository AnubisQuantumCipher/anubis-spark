-------------------------------------------------------------------------------
-- ANUBIS-SPARK: OS Permissions Helper (POSIX macOS/Linux)
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Interfaces.C; use Interfaces.C;

package Anubis_OS_Perms is

   -- Return True if file mode is 0600 (rw-------). Mode_Out receives numeric mode (octal).
   function Mode_600 (Path : String; Mode_Out : out Natural) return Boolean;

end Anubis_OS_Perms;

