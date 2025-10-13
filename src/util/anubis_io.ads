-------------------------------------------------------------------------------
-- ANUBIS-SPARK: I/O Contracts (Specification)
-- SPARK-friendly wrappers that capture intended file I/O semantics
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Directories;

package Anubis_IO is

   -- Check whether a path is non-empty (basic guard for CLI args)
   function Nonempty_Path (Path : String) return Boolean with
     Global  => null,
     Depends => (Nonempty_Path'Result => Path),
     Post    => (if Nonempty_Path'Result then Path'Length > 0);

   -- Probe if a file can be opened for reading without raising.
   function Can_Read (Path : String) return Boolean with
     Global  => null,
     Depends => (Can_Read'Result => Path);

   -- True if the file does not exist (useful for exclusive create intent)
   function Not_Exists (Path : String) return Boolean with
     Global  => null,
     Depends => (Not_Exists'Result => Path),
     Post    => (if Not_Exists'Result then not Ada.Directories.Exists (Path));

   -- Preconditions expressed as runtime checks for CLI flows
   procedure Require_Readable (Path : String) with
     Pre  => Path'Length > 0,
     Post => Can_Read (Path);

   procedure Require_Not_Exists (Path : String) with
     Pre  => Path'Length > 0,
     Post => Not_Exists (Path);

end Anubis_IO;

