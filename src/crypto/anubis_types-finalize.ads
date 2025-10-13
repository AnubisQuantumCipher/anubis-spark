-------------------------------------------------------------------------------
-- ANUBIS-SPARK: File Finalization
-- Provides .partial workflow and atomic rename for crash-safe encryption
-------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Streams.Stream_IO;

package Anubis_Types.Finalize is
   pragma SPARK_Mode (Off);  -- Uses Ada.Streams, not SPARK-compatible

   -------------------------------------------------------------------------
   -- Finalization Marker
   -------------------------------------------------------------------------

   -- Final marker written at end of complete files
   Final_Magic : constant String := "ANUB3:FINAL";

   -------------------------------------------------------------------------
   -- Operations
   -------------------------------------------------------------------------

   -- Get partial filename for a given final path
   -- Example: "file.pdf.anubis" → "file.pdf.anubis.partial"
   function Partial_Name (Final_Path : String) return String;

   -- Write finalization marker to end of file
   -- Returns True on success, False on error
   function Write_Final_Marker (
      File : in out Ada.Streams.Stream_IO.File_Type
   ) return Boolean;

   -- Check if file has valid finalization marker
   -- Returns True if file is complete, False if incomplete or error
   function Has_Final_Marker (Path : String) return Boolean;

   -- Atomic rename (partial → final)
   -- Renames partial file to final path
   -- Returns True on success, False on error
   function Atomic_Rename (
      Partial_Path : String;
      Final_Path   : String
   ) return Boolean;

end Anubis_Types.Finalize;
