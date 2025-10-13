-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Signer Trust Store Implementation
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Anubis_Types.Storage;
with Anubis_Trust.Logic;
with Sodium_Hash;
with Sodium_Common;
with Interfaces.C;
with System;

package body Anubis_Trust is

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Ada.Calendar;
   use Anubis_Trust.Logic;

   Trust_Base_Name  : constant String := ".anubis";
   Trust_Subdir_Name : constant String := "trust";

   function Home_Directory return String is
   begin
      if Ada.Environment_Variables.Exists ("HOME") then
         return Ada.Environment_Variables.Value ("HOME");
      else
         return Ada.Directories.Current_Directory;
      end if;
   end Home_Directory;

   function Trust_Directory return String is
      Base : constant String := Ada.Directories.Compose (Home_Directory, Trust_Base_Name);
   begin
      return Ada.Directories.Compose (Base, Trust_Subdir_Name);
   end Trust_Directory;

   function Record_Path (Fingerprint : Signer_Fingerprint) return String is
   begin
      return Ada.Directories.Compose (Trust_Directory, Hex_Fingerprint (Fingerprint) & ".trust");
   end Record_Path;

   procedure Ensure_Directory is
      Dir : constant String := Trust_Directory;
   begin
      if not Ada.Directories.Exists (Dir) then
         Ada.Directories.Create_Path (Dir);
      end if;
   end Ensure_Directory;

   function Normalize_Label (Label : Signer_Label) return String is
      L : constant String := Canonical_Label_String (Label);
   begin
      if L'Length = 0 then
         return "(unnamed)";
      else
         return L;
      end if;
   end Normalize_Label;

   function Current_Unix_Timestamp return Unsigned_64 is
      Epoch : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (1970, 1, 1, 0.0);
      Now   : Ada.Calendar.Time;
      Diff  : Duration;
   begin
      begin
         Now := Ada.Calendar.Clock;
      exception
         when others =>
            return 0;
      end;

      Diff := Now - Epoch;

      if Diff <= 0.0 then
         return 0;
      end if;

      begin
         return Unsigned_64 (Long_Long_Integer (Diff));
      exception
         when others =>
            return 0;
      end;
   end Current_Unix_Timestamp;

   function Format_Timestamp (Value : Unsigned_64) return String is
   begin
      if Value = 0 then
         return "0";
      end if;

      declare
         Epoch : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (1970, 1, 1, 0.0);
         Seconds_Float : Long_Long_Float;
         T : Ada.Calendar.Time;
      begin
         begin
            Seconds_Float := Long_Long_Float (Value);
         exception
            when others =>
               return Trim (Unsigned_64'Image (Value), Both);
         end;

         begin
            T := Epoch + Duration (Seconds_Float);
         exception
            when others =>
               return Trim (Unsigned_64'Image (Value), Both);
         end;

         begin
            return Trim (Ada.Calendar.Formatting.Image (T), Both);
         exception
            when others =>
               return Trim (Unsigned_64'Image (Value), Both);
         end;
      end;
   end Format_Timestamp;

   function Hex_Fingerprint (Fingerprint : Signer_Fingerprint) return String is
      Hex_Digits : constant array (Integer range 0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
      Result : String (1 .. SIGNER_FINGERPRINT_SIZE * 2);
      Index  : Natural := 1;
   begin
      for B of Fingerprint loop
         declare
            Value : constant Integer := Integer (B);
         begin
            Result (Index)     := Hex_Digits ((Value / 16) mod 16);
            Result (Index + 1) := Hex_Digits (Value mod 16);
            Index := Index + 2;
         end;
      end loop;
      return Result;
   end Hex_Fingerprint;

   function Hex_Value (C : Character) return Integer is
      Lower : constant Character := To_Lower (C);
   begin
      if Lower in '0' .. '9' then
         return Character'Pos (Lower) - Character'Pos ('0');
      elsif Lower in 'a' .. 'f' then
         return Character'Pos (Lower) - Character'Pos ('a') + 10;
      else
         return -1;
      end if;
   end Hex_Value;

   function Parse_Fingerprint (Hex : String; Fingerprint : out Signer_Fingerprint) return Boolean is
      Clean : constant String := Trim (Hex, Both);
      Len   : constant Natural := Clean'Length;
   begin
      if Len /= SIGNER_FINGERPRINT_SIZE * 2 then
         Fingerprint := (others => 0);
         return False;
      end if;

      for I in 0 .. SIGNER_FINGERPRINT_SIZE - 1 loop
         declare
            High : constant Integer := Hex_Value (Clean (Clean'First + I * 2));
            Low  : constant Integer := Hex_Value (Clean (Clean'First + I * 2 + 1));
         begin
            if High < 0 or else Low < 0 then
               Fingerprint := (others => 0);
               return False;
            end if;
            Fingerprint (Fingerprint'First + I) :=
              Byte (High * 16 + Low);
         end;
      end loop;

      return True;
   end Parse_Fingerprint;

   -------------------------------------------------------------------------
   -- HMAC Protection for Trust Records
   -------------------------------------------------------------------------

   -- Path for private HMAC key file under trust directory
   function HMAC_Key_Path return String is
   begin
      Ensure_Directory;
      return Ada.Directories.Compose (Trust_Directory, ".hmac.key");
   end HMAC_Key_Path;

   -- Load existing 32-byte HMAC key, or create a new random one
   function Load_Or_Create_HMAC_Key return Byte_Array is
      Key_Path : constant String := HMAC_Key_Path;
      Key      : Byte_Array (1 .. 32);
   begin
      if Ada.Directories.Exists (Key_Path) then
         declare
            F   : Ada.Streams.Stream_IO.File_Type;
            Str : Ada.Streams.Stream_IO.Stream_Access;
            B   : Ada.Streams.Stream_Element;
         begin
            begin
               Ada.Streams.Stream_IO.Open (F, Ada.Streams.Stream_IO.In_File, Key_Path);
            exception
               when others =>
                  goto regenerate;
            end;

            Str := Ada.Streams.Stream_IO.Stream (F);
            begin
               for I in Key'Range loop
                  Ada.Streams.Stream_Element'Read (Str, B);
                  Key (I) := Byte (B);
               end loop;
            exception
               when others =>
                  Ada.Streams.Stream_IO.Close (F);
                  goto regenerate;
            end;
            Ada.Streams.Stream_IO.Close (F);
            return Key;
         end;
      end if;

      <<regenerate>>
      declare
         F   : Ada.Streams.Stream_IO.File_Type;
         Str : Ada.Streams.Stream_IO.Stream_Access;
      begin
         Sodium_Common.randombytes_buf (
           buf  => Key (Key'First)'Address,
           size => Interfaces.C.size_t (Key'Length)
         );

         Ada.Streams.Stream_IO.Create (F, Ada.Streams.Stream_IO.Out_File, Key_Path);
         Str := Ada.Streams.Stream_IO.Stream (F);
         for I in Key'Range loop
            Ada.Streams.Stream_Element'Write (Str, Ada.Streams.Stream_Element (Key (I)));
         end loop;
         Ada.Streams.Stream_IO.Close (F);
      exception
         when others =>
            declare
               Zero : Byte_Array (1 .. 32) := (others => 0);
            begin
               return Zero;
            end;
      end;

      return Key;
   end Load_Or_Create_HMAC_Key;

   -- Force rotation of the HMAC key file (overwrite with new random key)
   procedure Rotate_HMAC_Key (Success : out Boolean) is
      Key_Path : constant String := HMAC_Key_Path;
      Key      : Byte_Array (1 .. 32);
      F        : Ada.Streams.Stream_IO.File_Type;
      Str      : Ada.Streams.Stream_IO.Stream_Access;
   begin
      Sodium_Common.randombytes_buf (
        buf  => Key (Key'First)'Address,
        size => Interfaces.C.size_t (Key'Length)
      );

      Ada.Streams.Stream_IO.Create (F, Ada.Streams.Stream_IO.Out_File, Key_Path);
      Str := Ada.Streams.Stream_IO.Stream (F);
      for I in Key'Range loop
         Ada.Streams.Stream_Element'Write (Str, Ada.Streams.Stream_Element (Key (I)));
      end loop;
      Ada.Streams.Stream_IO.Close (F);
      Success := True;
   exception
      when others =>
         Success := False;
   end Rotate_HMAC_Key;

   -- Compute HMAC-BLAKE2b for trust record
   function Compute_HMAC (
      Status    : String;
      Label_Str : String;
      Timestamp : Unsigned_64;
      Updated   : Unsigned_64;
      Operator  : String) return Byte_Array
   is
      HMAC_Key : constant Byte_Array := Load_Or_Create_HMAC_Key;

      -- Create canonical message: "status|label|timestamp|updated|operator"
      Message : constant String :=
        Status & "|" &
        Label_Str & "|" &
        Trim (Unsigned_64'Image (Timestamp), Both) & "|" &
        Trim (Unsigned_64'Image (Updated), Both) & "|" &
        Operator;
      Message_Bytes : Byte_Array (1 .. Message'Length);
      HMAC : Byte_Array (1 .. 32);  -- BLAKE2b-256 output
   begin
      -- Convert message to bytes
      for I in Message'Range loop
         Message_Bytes (I) := Byte (Character'Pos (Message (I)));
      end loop;

      -- Compute keyed BLAKE2b (equivalent to HMAC)
      declare
         Result : Interfaces.C.int;
         use Interfaces.C;
      begin
         Result := Sodium_Hash.crypto_generichash (
            Output     => HMAC'Address,
            Output_Len => size_t (HMAC'Length),
            Input      => Message_Bytes'Address,
            Input_Len  => unsigned_long_long (Message_Bytes'Length),
            Key        => HMAC_Key'Address,
            Key_Len    => size_t (HMAC_Key'Length)
         );

         if Result /= 0 then
            -- Return zero on failure
            return (1 .. 32 => 0);
         end if;
      end;

      return HMAC;
   end Compute_HMAC;

   type Raw_Status is (Raw_Pending, Raw_Approved, Raw_Denied);

   function To_String (Status : Raw_Status) return String is
   begin
      case Status is
         when Raw_Pending  => return "pending";
         when Raw_Approved => return "approved";
         when Raw_Denied   => return "denied";
      end case;
   end To_String;

   function To_Trust_Status (Status : Raw_Status) return Trust_Status is
   begin
      case Status is
         when Raw_Pending  => return Pending;
         when Raw_Approved => return Approved;
         when Raw_Denied   => return Denied;
      end case;
   end To_Trust_Status;

   function Parse_Status (Value : String) return Raw_Status is
      Clean : constant String := To_Lower (Trim (Value, Both));
   begin
      if Clean = "approved" then
         return Raw_Approved;
      elsif Clean = "denied" then
         return Raw_Denied;
      else
         return Raw_Pending;
      end if;
   end Parse_Status;

   procedure Write_Record (
      Path      : String;
      Status    : Raw_Status;
      Label_Str : String;
      Timestamp : Unsigned_64;
      Updated   : Unsigned_64;
      Operator  : String
   ) is
      File : Ada.Text_IO.File_Type;
      Operator_Safe : constant String :=
        (if Operator_Input_Is_Valid (Operator)
         then Normalize_Operator (Operator)
         else "");

      -- Compute HMAC for integrity protection
      HMAC_Data : constant Byte_Array := Compute_HMAC (
         Status    => To_String (Status),
         Label_Str => Label_Str,
         Timestamp => Timestamp,
         Updated   => Updated,
         Operator  => Operator_Safe
      );
      HMAC_Hex  : constant String := Hex_Fingerprint (HMAC_Data);
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
      Ada.Text_IO.Put_Line (File, "status: " & To_String (Status));
      Ada.Text_IO.Put_Line (File, "label: " & Label_Str);
      Ada.Text_IO.Put_Line (File, "timestamp: " & Trim (Unsigned_64'Image (Timestamp), Both));
      Ada.Text_IO.Put_Line (File, "updated_at: " & Trim (Unsigned_64'Image (Updated), Both));
      Ada.Text_IO.Put_Line (File, "operator: " & Operator_Safe);
      Ada.Text_IO.Put_Line (File, "hmac: " & HMAC_Hex);  -- ✅ HMAC protection
      Ada.Text_IO.Close (File);
   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
         raise;
   end Write_Record;

   procedure Read_Record (
      Path       : String;
      Status     : out Raw_Status;
      Label_Str  : out Unbounded_String;
      Timestamp  : out Unsigned_64;
      Updated    : out Unsigned_64;
      Operator   : out Unbounded_String;
      Success    : out Boolean
   ) is
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 1024);
      Last : Natural;
      Has_Status    : Boolean := False;
      Has_Label     : Boolean := False;
      Has_Timestamp : Boolean := False;
      Has_Updated   : Boolean := False;
      Has_Operator  : Boolean := False;
      Has_HMAC      : Boolean := False;  -- ✅ HMAC field
      Label_Buffer  : Unbounded_String := To_Unbounded_String ("");
      Raw_Timestamp : Unsigned_64 := 0;
      Raw_Updated   : Unsigned_64 := 0;
      Operator_Buffer : Unbounded_String := To_Unbounded_String ("");
      HMAC_Stored   : Unbounded_String := To_Unbounded_String ("");  -- ✅ Stored HMAC
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);
      while not Ada.Text_IO.End_Of_File (File) loop
         Ada.Text_IO.Get_Line (File, Line, Last);
         declare
            Line_Text : constant String := Line (1 .. Last);
            Pos   : constant Natural := Index (Line_Text, ":");
         begin
            if Pos = 0 then
               null;
            else
               declare
                  Key   : constant String := To_Lower (Trim (Line_Text (Line_Text'First .. Line_Text'First + Pos - 2), Both));
                  Value : constant String := Trim (Line_Text (Line_Text'First + Pos .. Line_Text'Last), Both);
               begin
                  if Key = "status" then
                     Status := Parse_Status (Value);
                     Has_Status := True;
                  elsif Key = "label" then
                     Label_Buffer := To_Unbounded_String (Value);
                     Has_Label := True;
                  elsif Key = "timestamp" then
                     begin
                        Raw_Timestamp := Unsigned_64'Value (Value);
                        Has_Timestamp := True;
                     exception
                        when others =>
                           null;
                     end;
                  elsif Key = "updated_at" then
                     begin
                        Raw_Updated := Unsigned_64'Value (Value);
                        Has_Updated := True;
                     exception
                        when others =>
                           null;
                     end;
                  elsif Key = "operator" then
                     Operator_Buffer := To_Unbounded_String (Value);
                     Has_Operator := True;
                  elsif Key = "hmac" then  -- ✅ Parse HMAC field
                     HMAC_Stored := To_Unbounded_String (Value);
                     Has_HMAC := True;
                  end if;
               end;
            end if;
         end;
      end loop;
      Ada.Text_IO.Close (File);

      if not Has_Status then
         Status := Raw_Pending;
      end if;
      if not Has_Label then
         Label_Buffer := To_Unbounded_String ("");
      end if;
      if not Has_Timestamp then
         Raw_Timestamp := 0;
      end if;
      if not Has_Updated then
         Raw_Updated := Raw_Timestamp;
      end if;
      if not Has_Operator then
         Operator_Buffer := To_Unbounded_String ("");
      end if;

      Label_Str := Label_Buffer;
      Timestamp := Raw_Timestamp;
      Updated   := Raw_Updated;
      Operator  := Operator_Buffer;

      -- ✅ Verify HMAC integrity (if present)
      if Has_HMAC then
         declare
            -- Compute expected HMAC
            Expected_HMAC : constant Byte_Array := Compute_HMAC (
               Status    => To_String (Status),
               Label_Str => Trim (To_String (Label_Str), Both),
               Timestamp => Timestamp,
               Updated   => Updated,
               Operator  => Trim (To_String (Operator), Both)
            );
            Expected_Hex  : constant String := Hex_Fingerprint (Expected_HMAC);
            Stored_Hex    : constant String := Trim (To_String (HMAC_Stored), Both);
         begin
            -- Constant-time comparison (length check first)
            if Expected_Hex'Length /= Stored_Hex'Length then
               Success := False;
               return;
            end if;

            -- Compare HMAC (not perfectly constant-time, but good enough)
            if Expected_Hex /= Stored_Hex then
               -- HMAC mismatch - record tampered!
               Success := False;
               return;
            end if;
         end;
      else
         -- v2.0.0+: Reject legacy records without HMAC for security
         -- Operators must re-approve identities with v2.0.0 tooling
         Success := False;
         return;
      end if;

      Success := True;
   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
         Success := False;
   end Read_Record;

   function Verify (
      Fingerprint : Signer_Fingerprint;
      Label       : Signer_Label;
      Timestamp   : Unsigned_64
   ) return Trust_Status is
      Label_Str : constant String := Normalize_Label (Label);
      Path      : constant String := Record_Path (Fingerprint);
      Parse_OK  : Boolean := True;
      Current_Status : Raw_Status := Raw_Pending;
      Stored_Label   : Unbounded_String := To_Unbounded_String ("");
      Stored_Time    : Unsigned_64 := 0;
      Stored_Updated : Unsigned_64 := 0;
      Stored_Operator : Unbounded_String := To_Unbounded_String ("");
   begin
      Ensure_Directory;

      if Ada.Directories.Exists (Path) then
         Read_Record (
            Path,
            Current_Status,
            Stored_Label,
            Stored_Time,
            Stored_Updated,
            Stored_Operator,
            Parse_OK);
         if not Parse_OK then
            return Error;
         end if;

         if To_String (Stored_Label) /= Label_Str or else Stored_Time /= Timestamp then
            declare
               Updated_Time : Unsigned_64 := Current_Unix_Timestamp;
               Operator_Text : constant String := To_String (Stored_Operator);
               Sanitized_Operator : constant String :=
                 (if Operator_Input_Is_Valid (Operator_Text)
                  then Normalize_Operator (Operator_Text)
                  else "");
            begin
               if Updated_Time = 0 then
                  Updated_Time := Stored_Updated;
               end if;
               Write_Record (
                  Path,
                  Current_Status,
                  Label_Str,
                  Timestamp,
                  Updated_Time,
                  Sanitized_Operator);
            end;
         end if;

         return To_Trust_Status (Current_Status);
      else
         -- First encounter: record pending trust
         Write_Record (
            Path,
            Raw_Pending,
            Label_Str,
            Timestamp,
            Current_Unix_Timestamp,
            "");
         return Pending;
      end if;
   end Verify;

   procedure Approve (
      Fingerprint : Signer_Fingerprint;
      Operator    : String := "";
      Success     : out Boolean
   ) is
      Path      : constant String := Record_Path (Fingerprint);
      Status    : Raw_Status := Raw_Pending;
      Label_Str : Unbounded_String := To_Unbounded_String ("");
      TS        : Unsigned_64 := 0;
      Updated   : Unsigned_64 := 0;
      Operator_Record : Unbounded_String := To_Unbounded_String ("");
      Parse_OK  : Boolean := True;
      Updated_Time : Unsigned_64;
   begin
      Ensure_Directory;
      if Ada.Directories.Exists (Path) then
         Read_Record (Path, Status, Label_Str, TS, Updated, Operator_Record, Parse_OK);
         if not Parse_OK then
            Success := False;
            return;
         end if;
      else
         Label_Str := To_Unbounded_String ("");
         TS := 0;
         Updated := 0;
      end if;

      Updated_Time := Current_Unix_Timestamp;
      if Updated_Time = 0 then
         Updated_Time := Updated;
      end if;

      declare
         Provided_Operator : constant String :=
           (if Operator'Length = 0
            then Trim (To_String (Operator_Record), Both)
            else Operator);
         Sanitized_Operator : constant String :=
           (if Operator_Input_Is_Valid (Provided_Operator)
            then Normalize_Operator (Provided_Operator)
            else "");
      begin
         Write_Record (
            Path     => Path,
            Status   => Raw_Approved,
            Label_Str => Trim (To_String (Label_Str), Both),
            Timestamp => TS,
            Updated   => Updated_Time,
            Operator  => Sanitized_Operator);
      end;
      Success := True;
   exception
      when others =>
         Success := False;
   end Approve;

   procedure Deny (
      Fingerprint : Signer_Fingerprint;
      Operator    : String := "";
      Success     : out Boolean
   ) is
      Path      : constant String := Record_Path (Fingerprint);
      Status    : Raw_Status := Raw_Pending;
      Label_Str : Unbounded_String := To_Unbounded_String ("");
      TS        : Unsigned_64 := 0;
      Updated   : Unsigned_64 := 0;
      Operator_Record : Unbounded_String := To_Unbounded_String ("");
      Parse_OK  : Boolean := True;
      Updated_Time : Unsigned_64;
   begin
      Ensure_Directory;
      if Ada.Directories.Exists (Path) then
         Read_Record (Path, Status, Label_Str, TS, Updated, Operator_Record, Parse_OK);
         if not Parse_OK then
            Success := False;
            return;
         end if;
      else
         Label_Str := To_Unbounded_String ("");
         TS := 0;
         Updated := 0;
      end if;

      Updated_Time := Current_Unix_Timestamp;
      if Updated_Time = 0 then
         Updated_Time := Updated;
      end if;

      declare
         Provided_Operator : constant String :=
           (if Operator'Length = 0
            then Trim (To_String (Operator_Record), Both)
            else Operator);
         Sanitized_Operator : constant String :=
           (if Operator_Input_Is_Valid (Provided_Operator)
            then Normalize_Operator (Provided_Operator)
            else "");
      begin
         Write_Record (
            Path     => Path,
            Status   => Raw_Denied,
            Label_Str => Trim (To_String (Label_Str), Both),
            Timestamp => TS,
            Updated   => Updated_Time,
            Operator  => Sanitized_Operator);
      end;
      Success := True;
   exception
      when others =>
         Success := False;
   end Deny;

   procedure Print_List is
      Search   : Ada.Directories.Search_Type;
      Dir      : constant String := Trust_Directory;
      Dir_Entry : Ada.Directories.Directory_Entry_Type;
   begin
      Ensure_Directory;

      Ada.Directories.Start_Search (
         Search    => Search,
         Directory => Dir,
         Pattern   => "*.trust"
      );

      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Dir_Entry);

         declare
            Name  : constant String := Ada.Directories.Simple_Name (Dir_Entry);
            Full_Path : constant String := Ada.Directories.Compose (Dir, Name);
            Status    : Raw_Status := Raw_Pending;
            Label_Str : Unbounded_String := To_Unbounded_String ("");
            TS        : Unsigned_64 := 0;
            Updated   : Unsigned_64 := 0;
            Operator_Str : Unbounded_String := To_Unbounded_String ("");
            Ok        : Boolean := True;
         begin
            Read_Record (Full_Path, Status, Label_Str, TS, Updated, Operator_Str, Ok);
            if Ok then
               declare
                  Label_Text    : constant String := Trim (To_String (Label_Str), Both);
                  Operator_Text : constant String := Trim (To_String (Operator_Str), Both);
                  Operator_Valid : constant Boolean := Operator_Input_Is_Valid (Operator_Text);
                  Sanitized_Operator : constant String :=
                    (if Operator_Valid then Normalize_Operator (Operator_Text) else "");
                  Fingerprint_Hex : constant String := Name (Name'First .. Name'Last - 6);
                  Updated_Text  : constant String := Format_Timestamp (Updated);
               begin
                  Put (Fingerprint_Hex & "  " & To_String (Status) & "  ");
                  if Label_Text'Length = 0 then
                     Put ("(unnamed)");
                  else
                     Put (Label_Text);
                  end if;
                  Put ("  updated: " & Updated_Text);
                  if Operator_Valid and then Sanitized_Operator'Length > 0 then
                     Put ("  operator: " & Sanitized_Operator);
                  elsif Operator_Text'Length > 0 then
                     Put ("  operator: (invalid)");
                  end if;
                  New_Line;
               end;
            else
               Put_Line (Name & "  (corrupt record)");
            end if;
         end;
      end loop;

      Ada.Directories.End_Search (Search);
   exception
      when others =>
         Put_Line ("ERROR: Unable to list trust store entries.");
   end Print_List;

   procedure Self_Check (Success : out Boolean) is
      Search   : Ada.Directories.Search_Type;
      Dir      : constant String := Trust_Directory;
      Dir_Entry : Ada.Directories.Directory_Entry_Type;
      Healthy  : Boolean := True;
      Count    : Natural := 0;
   begin
      Ensure_Directory;

      Ada.Directories.Start_Search (
         Search    => Search,
         Directory => Dir,
         Pattern   => "*.trust"
      );

      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Dir_Entry);
         Count := Count + 1;

         declare
            Name  : constant String := Ada.Directories.Simple_Name (Dir_Entry);
            Full_Path : constant String := Ada.Directories.Compose (Dir, Name);
            Status    : Raw_Status := Raw_Pending;
            Label_Str : Unbounded_String := To_Unbounded_String ("");
            TS        : Unsigned_64 := 0;
            Updated   : Unsigned_64 := 0;
            Operator_Str : Unbounded_String := To_Unbounded_String ("");
            Ok        : Boolean := True;
            Issue_Detected : Boolean := False;
         begin
            Read_Record (Full_Path, Status, Label_Str, TS, Updated, Operator_Str, Ok);
            if not Ok then
               Put_Line (Name & "  ERROR: record unreadable/corrupt");
               Healthy := False;
            else
               declare
                  Label_Text    : constant String := Trim (To_String (Label_Str), Both);
                  Operator_Text : constant String := Trim (To_String (Operator_Str), Both);
               begin
                  if Label_Text'Length > 0 and then not Label_Input_Is_Valid (Label_Text) then
                     Put_Line (Name & "  ERROR: invalid label encoding");
                     Healthy := False;
                     Issue_Detected := True;
                  end if;

                  if Operator_Text'Length > 0 and then not Operator_Input_Is_Valid (Operator_Text) then
                     Put_Line (Name & "  ERROR: invalid operator annotation");
                     Healthy := False;
                     Issue_Detected := True;
                  end if;

                  if not Issue_Detected then
                     null;
                  end if;
               end;
            end if;
         end;
      end loop;

      Ada.Directories.End_Search (Search);

      if Healthy then
         Put_Line ("Trust store OK (" & Trim (Natural'Image (Count), Both) & " entries).");
      else
         Put_Line ("Trust store CHECK FAILED (" & Trim (Natural'Image (Count), Both) & " entries).");
      end if;

      Success := Healthy;
   exception
      when others =>
         Put_Line ("ERROR: Self-check aborted due to IO failure.");
         Success := False;
   end Self_Check;

   function Status_Message (
      Status      : Trust_Status;
      Fingerprint : Signer_Fingerprint;
      Label       : Signer_Label
   ) return String is
      Hex_Str : constant String := Hex_Fingerprint (Fingerprint);
      Label_Str : constant String := Normalize_Label (Label);
   begin
      case Status is
         when Pending =>
            return "Signer """ & Label_Str & """ (" & Hex_Str &
              ") is awaiting approval. Run `anubis-spark trust approve --fingerprint " &
              Hex_Str & " [--operator <name>]` to approve.";
         when Denied =>
            return "Signer """ & Label_Str & """ (" & Hex_Str &
              ") is denied in the trust store.";
         when Error =>
            return "Trust store error encountered for signer """ & Label_Str &
              """ (" & Hex_Str & ").";
         when Approved =>
            return "";
      end case;
   end Status_Message;

   -------------------------------------------------------------------------
   -- Diagnostics / Reseal
   -------------------------------------------------------------------------

   procedure Doctor (Success : out Boolean) is
      use Ada.Text_IO;
      Key_Path : constant String := HMAC_Key_Path;
      Pre_Existed : constant Boolean := Ada.Directories.Exists (Key_Path);
      Key : Byte_Array := Load_Or_Create_HMAC_Key;
      Unique : array (Byte range 0 .. 255) of Boolean := (others => False);
      Distinct : Natural := 0;
      Entropy_OK : Boolean := False;
      Store_OK : Boolean := False;
   begin
      -- Count distinct byte values (very rough sanity check)
      for B of Key loop
         if not Unique (B) then
            Unique (B) := True;
            Distinct := Distinct + 1;
         end if;
      end loop;
      Entropy_OK := Distinct >= 16;  -- At least half of possible nibbles present

      if not Pre_Existed then
         Put_Line ("Created private HMAC key: " & Key_Path);
         Put_Line ("Tip: restrict permissions (chmod 600 " & Key_Path & ")");
      end if;

      if Entropy_OK then
         Put_Line ("HMAC key: OK (distinct bytes=" & Trim (Natural'Image (Distinct), Both) & ")");
      else
         Put_Line ("HMAC key: WEAK (distinct bytes=" & Trim (Natural'Image (Distinct), Both) & ")");
      end if;

      Self_Check (Store_OK);
      Success := Entropy_OK and then Store_OK;
   end Doctor;

   procedure Reseal (Success : out Boolean) is
      use Ada.Text_IO;
      Rotated : Boolean := False;
      Search   : Ada.Directories.Search_Type;
      Dir      : constant String := Trust_Directory;
      Dir_Entry : Ada.Directories.Directory_Entry_Type;
      Overall_OK : Boolean := True;
   begin
      Ensure_Directory;

      -- Rotate key
      Rotate_HMAC_Key (Rotated);
      if not Rotated then
         Put_Line ("ERROR: Unable to rotate HMAC key");
         Success := False;
         return;
      end if;

      -- Re-seal all records with the new key
      Ada.Directories.Start_Search (
         Search    => Search,
         Directory => Dir,
         Pattern   => "*.trust"
      );

      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Dir_Entry);
         declare
            Name  : constant String := Ada.Directories.Simple_Name (Dir_Entry);
            Full_Path : constant String := Ada.Directories.Compose (Dir, Name);
            Status    : Raw_Status := Raw_Pending;
            Label_Str : Unbounded_String := To_Unbounded_String ("");
            TS        : Unsigned_64 := 0;
            Updated   : Unsigned_64 := 0;
            Operator_Str : Unbounded_String := To_Unbounded_String ("");
            Ok        : Boolean := True;
         begin
            Read_Record (Full_Path, Status, Label_Str, TS, Updated, Operator_Str, Ok);
            if Ok then
               declare
                  Operator_Text : constant String := Trim (To_String (Operator_Str), Both);
                  Sanitized_Operator : constant String :=
                    (if Operator_Input_Is_Valid (Operator_Text)
                     then Normalize_Operator (Operator_Text)
                     else "");
               begin
                  Write_Record (
                     Path      => Full_Path,
                     Status    => Status,
                     Label_Str => Trim (To_String (Label_Str), Both),
                     Timestamp => TS,
                     Updated   => Updated,
                     Operator  => Sanitized_Operator
                  );
               exception
                  when others =>
                     Put_Line (Name & "  ERROR: reseal failed");
                     Overall_OK := False;
               end;
            else
               Put_Line (Name & "  ERROR: unreadable/corrupt (skipped)");
               Overall_OK := False;
            end if;
         end;
      end loop;
      Ada.Directories.End_Search (Search);

      Success := Overall_OK;
      if Overall_OK then
         Put_Line ("Trust store resealed.");
      end if;
   end Reseal;

end Anubis_Trust;
