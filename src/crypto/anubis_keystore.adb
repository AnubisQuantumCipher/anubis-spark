-------------------------------------------------------------------------------
-- ANUBIS-SPARK: ANUBISK3 Keystore Implementation
-- Multi-passphrase keystore with elaborate contracts
-- SPARKNaCl principles: Complete zeroization, type-safety
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- Uses libsodium bindings

with Anubis_Types; use Anubis_Types;
with Interfaces.C; use Interfaces.C;
with System;

package body Anubis_Keystore is

   -------------------------------------------------------------------------
   -- Libsodium FFI Bindings
   -------------------------------------------------------------------------

   procedure randombytes_buf (
      buf  : out Byte_Array;
      size : size_t
   ) with Import, Convention => C, External_Name => "randombytes_buf";

   -------------------------------------------------------------------------
   -- Internal Helpers
   -------------------------------------------------------------------------

   -- Initialize keyslot to disabled state (zeroed)
   procedure Initialize_Keyslot (Keyslot : out Keyslot_Entry);

   procedure Initialize_Keyslot (Keyslot : out Keyslot_Entry) is
   begin
      Keyslot.Status := Disabled;
      for I in Keyslot.Argon2id_Salt'Range loop
         Keyslot.Argon2id_Salt (I) := 0;
      end loop;
      for I in Keyslot.AF_Salt'Range loop
         Keyslot.AF_Salt (I) := 0;
      end loop;
      for I in Keyslot.AF_Split_Data'Range loop
         Keyslot.AF_Split_Data (I) := 0;
      end loop;
   end Initialize_Keyslot;

   -- Derive XTS key and decrypt keyslot to recover master key
   procedure Unlock_Keyslot (
      Keyslot    : in     Keyslot_Entry;
      Passphrase : in     String;
      Master_Key : out    Master_Key_Data;
      Success    : out    Boolean
   );

   procedure Unlock_Keyslot (
      Keyslot    : in     Keyslot_Entry;
      Passphrase : in     String;
      Master_Key : out    Master_Key_Data;
      Success    : out    Boolean
   ) is
      XTS_Key  : XTS_Key_Wrapper;
      KDF_OK   : Boolean;
      Merge_OK : Boolean;
   begin
      -- Initialize master key to zeros
      for I in Master_Key'Range loop
         Master_Key (I) := 0;
      end loop;

      -- Step 1: Derive XTS key from passphrase using Argon2id
      Derive_XTS_Key_From_Passphrase (
         Passphrase => Passphrase,
         Salt       => Keyslot.Argon2id_Salt,
         Key        => XTS_Key,
         Success    => KDF_OK
      );

      if not KDF_OK then
         Success := False;
         Zeroize_XTS_Key (XTS_Key);
         return;
      end if;

      -- Step 2: Decrypt AF-split data (for now, just merge - AES decryption TODO)
      -- In full implementation: AES-XTS decrypt AF_Split_Data first
      -- For Phase 2: Direct AF-Merge (assumes AF_Split_Data is plaintext)
      AF_Merge (
         Split_Data => Keyslot.AF_Split_Data,
         Salt       => Keyslot.AF_Salt,
         Master_Key => Master_Key,
         Success    => Merge_OK
      );

      -- Zeroize XTS key (defense-in-depth)
      Zeroize_XTS_Key (XTS_Key);

      Success := Merge_OK;

   exception
      when others =>
         for I in Master_Key'Range loop
            Master_Key (I) := 0;
         end loop;
         Success := False;
   end Unlock_Keyslot;

   -- Encrypt master key into keyslot using passphrase
   procedure Lock_Keyslot (
      Master_Key     : in     Master_Key_Data;
      Passphrase     : in     String;
      Argon2id_Salt  : in     Byte_Array;
      AF_Salt        : in     Byte_Array;
      Keyslot        : out    Keyslot_Entry;
      Success        : out    Boolean
   );

   procedure Lock_Keyslot (
      Master_Key     : in     Master_Key_Data;
      Passphrase     : in     String;
      Argon2id_Salt  : in     Byte_Array;
      AF_Salt        : in     Byte_Array;
      Keyslot        : out    Keyslot_Entry;
      Success        : out    Boolean
   ) is
      XTS_Key  : XTS_Key_Wrapper;
      KDF_OK   : Boolean;
      Split_OK : Boolean;
   begin
      -- Initialize keyslot to zeros
      Initialize_Keyslot (Keyslot);

      -- Step 1: Derive XTS key from passphrase using Argon2id
      Derive_XTS_Key_From_Passphrase (
         Passphrase => Passphrase,
         Salt       => Argon2id_Salt,
         Key        => XTS_Key,
         Success    => KDF_OK
      );

      if not KDF_OK then
         Success := False;
         Zeroize_XTS_Key (XTS_Key);
         return;
      end if;

      -- Step 2: Split master key using AF-Splitter
      AF_Split (
         Master_Key => Master_Key,
         Salt       => AF_Salt,
         Split_Data => Keyslot.AF_Split_Data,
         Success    => Split_OK
      );

      if not Split_OK then
         Initialize_Keyslot (Keyslot);
         Success := False;
         Zeroize_XTS_Key (XTS_Key);
         return;
      end if;

      -- Step 3: Encrypt AF-split data with AES-XTS (EXPERIMENTAL/TODO)
      -- Current status: ANUBISK3 is not wired into the CLI, and the
      -- anubis_aes_xts module is a XChaCha20 wrapper used as a stand-in.
      -- For now, AF_Split_Data remains plaintext in the keyslot. Planned:
      -- replace wrapper with real AES-256-XTS (OpenSSL EVP) and enable
      -- sealed keyslots in the CLI flows.

      -- Step 4: Finalize keyslot
      Keyslot.Status := Active;
      Keyslot.Argon2id_Salt := Argon2id_Salt;
      Keyslot.AF_Salt := AF_Salt;

      -- Zeroize XTS key (defense-in-depth)
      Zeroize_XTS_Key (XTS_Key);

      Success := True;

   exception
      when others =>
         Initialize_Keyslot (Keyslot);
         Success := False;
   end Lock_Keyslot;

   -- Count active keyslots
   function Count_Active_Keyslots (Keystore : ANUBISK3_Keystore) return Natural;

   function Count_Active_Keyslots (Keystore : ANUBISK3_Keystore) return Natural is
      Count : Natural := 0;
   begin
      for I in Keyslot_Index loop
         if Keystore.Keyslots (I).Status = Active then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Active_Keyslots;

   -------------------------------------------------------------------------
   -- Keystore Initialization
   -------------------------------------------------------------------------

   procedure Create_Keystore (
      Passphrase : in     String;
      Keystore   : out    ANUBISK3_Keystore;
      Success    : out    Boolean
   ) is
      Argon2id_Salt : Byte_Array (1 .. 32);
      AF_Salt       : Byte_Array (1 .. 32);
      Lock_OK       : Boolean;
   begin
      -- Initialize keystore to zeros
      Keystore.Master_Valid := False;
      for I in Keystore.Master_Key'Range loop
         Keystore.Master_Key (I) := 0;
      end loop;

      -- Initialize header
      Keystore.Header.Magic := (
         Byte (Character'Pos ('A')),
         Byte (Character'Pos ('N')),
         Byte (Character'Pos ('U')),
         Byte (Character'Pos ('B')),
         Byte (Character'Pos ('3'))
      );
      Keystore.Header.Version := KEYSTORE_VERSION;
      for I in Keystore.Header.Reserved'Range loop
         Keystore.Header.Reserved (I) := 0;
      end loop;

      -- Initialize all keyslots to disabled
      for I in Keyslot_Index loop
         Initialize_Keyslot (Keystore.Keyslots (I));
      end loop;

      -- Step 1: Generate random master key
      begin
         randombytes_buf (Keystore.Master_Key, size_t (Keystore.Master_Key'Length));
      exception
         when others =>
            Success := False;
            return;
      end;

      -- Verify entropy
      if Is_All_Zero (Keystore.Master_Key) then
         Success := False;
         return;
      end if;

      Keystore.Master_Valid := True;

      -- Step 2: Generate random salts
      begin
         randombytes_buf (Argon2id_Salt, size_t (Argon2id_Salt'Length));
         randombytes_buf (AF_Salt, size_t (AF_Salt'Length));
      exception
         when others =>
            for I in Keystore.Master_Key'Range loop
               Keystore.Master_Key (I) := 0;
            end loop;
            Keystore.Master_Valid := False;
            Success := False;
            return;
      end;

      -- Step 3: Lock master key into first keyslot with passphrase
      Lock_Keyslot (
         Master_Key    => Keystore.Master_Key,
         Passphrase    => Passphrase,
         Argon2id_Salt => Argon2id_Salt,
         AF_Salt       => AF_Salt,
         Keyslot       => Keystore.Keyslots (1),
         Success       => Lock_OK
      );

      if not Lock_OK then
         -- Failed to lock keyslot - zeroize master key
         for I in Keystore.Master_Key'Range loop
            Keystore.Master_Key (I) := 0;
         end loop;
         Keystore.Master_Valid := False;
         Success := False;
         return;
      end if;

      -- Success: Keystore created with first passphrase
      Success := True;

   exception
      when others =>
         for I in Keystore.Master_Key'Range loop
            Keystore.Master_Key (I) := 0;
         end loop;
         Keystore.Master_Valid := False;
         Success := False;
   end Create_Keystore;

   -------------------------------------------------------------------------
   -- Keyslot Operations
   -------------------------------------------------------------------------

   procedure Unlock_Keystore (
      Keystore   : in out ANUBISK3_Keystore;
      Passphrase : in     String;
      Success    : out    Boolean
   ) is
      Unlock_OK : Boolean;
   begin
      -- Try each keyslot until one succeeds
      for I in Keyslot_Index loop
         if Keystore.Keyslots (I).Status = Active then
            Unlock_Keyslot (
               Keyslot    => Keystore.Keyslots (I),
               Passphrase => Passphrase,
               Master_Key => Keystore.Master_Key,
               Success    => Unlock_OK
            );

            if Unlock_OK then
               -- Success: Master key unlocked
               Keystore.Master_Valid := True;
               Success := True;
               return;
            end if;
         end if;
      end loop;

      -- No keyslot matched - passphrase incorrect
      Keystore.Master_Valid := False;
      for I in Keystore.Master_Key'Range loop
         Keystore.Master_Key (I) := 0;
      end loop;
      Success := False;

   exception
      when others =>
         Keystore.Master_Valid := False;
         for I in Keystore.Master_Key'Range loop
            Keystore.Master_Key (I) := 0;
         end loop;
         Success := False;
   end Unlock_Keystore;

   procedure Lock_Keystore (
      Keystore : in out ANUBISK3_Keystore
   ) is
   begin
      -- Zeroize master key (SPARKNaCl principle: explicit zeroization)
      Keystore.Master_Valid := False;
      for I in Keystore.Master_Key'Range loop
         Keystore.Master_Key (I) := 0;
      end loop;
   end Lock_Keystore;

   procedure Add_Keyslot (
      Keystore       : in out ANUBISK3_Keystore;
      New_Passphrase : in     String;
      Slot_Index     : out    Keyslot_Index;
      Success        : out    Boolean
   ) is
      Argon2id_Salt : Byte_Array (1 .. 32);
      AF_Salt       : Byte_Array (1 .. 32);
      Lock_OK       : Boolean;
   begin
      -- Find first empty keyslot
      for I in Keyslot_Index loop
         if Keystore.Keyslots (I).Status = Disabled then
            -- Generate random salts
            begin
               randombytes_buf (Argon2id_Salt, size_t (Argon2id_Salt'Length));
               randombytes_buf (AF_Salt, size_t (AF_Salt'Length));
            exception
               when others =>
                  Success := False;
                  return;
            end;

            -- Lock master key into this keyslot
            Lock_Keyslot (
               Master_Key    => Keystore.Master_Key,
               Passphrase    => New_Passphrase,
               Argon2id_Salt => Argon2id_Salt,
               AF_Salt       => AF_Salt,
               Keyslot       => Keystore.Keyslots (I),
               Success       => Lock_OK
            );

            if Lock_OK then
               Slot_Index := I;
               Success := True;
               return;
            else
               Success := False;
               return;
            end if;
         end if;
      end loop;

      -- No empty keyslot found
      Success := False;

   exception
      when others =>
         Success := False;
   end Add_Keyslot;

   procedure Remove_Keyslot (
      Keystore   : in out ANUBISK3_Keystore;
      Slot_Index : in     Keyslot_Index;
      Success    : out    Boolean
   ) is
      Active_Count : Natural;
   begin
      -- Check if this is the last active keyslot
      Active_Count := Count_Active_Keyslots (Keystore);

      if Active_Count <= 1 then
         -- Cannot remove last keyslot (would lose access)
         Success := False;
         return;
      end if;

      -- Zeroize and disable keyslot
      Initialize_Keyslot (Keystore.Keyslots (Slot_Index));
      Success := True;

   exception
      when others =>
         Success := False;
   end Remove_Keyslot;

   procedure Change_Keyslot_Passphrase (
      Keystore       : in out ANUBISK3_Keystore;
      Slot_Index     : in     Keyslot_Index;
      New_Passphrase : in     String;
      Success        : out    Boolean
   ) is
      Argon2id_Salt : Byte_Array (1 .. 32);
      AF_Salt       : Byte_Array (1 .. 32);
      New_Keyslot   : Keyslot_Entry;
      Lock_OK       : Boolean;
   begin
      -- Generate new random salts (key rotation best practice)
      begin
         randombytes_buf (Argon2id_Salt, size_t (Argon2id_Salt'Length));
         randombytes_buf (AF_Salt, size_t (AF_Salt'Length));
      exception
         when others =>
            Success := False;
            return;
      end;

      -- Lock master key with new passphrase
      Lock_Keyslot (
         Master_Key    => Keystore.Master_Key,
         Passphrase    => New_Passphrase,
         Argon2id_Salt => Argon2id_Salt,
         AF_Salt       => AF_Salt,
         Keyslot       => New_Keyslot,
         Success       => Lock_OK
      );

      if Lock_OK then
         -- Replace old keyslot with new one (atomic update)
         Keystore.Keyslots (Slot_Index) := New_Keyslot;
         Success := True;
      else
         Success := False;
      end if;

   exception
      when others =>
         Success := False;
   end Change_Keyslot_Passphrase;

   -------------------------------------------------------------------------
   -- Keystore Serialization
   -------------------------------------------------------------------------

   procedure Serialize_Keystore (
      Keystore : in     ANUBISK3_Keystore;
      Data     : out    Byte_Array;
      Success  : out    Boolean
   ) is
      Offset : Natural := Data'First;

      procedure Write_Bytes (Bytes : Byte_Array);
      procedure Write_Bytes (Bytes : Byte_Array) is
      begin
         for I in Bytes'Range loop
            Data (Offset) := Bytes (I);
            Offset := Offset + 1;
         end loop;
      end Write_Bytes;

      procedure Write_Byte (B : Byte);
      procedure Write_Byte (B : Byte) is
      begin
         Data (Offset) := B;
         Offset := Offset + 1;
      end Write_Byte;

   begin
      -- Initialize data to zeros
      for I in Data'Range loop
         Data (I) := 0;
      end loop;

      Offset := Data'First;

      -- Write header
      Write_Bytes (Keystore.Header.Magic);
      Write_Byte (Keystore.Header.Version);
      Write_Bytes (Keystore.Header.Reserved);

      -- Write keyslots
      for I in Keyslot_Index loop
         -- Write status byte
         if Keystore.Keyslots (I).Status = Active then
            Write_Byte (1);
         else
            Write_Byte (0);
         end if;

         -- Write salts
         Write_Bytes (Keystore.Keyslots (I).Argon2id_Salt);
         Write_Bytes (Keystore.Keyslots (I).AF_Salt);

         -- Write AF-split data
         Write_Bytes (Keystore.Keyslots (I).AF_Split_Data);
      end loop;

      Success := True;

   exception
      when others =>
         for I in Data'Range loop
            Data (I) := 0;
         end loop;
         Success := False;
   end Serialize_Keystore;

   procedure Deserialize_Keystore (
      Data     : in     Byte_Array;
      Keystore : out    ANUBISK3_Keystore;
      Success  : out    Boolean
   ) is
      Offset : Natural := Data'First;

      procedure Read_Bytes (Bytes : out Byte_Array);
      procedure Read_Bytes (Bytes : out Byte_Array) is
      begin
         for I in Bytes'Range loop
            Bytes (I) := Data (Offset);
            Offset := Offset + 1;
         end loop;
      end Read_Bytes;

      function Read_Byte return Byte;
      function Read_Byte return Byte is
         B : constant Byte := Data (Offset);
      begin
         Offset := Offset + 1;
         return B;
      end Read_Byte;

   begin
      -- Initialize keystore
      Keystore.Master_Valid := False;
      for I in Keystore.Master_Key'Range loop
         Keystore.Master_Key (I) := 0;
      end loop;

      Offset := Data'First;

      -- Read header
      Read_Bytes (Keystore.Header.Magic);
      Keystore.Header.Version := Read_Byte;
      Read_Bytes (Keystore.Header.Reserved);

      -- Validate header
      if Keystore.Header.Magic /= (
         Byte (Character'Pos ('A')),
         Byte (Character'Pos ('N')),
         Byte (Character'Pos ('U')),
         Byte (Character'Pos ('B')),
         Byte (Character'Pos ('3')))
      then
         Success := False;
         return;
      end if;

      if Keystore.Header.Version /= KEYSTORE_VERSION then
         Success := False;
         return;
      end if;

      -- Read keyslots
      for I in Keyslot_Index loop
         declare
            Status_Byte : constant Byte := Read_Byte;
         begin
            if Status_Byte = 1 then
               Keystore.Keyslots (I).Status := Active;
            else
               Keystore.Keyslots (I).Status := Disabled;
            end if;
         end;

         -- Read salts
         Read_Bytes (Keystore.Keyslots (I).Argon2id_Salt);
         Read_Bytes (Keystore.Keyslots (I).AF_Salt);

         -- Read AF-split data
         Read_Bytes (Keystore.Keyslots (I).AF_Split_Data);
      end loop;

      -- Verify at least one keyslot is active
      if Count_Active_Keyslots (Keystore) = 0 then
         Success := False;
         return;
      end if;

      Success := True;

   exception
      when others =>
         Keystore.Master_Valid := False;
         for I in Keystore.Master_Key'Range loop
            Keystore.Master_Key (I) := 0;
         end loop;
         Success := False;
   end Deserialize_Keystore;

end Anubis_Keystore;
