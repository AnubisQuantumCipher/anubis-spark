-------------------------------------------------------------------------------
-- ANUBIS-SPARK: ANUBISK3 Keystore Format (LUKS2-Inspired)
-- Multi-passphrase keystore with 8 keyslots for master key protection
-- Two-Kill Defense: Argon2id + AES-XTS → Quantum Hybrid Encryption
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Anubis_AF_Splitter; use Anubis_AF_Splitter;
with Anubis_AES_XTS; use Anubis_AES_XTS;
with Interfaces; use Interfaces;

package Anubis_Keystore is

   -------------------------------------------------------------------------
   -- Constants
   -------------------------------------------------------------------------

   KEYSTORE_MAGIC   : constant String := "ANUB3";  -- 5 bytes magic identifier
   KEYSTORE_VERSION : constant Byte := 1;          -- Version 1

   MAX_KEYSLOTS     : constant := 8;               -- Maximum number of passphrases
   KEYSLOT_SIZE     : constant := 128_065;         -- 128 KB per keyslot (1 + 32 + 32 + 128000)
   HEADER_SIZE      : constant := 256;             -- Header metadata (256 bytes)
   KEYSTORE_SIZE    : constant := HEADER_SIZE + (KEYSLOT_SIZE * MAX_KEYSLOTS);  -- ~1 MB

   -------------------------------------------------------------------------
   -- Types
   -------------------------------------------------------------------------

   -- Keyslot status
   type Keyslot_Status is (
      Disabled,  -- Keyslot not in use
      Active     -- Keyslot contains valid key material
   );

   -- Keyslot index (1-8)
   subtype Keyslot_Index is Natural range 1 .. MAX_KEYSLOTS;

   -- Individual keyslot structure
   type Keyslot_Entry is record
      Status         : Keyslot_Status;     -- Active or disabled
      Argon2id_Salt  : Byte_Array (1 .. 32);  -- Salt for passphrase derivation
      AF_Salt        : Byte_Array (1 .. 32);  -- Salt for AF-Splitter
      AF_Split_Data  : AF_Split_Data;      -- Diffused master key (128,000 bytes)
   end record;

   -- Keystore header
   type Keystore_Header is record
      Magic          : Byte_Array (1 .. 5);   -- "ANUB3"
      Version        : Byte;                   -- Version 1
      Reserved       : Byte_Array (1 .. 250); -- Reserved for future use
   end record;

   -- Complete ANUBISK3 keystore
   type ANUBISK3_Keystore is record
      Header         : Keystore_Header;
      Keyslots       : array (Keyslot_Index) of Keyslot_Entry;
      Master_Key     : Master_Key_Data;       -- In-memory master key (never on disk)
      Master_Valid   : Boolean := False;      -- Is master key unlocked?
   end record;

   -------------------------------------------------------------------------
   -- Ghost Functions (PLATINUM+ Verification)
   -------------------------------------------------------------------------

   -- Ghost: Verify keystore has valid magic and version
   function Keystore_Header_Valid (Header : Keystore_Header) return Boolean is
      (Header.Magic = (Byte (Character'Pos ('A')),
                       Byte (Character'Pos ('N')),
                       Byte (Character'Pos ('U')),
                       Byte (Character'Pos ('B')),
                       Byte (Character'Pos ('3'))) and then
       Header.Version = KEYSTORE_VERSION)
   with Ghost;

   -- Ghost: Verify at least one keyslot is active
   function Has_Active_Keyslot (Keystore : ANUBISK3_Keystore) return Boolean is
      ((for some I in Keyslot_Index => Keystore.Keyslots (I).Status = Active))
   with Ghost;

   -- Ghost: Verify keyslot has entropy
   function Keyslot_Has_Entropy (Keyslot : Keyslot_Entry) return Boolean is
      (Keyslot.Status = Active and then
       Split_Data_Has_Entropy (Keyslot.AF_Split_Data) and then
       not Is_All_Zero (Keyslot.Argon2id_Salt) and then
       not Is_All_Zero (Keyslot.AF_Salt))
   with Ghost;

   -- Ghost: Verify master key is unlocked and valid
   function Master_Key_Unlocked (Keystore : ANUBISK3_Keystore) return Boolean is
      (Keystore.Master_Valid and then
       Master_Key_Has_Entropy (Keystore.Master_Key))
   with Ghost;

   -- Ghost: Verify keystore is locked (master key zeroed)
   function Keystore_Is_Locked (Keystore : ANUBISK3_Keystore) return Boolean is
      (not Keystore.Master_Valid and then
       Is_All_Zero (Keystore.Master_Key))
   with Ghost;

   -- Ghost: Verify keyslot index is valid
   function Keyslot_Index_Valid (Index : Natural) return Boolean is
      (Index >= 1 and Index <= MAX_KEYSLOTS)
   with Ghost;

   -------------------------------------------------------------------------
   -- Keystore Initialization
   -------------------------------------------------------------------------

   -- Create new keystore with first passphrase
   -- PLATINUM: Elaborate contract proving keystore initialization
   procedure Create_Keystore (
      Passphrase : in     String;
      Keystore   : out    ANUBISK3_Keystore;
      Success    : out    Boolean
   ) with
      Pre  => -- ELABORATE PRECONDITION: All creation requirements
              Passphrase'Length >= 12 and then   -- Minimum passphrase length
              Passphrase'Length <= 256,          -- Maximum reasonable length
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE POSTCONDITION: Prove all creation properties
              (if Success then
                  -- On success: Keystore initialized with valid header and keyslot
                  (Keystore_Header_Valid (Keystore.Header) and then
                   Has_Active_Keyslot (Keystore) and then
                   Keystore.Keyslots (1).Status = Active and then
                   Keyslot_Has_Entropy (Keystore.Keyslots (1)) and then
                   Master_Key_Unlocked (Keystore))
               else
                  -- On failure: Keystore locked and zeroed
                  Keystore_Is_Locked (Keystore)),
      Contract_Cases => (
         -- SUCCESS: Keystore created with first passphrase
         Success and Passphrase'Length >= 12 =>
            (Keystore_Header_Valid (Keystore.Header) and then
             Has_Active_Keyslot (Keystore) and then
             Master_Key_Unlocked (Keystore)),
         -- FAILURE: Passphrase too short or crypto failure
         not Success =>
            Keystore_Is_Locked (Keystore)
      );

   -------------------------------------------------------------------------
   -- Keyslot Operations
   -------------------------------------------------------------------------

   -- Unlock keystore with passphrase (load master key into memory)
   -- PLATINUM: Elaborate contract proving secure unlocking
   procedure Unlock_Keystore (
      Keystore   : in out ANUBISK3_Keystore;
      Passphrase : in     String;
      Success    : out    Boolean
   ) with
      Pre  => -- ELABORATE PRECONDITION: All unlock requirements
              Passphrase'Length >= 12 and then
              Passphrase'Length <= 256 and then
              Keystore_Header_Valid (Keystore.Header) and then
              Has_Active_Keyslot (Keystore) and then
              not Master_Key_Unlocked (Keystore),  -- Must be locked
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE POSTCONDITION: Prove all unlock outcomes
              Keystore_Header_Valid (Keystore.Header) and then  -- Header unchanged
              Has_Active_Keyslot (Keystore) and then            -- Keyslots unchanged
              (if Success then
                  -- On success: Master key unlocked and has entropy
                  Master_Key_Unlocked (Keystore)
               else
                  -- On failure: Keystore remains locked
                  Keystore_Is_Locked (Keystore)),
      Contract_Cases => (
         -- SUCCESS: Passphrase matches, master key unlocked
         Success =>
            Master_Key_Unlocked (Keystore),
         -- FAILURE: Wrong passphrase or crypto failure
         not Success =>
            Keystore_Is_Locked (Keystore)
      );

   -- Lock keystore (zeroize master key in memory)
   -- PLATINUM: Elaborate postcondition proves complete zeroization
   procedure Lock_Keystore (
      Keystore : in out ANUBISK3_Keystore
   ) with
      Global => null,  -- FRAME: No side effects
      Post   => -- ELABORATE: Prove complete locking
                Keystore_Is_Locked (Keystore) and then
                (for all I in Keystore.Master_Key'Range =>
                   Keystore.Master_Key (I) = 0);

   -- Add new passphrase to empty keyslot
   -- PLATINUM: Elaborate contract proving keyslot addition
   procedure Add_Keyslot (
      Keystore       : in out ANUBISK3_Keystore;
      New_Passphrase : in     String;
      Slot_Index     : out    Keyslot_Index;
      Success        : out    Boolean
   ) with
      Pre  => -- ELABORATE PRECONDITION: All requirements for adding keyslot
              New_Passphrase'Length >= 12 and then
              New_Passphrase'Length <= 256 and then
              Keystore_Header_Valid (Keystore.Header) and then
              Master_Key_Unlocked (Keystore) and then  -- Must be unlocked
              Has_Active_Keyslot (Keystore),
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE POSTCONDITION: Prove all addition outcomes
              Keystore_Header_Valid (Keystore.Header) and then  -- Header unchanged
              Master_Key_Unlocked (Keystore) and then           -- Master key unchanged
              (if Success then
                  -- On success: New keyslot is active with entropy
                  (Keyslot_Index_Valid (Slot_Index) and then
                   Keystore.Keyslots (Slot_Index).Status = Active and then
                   Keyslot_Has_Entropy (Keystore.Keyslots (Slot_Index)) and then
                   Has_Active_Keyslot (Keystore))
               else
                  -- On failure: Keyslots unchanged
                  Has_Active_Keyslot (Keystore)),
      Contract_Cases => (
         -- SUCCESS: Keyslot added successfully
         Success =>
            (Keyslot_Index_Valid (Slot_Index) and then
             Keystore.Keyslots (Slot_Index).Status = Active and then
             Master_Key_Unlocked (Keystore)),
         -- FAILURE: All keyslots full or crypto failure
         not Success =>
            (Master_Key_Unlocked (Keystore) and then
             Has_Active_Keyslot (Keystore))
      );

   -- Remove passphrase from keyslot
   -- PLATINUM: Elaborate contract proving safe removal
   procedure Remove_Keyslot (
      Keystore   : in out ANUBISK3_Keystore;
      Slot_Index : in     Keyslot_Index;
      Success    : out    Boolean
   ) with
      Pre  => -- ELABORATE PRECONDITION: All requirements for removal
              Keystore_Header_Valid (Keystore.Header) and then
              Master_Key_Unlocked (Keystore) and then  -- Must be unlocked
              Has_Active_Keyslot (Keystore) and then
              Keyslot_Index_Valid (Slot_Index),
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE POSTCONDITION: Prove all removal outcomes
              Keystore_Header_Valid (Keystore.Header) and then  -- Header unchanged
              Master_Key_Unlocked (Keystore) and then           -- Master key unchanged
              (if Success then
                  -- On success: Keyslot disabled and zeroed
                  (Keystore.Keyslots (Slot_Index).Status = Disabled and then
                   Is_All_Zero (Keystore.Keyslots (Slot_Index).AF_Split_Data))
               else
                  -- On failure: Cannot remove last keyslot
                  Has_Active_Keyslot (Keystore)),
      Contract_Cases => (
         -- SUCCESS: Keyslot removed (not the last one)
         Success =>
            (Keystore.Keyslots (Slot_Index).Status = Disabled and then
             Master_Key_Unlocked (Keystore)),
         -- FAILURE: Cannot remove last keyslot
         not Success =>
            (Has_Active_Keyslot (Keystore) and then
             Master_Key_Unlocked (Keystore))
      );

   -- Change passphrase in keyslot
   -- PLATINUM: Elaborate contract proving secure passphrase rotation
   procedure Change_Keyslot_Passphrase (
      Keystore       : in out ANUBISK3_Keystore;
      Slot_Index     : in     Keyslot_Index;
      New_Passphrase : in     String;
      Success        : out    Boolean
   ) with
      Pre  => -- ELABORATE PRECONDITION: All requirements for passphrase change
              New_Passphrase'Length >= 12 and then
              New_Passphrase'Length <= 256 and then
              Keystore_Header_Valid (Keystore.Header) and then
              Master_Key_Unlocked (Keystore) and then  -- Must be unlocked
              Has_Active_Keyslot (Keystore) and then
              Keyslot_Index_Valid (Slot_Index) and then
              Keystore.Keyslots (Slot_Index).Status = Active,
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE POSTCONDITION: Prove all change outcomes
              Keystore_Header_Valid (Keystore.Header) and then  -- Header unchanged
              Master_Key_Unlocked (Keystore) and then           -- Master key unchanged
              Keystore.Keyslots (Slot_Index).Status = Active and then
              (if Success then
                  -- On success: Keyslot updated with new passphrase
                  (Keyslot_Has_Entropy (Keystore.Keyslots (Slot_Index)) and then
                   Has_Active_Keyslot (Keystore))
               else
                  -- On failure: Keyslot unchanged
                  Has_Active_Keyslot (Keystore)),
      Contract_Cases => (
         -- SUCCESS: Passphrase changed successfully
         Success =>
            (Keyslot_Has_Entropy (Keystore.Keyslots (Slot_Index)) and then
             Master_Key_Unlocked (Keystore)),
         -- FAILURE: Crypto failure, keyslot unchanged
         not Success =>
            (Keystore.Keyslots (Slot_Index).Status = Active and then
             Master_Key_Unlocked (Keystore))
      );

   -------------------------------------------------------------------------
   -- Keystore Serialization
   -------------------------------------------------------------------------

   -- Serialize keystore to bytes (for saving to disk)
   -- PLATINUM: Elaborate contract proving complete serialization
   procedure Serialize_Keystore (
      Keystore : in     ANUBISK3_Keystore;
      Data     : out    Byte_Array;
      Success  : out    Boolean
   ) with
      Pre  => -- ELABORATE PRECONDITION: All serialization requirements
              Keystore_Header_Valid (Keystore.Header) and then
              Has_Active_Keyslot (Keystore) and then
              Data'Length = KEYSTORE_SIZE,  -- Exact size required
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE POSTCONDITION: Prove all serialization outcomes
              (if Success then
                  -- On success: Data contains serialized keystore
                  not Is_All_Zero (Data)
               else
                  -- On failure: Data zeroed
                  Is_All_Zero (Data)),
      Contract_Cases => (
         -- SUCCESS: Keystore serialized successfully
         Success =>
            not Is_All_Zero (Data),
         -- FAILURE: Serialization failed
         not Success =>
            Is_All_Zero (Data)
      );

   -- Deserialize keystore from bytes (for loading from disk)
   -- PLATINUM: Elaborate contract proving safe deserialization
   procedure Deserialize_Keystore (
      Data     : in     Byte_Array;
      Keystore : out    ANUBISK3_Keystore;
      Success  : out    Boolean
   ) with
      Pre  => -- ELABORATE PRECONDITION: All deserialization requirements
              Data'Length = KEYSTORE_SIZE and then  -- Exact size required
              not Is_All_Zero (Data),               -- Data has content
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE POSTCONDITION: Prove all deserialization outcomes
              (if Success then
                  -- On success: Keystore initialized with valid header
                  (Keystore_Header_Valid (Keystore.Header) and then
                   Has_Active_Keyslot (Keystore) and then
                   Keystore_Is_Locked (Keystore))  -- Always locked after load
               else
                  -- On failure: Keystore zeroed
                  Keystore_Is_Locked (Keystore)),
      Contract_Cases => (
         -- SUCCESS: Valid keystore loaded (always locked)
         Success =>
            (Keystore_Header_Valid (Keystore.Header) and then
             Has_Active_Keyslot (Keystore) and then
             Keystore_Is_Locked (Keystore)),
         -- FAILURE: Invalid format or corruption
         not Success =>
            Keystore_Is_Locked (Keystore)
      );

end Anubis_Keystore;
