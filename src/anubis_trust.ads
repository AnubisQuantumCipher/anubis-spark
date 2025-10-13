-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Signer Trust Store Interface
-- Provides TOFU-based trust verification and management commands
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Trust is

   type Trust_Status is (Approved, Pending, Denied, Error);

   -- Verify signer fingerprint according to trust policy (TOFU + explicit approval)
   function Verify (
      Fingerprint : Signer_Fingerprint;
      Label       : Signer_Label;
      Timestamp   : Unsigned_64
   ) return Trust_Status;

   -- Approve signer fingerprint (creates/updates trust record)
   procedure Approve (
      Fingerprint : Signer_Fingerprint;
      Operator    : String := "";
      Success     : out Boolean
   );

   -- Deny signer fingerprint
   procedure Deny (
      Fingerprint : Signer_Fingerprint;
      Operator    : String := "";
      Success     : out Boolean
   );

   -- Print trust store entries (for CLI listing)
   procedure Print_List;

   -- Validate all trust records in the store
   procedure Self_Check (Success : out Boolean);

   -- Perform trust-store diagnostics: ensure HMAC key presence and plausibility
   -- and run Self_Check. Returns Success=True when all checks pass.
   procedure Doctor (Success : out Boolean);

   -- Rotate the private HMAC key and re-seal all trust records with new HMACs.
   -- Useful if the key is suspected to be compromised or migrated.
   procedure Reseal (Success : out Boolean);

   -- Convert fingerprint bytes to hexadecimal and vice versa
   function Hex_Fingerprint (Fingerprint : Signer_Fingerprint) return String;
   function Parse_Fingerprint (Hex : String; Fingerprint : out Signer_Fingerprint) return Boolean;

   -- Produce human-friendly message for trust status transitions
   function Status_Message (
      Status      : Trust_Status;
      Fingerprint : Signer_Fingerprint;
      Label       : Signer_Label
   ) return String;

end Anubis_Trust;
