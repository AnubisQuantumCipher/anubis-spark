-------------------------------------------------------------------------------
-- ANUBIS-SPARK v1.1.0: Simple Encrypted Keystore Test
-- Quick test of Save/Load encrypted keystore functionality
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.Storage; use Anubis_Types.Storage;

procedure Test_Keystore_Simple is

   Test_File : constant String := "/tmp/test-simple-keystore.anubisk2";
   Passphrase : constant String := "TestPass123!";

   Identity1 : Identity_Keypair;
   Identity2 : Identity_Keypair;
   Success : Boolean;

begin
   Put_Line ("ANUBIS-SPARK v1.1.0: Simple Encrypted Keystore Test");
   Put_Line ("======================================================");
   New_Line;

   -- Test 1: Generate identity
   Put_Line ("[1/4] Generating identity...");
   Generate_Identity (Identity1, Success);

   if not Success then
      Put_Line ("ERROR: Failed to generate identity");
      return;
   end if;
   Put_Line ("      SUCCESS: Identity generated");
   New_Line;

   -- Test 2: Save encrypted
   Put_Line ("[2/4] Saving encrypted keystore (Argon2id SENSITIVE: ~3-5 seconds)...");
   Save_Identity_Encrypted (
      Identity   => Identity1,
      Filename   => Test_File,
      Passphrase => Passphrase,
      Success    => Success
   );

   if not Success then
      Put_Line ("ERROR: Failed to save encrypted keystore");
      Zeroize_Identity (Identity1);
      return;
   end if;
   Put_Line ("      SUCCESS: Encrypted keystore saved");
   New_Line;

   -- Test 3: Load encrypted (correct passphrase)
   Put_Line ("[3/4] Loading encrypted keystore with correct passphrase...");
   Load_Identity_Encrypted (
      Filename   => Test_File,
      Passphrase => Passphrase,
      Identity   => Identity2,
      Success    => Success
   );

   if not Success then
      Put_Line ("ERROR: Failed to load encrypted keystore");
      Zeroize_Identity (Identity1);
      return;
   end if;
   Put_Line ("      SUCCESS: Loaded encrypted keystore");
   New_Line;

   -- Test 4: Verify keys match
   Put_Line ("[4/4] Verifying loaded keys match original...");

   declare
      PK1 : constant X25519_Public_Key := Get_X25519_Public (Identity1);
      PK2 : constant X25519_Public_Key := Get_X25519_Public (Identity2);
   begin
      if PK1 = PK2 then
         Put_Line ("      SUCCESS: X25519 public keys match");
      else
         Put_Line ("ERROR: X25519 public keys DO NOT match!");
         Zeroize_Identity (Identity1);
         Zeroize_Identity (Identity2);
         return;
      end if;
   end;

   -- Cleanup
   Zeroize_Identity (Identity1);
   Zeroize_Identity (Identity2);

   New_Line;
   Put_Line ("======================================================");
   Put_Line ("RESULT: ALL TESTS PASSED");
   Put_Line ("======================================================");

end Test_Keystore_Simple;
