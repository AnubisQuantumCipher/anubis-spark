-------------------------------------------------------------------------------
-- ANUBIS-SPARK v1.1.0: Encrypted Keystore Test
-- Tests Argon2id + XChaCha20-Poly1305 passphrase-protected keystores
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.Storage; use Anubis_Types.Storage;

procedure Test_Encrypted_Keystore is

   Test_File : constant String := "/tmp/test-encrypted-keystore-v1.1.0.anubisk2";
   Passphrase : constant String := "SuperSecure#Quantum2024!";
   Wrong_Passphrase : constant String := "WrongPassword123!";

   Identity1 : Identity_Keypair;
   Identity2 : Identity_Keypair;
   Success : Boolean;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   Fail_Count : Natural := 0;

   -- ANSI colors
   Red    : constant String := ASCII.ESC & "[31m";
   Green  : constant String := ASCII.ESC & "[32m";
   Yellow : constant String := ASCII.ESC & "[33m";
   Blue   : constant String := ASCII.ESC & "[34m";
   Cyan   : constant String := ASCII.ESC & "[36m";
   Reset  : constant String := ASCII.ESC & "[0m";

   procedure Test_Header (Name : String) is
   begin
      New_Line;
      Put_Line (Cyan & "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" & Reset);
      Put_Line (Cyan & "TEST: " & Name & Reset);
      Put_Line (Cyan & "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" & Reset);
   end Test_Header;

   procedure Test_Pass (Msg : String) is
   begin
      Put_Line (Green & "✓ PASS" & Reset & ": " & Msg);
      Pass_Count := Pass_Count + 1;
   end Test_Pass;

   procedure Test_Fail (Msg : String) is
   begin
      Put_Line (Red & "✗ FAIL" & Reset & ": " & Msg);
      Fail_Count := Fail_Count + 1;
   end Test_Fail;

   procedure Test_Info (Msg : String) is
   begin
      Put_Line (Blue & "ℹ INFO" & Reset & ": " & Msg);
   end Test_Info;

begin
   Put_Line (Cyan & "═══════════════════════════════════════════════════════════" & Reset);
   Put_Line (Cyan & "ANUBIS-SPARK v1.1.0: Encrypted Keystore Test Suite" & Reset);
   Put_Line (Cyan & "═══════════════════════════════════════════════════════════" & Reset);

   -- Clean up old test file
   if Ada.Directories.Exists (Test_File) then
      Ada.Directories.Delete_File (Test_File);
   end if;

   -- =========================================================================
   -- TEST 1: Generate identity and save encrypted keystore
   -- =========================================================================
   Test_Header ("Generate Identity and Save Encrypted Keystore");

   Test_Info ("Generating new identity keypair...");
   Generate_Identity (Identity1, Success);

   if Success then
      Test_Pass ("Identity generated successfully");
   else
      Test_Fail ("Failed to generate identity");
      return;
   end if;

   Test_Info ("Saving to encrypted keystore: " & Test_File);
   Test_Info ("Passphrase: " & Passphrase);

   Save_Identity_Encrypted (
      Identity   => Identity1,
      Filename   => Test_File,
      Passphrase => Passphrase,
      Success    => Success
   );

   if Success then
      Test_Pass ("Encrypted keystore saved successfully");
   else
      Test_Fail ("Failed to save encrypted keystore");
      return;
   end if;

   -- Verify file exists
   if Ada.Directories.Exists (Test_File) then
      Test_Pass ("Keystore file exists: " & Test_File);

      declare
         File_Size : constant Ada.Directories.File_Size := Ada.Directories.Size (Test_File);
      begin
         Test_Info ("File size:" & File_Size'Image & " bytes");

         -- Expected size: 8 (magic) + 2 (version) + 16 (KDF params) + 16 (salt) +
         --                24 (nonce) + 4 (length) + 12352 (ciphertext) + 16 (tag)
         --                = 12,438 bytes
         if File_Size >= 12_400 and File_Size <= 12_500 then
            Test_Pass ("File size is within expected range (~12,438 bytes)");
         else
            Test_Fail ("File size is outside expected range");
         end if;
      end;
   else
      Test_Fail ("Keystore file not found");
      return;
   end if;

   -- =========================================================================
   -- TEST 2: Load encrypted keystore with correct passphrase
   -- =========================================================================
   Test_Header ("Load Encrypted Keystore (Correct Passphrase)");

   Test_Info ("Loading keystore with correct passphrase...");

   Load_Identity_Encrypted (
      Filename   => Test_File,
      Passphrase => Passphrase,
      Identity   => Identity2,
      Success    => Success
   );

   if Success then
      Test_Pass ("Loaded encrypted keystore successfully");
   else
      Test_Fail ("Failed to load encrypted keystore with correct passphrase");
      return;
   end if;

   -- =========================================================================
   -- TEST 3: Verify loaded identity matches original
   -- =========================================================================
   Test_Header ("Verify Loaded Identity Matches Original");

   declare
      X25519_PK1 : constant X25519_Public_Key := Get_X25519_Public (Identity1);
      X25519_PK2 : constant X25519_Public_Key := Get_X25519_Public (Identity2);

      Ed25519_PK1 : constant Ed25519_Public_Key := Get_Ed25519_Public (Identity1);
      Ed25519_PK2 : constant Ed25519_Public_Key := Get_Ed25519_Public (Identity2);

      ML_KEM_PK1 : constant ML_KEM_Public_Key := Get_ML_KEM_Public (Identity1);
      ML_KEM_PK2 : constant ML_KEM_Public_Key := Get_ML_KEM_Public (Identity2);

      ML_DSA_PK1 : constant ML_DSA_Public_Key := Get_ML_DSA_Public (Identity1);
      ML_DSA_PK2 : constant ML_DSA_Public_Key := Get_ML_DSA_Public (Identity2);
   begin
      if X25519_PK1 = X25519_PK2 then
         Test_Pass ("X25519 public keys match");
      else
         Test_Fail ("X25519 public keys DO NOT match");
      end if;

      if Ed25519_PK1 = Ed25519_PK2 then
         Test_Pass ("Ed25519 public keys match");
      else
         Test_Fail ("Ed25519 public keys DO NOT match");
      end if;

      if ML_KEM_PK1 = ML_KEM_PK2 then
         Test_Pass ("ML-KEM-1024 public keys match");
      else
         Test_Fail ("ML-KEM-1024 public keys DO NOT match");
      end if;

      if ML_DSA_PK1 = ML_DSA_PK2 then
         Test_Pass ("ML-DSA-87 public keys match");
      else
         Test_Fail ("ML-DSA-87 public keys DO NOT match");
      end if;
   end;

   -- =========================================================================
   -- TEST 4: Attempt to load with wrong passphrase
   -- =========================================================================
   Test_Header ("Load Encrypted Keystore (Wrong Passphrase)");

   Test_Info ("Attempting to load with wrong passphrase: " & Wrong_Passphrase);

   declare
      Identity_Wrong : Identity_Keypair;
   begin
      Load_Identity_Encrypted (
         Filename   => Test_File,
         Passphrase => Wrong_Passphrase,
         Identity   => Identity_Wrong,
         Success    => Success
      );

      if not Success then
         Test_Pass ("Correctly rejected wrong passphrase (authentication failed)");
      else
         Test_Fail ("SECURITY VULNERABILITY: Accepted wrong passphrase!");
      end if;
   end;

   -- =========================================================================
   -- TEST 5: Round-trip test (generate, save, load multiple times)
   -- =========================================================================
   Test_Header ("Round-Trip Test (Save and Load 3 Times)");

   for I in 1 .. 3 loop
      declare
         RT_File : constant String := "/tmp/test-roundtrip-" & I'Image & ".anubisk2";
         RT_Pass : constant String := "TestPass" & I'Image & "#Quantum!";
         Identity_A : Identity_Keypair;
         Identity_B : Identity_Keypair;
         Gen_Success : Boolean;
         Save_Success : Boolean;
         Load_Success : Boolean;
      begin
         Test_Info ("Round-trip" & I'Image & "/3: " & RT_File);

         -- Generate
         Generate_Identity (Identity_A, Gen_Success);
         if not Gen_Success then
            Test_Fail ("Round-trip" & I'Image & ": Generation failed");
            goto Next_Iteration;
         end if;

         -- Save
         Save_Identity_Encrypted (
            Identity   => Identity_A,
            Filename   => RT_File,
            Passphrase => RT_Pass,
            Success    => Save_Success
         );

         if not Save_Success then
            Test_Fail ("Round-trip" & I'Image & ": Save failed");
            goto Next_Iteration;
         end if;

         -- Load
         Load_Identity_Encrypted (
            Filename   => RT_File,
            Passphrase => RT_Pass,
            Identity   => Identity_B,
            Success    => Load_Success
         );

         if not Load_Success then
            Test_Fail ("Round-trip" & I'Image & ": Load failed");
            goto Next_Iteration;
         end if;

         -- Verify
         if Get_X25519_Public (Identity_A) = Get_X25519_Public (Identity_B) then
            Test_Pass ("Round-trip" & I'Image & " succeeded");
         else
            Test_Fail ("Round-trip" & I'Image & ": Keys do not match");
         end if;

         -- Cleanup
         Zeroize_Identity (Identity_A);
         Zeroize_Identity (Identity_B);

         if Ada.Directories.Exists (RT_File) then
            Ada.Directories.Delete_File (RT_File);
         end if;

         <<Next_Iteration>>
      end;
   end loop;

   -- =========================================================================
   -- Cleanup
   -- =========================================================================
   Zeroize_Identity (Identity1);
   Zeroize_Identity (Identity2);

   if Ada.Directories.Exists (Test_File) then
      Ada.Directories.Delete_File (Test_File);
      Test_Info ("Cleaned up test files");
   end if;

   -- =========================================================================
   -- SUMMARY
   -- =========================================================================
   New_Line;
   Put_Line (Yellow & "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" & Reset);
   Put_Line (Yellow & "TEST SUMMARY: ANUBIS-SPARK v1.1.0 Encrypted Keystore" & Reset);
   Put_Line (Yellow & "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" & Reset);
   Put_Line (Green & "PASSED:" & Pass_Count'Image & Reset);
   Put_Line (Red & "FAILED:" & Fail_Count'Image & Reset);
   Put_Line ("TOTAL: " & Natural'Image (Pass_Count + Fail_Count));

   if Fail_Count > 0 then
      Put_Line (Red & "RESULT: FAILED" & Reset);
   else
      Put_Line (Green & "RESULT: ALL TESTS PASSED" & Reset);
   end if;

end Test_Encrypted_Keystore;
