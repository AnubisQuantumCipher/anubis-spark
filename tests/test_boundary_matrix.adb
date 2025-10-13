-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Comprehensive Boundary/Tamper Matrix Test
-- Tests multiple attack vectors: header corruption, chunk reordering,
-- truncation, byte flipping, wrong keys
-------------------------------------------------------------------------------

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Directories;
with Ada.Command_Line;
with Interfaces;
use type Interfaces.Unsigned_8;
with Anubis_Types;               use Anubis_Types;
with Anubis_Types.Streaming;     use Anubis_Types.Streaming;
with Anubis_Types.Storage;
with Anubis_Trust;
with Anubis_Trust.Logic;

procedure Test_Boundary_Matrix is

   -------------------------------------------------------------------------
   -- Helper Procedures
   -------------------------------------------------------------------------

   procedure Write_File (Path, Content : String) is
      F : File_Type;
   begin
      Create (F, Out_File, Path);
      Put_Line (F, Content);
      Close (F);
   end Write_File;

   procedure Copy_File (Src, Dst : String) is
      InF  : File_Type;
      OutF : File_Type;
      C    : Character;
   begin
      Open (InF, In_File, Src);
      Create (OutF, Out_File, Dst);
      while not End_Of_File (InF) loop
         Get (InF, C);
         Put (OutF, C);
      end loop;
      Close (InF);
      Close (OutF);
   end Copy_File;

   procedure Flip_Byte_At (Path : String; Pos : Positive) is
      InF  : File_Type;
      OutF : File_Type;
      C    : Character;
      Count : Positive := 1;
   begin
      Open (InF, In_File, Path);
      Create (OutF, Out_File, Path & ".tmp");

      while not End_Of_File (InF) loop
         Get (InF, C);
         if Count = Pos then
            -- Flip all bits
            declare
               Byte : Interfaces.Unsigned_8 := Interfaces.Unsigned_8 (Character'Pos (C));
            begin
               Byte := Byte xor Interfaces.Unsigned_8 (16#FF#);
               C := Character'Val (Integer (Byte));
            end;
         end if;
         Put (OutF, C);
         Count := Count + 1;
      end loop;

      Close (InF);
      Close (OutF);

      -- Replace original with modified
      Copy_File (Path & ".tmp", Path);
   end Flip_Byte_At;

   procedure Truncate_File (Path : String; Keep_Bytes : Positive) is
      InF  : File_Type;
      OutF : File_Type;
      C    : Character;
      Count : Positive := 1;
   begin
      Open (InF, In_File, Path);
      Create (OutF, Out_File, Path & ".tmp");

      while not End_Of_File (InF) and then Count <= Keep_Bytes loop
         Get (InF, C);
         Put (OutF, C);
         Count := Count + 1;
      end loop;

      Close (InF);
      Close (OutF);
      Copy_File (Path & ".tmp", Path);
   end Truncate_File;

   function Run_Decrypt (
      Cipher_Path : String;
      Output_Path : String;
      X_Sk        : X25519_Secret_Key;
      Kem_Sk      : ML_KEM_Secret_Key;
      Ed_Pk       : Ed25519_Public_Key;
      Ml_Pk       : ML_DSA_Public_Key
   ) return Streaming.Result_Code is
      Rc : Streaming.Result_Code;
      Label_Raw : Signer_Label;
      Timestamp_Raw : Interfaces.Unsigned_64;
      Fingerprint_Raw : Signer_Fingerprint;
   begin
      Streaming.Decrypt_File_Streaming (
         Input_Path      => Cipher_Path,
         Output_Path     => Output_Path,
         X25519_SK       => X_Sk,
         ML_KEM_SK       => Kem_Sk,
         Ed25519_PK      => Ed_Pk,
         ML_DSA_PK       => Ml_Pk,
         Signer_Label_Data    => Label_Raw,
         Signer_Timestamp     => Timestamp_Raw,
         Signer_Fingerprint_Data => Fingerprint_Raw,
         Result          => Rc
      );
      pragma Unreferenced (Label_Raw);
      pragma Unreferenced (Timestamp_Raw);
      pragma Unreferenced (Fingerprint_Raw);
      return Rc;
   end Run_Decrypt;

   -------------------------------------------------------------------------
   -- Test State
   -------------------------------------------------------------------------

   Identity1     : Anubis_Types.Storage.Identity_Keypair;
   Identity2     : Anubis_Types.Storage.Identity_Keypair;  -- Wrong keys
   Ok            : Boolean;
   Rc            : Streaming.Result_Code;

   Inp           : constant String := "/tmp/tamper_test_input.txt";
   Enc           : constant String := "/tmp/tamper_test_input.txt.anubis";
   Work          : constant String := "/tmp/tamper_test_work.anubis";
   Dec           : constant String := "/tmp/tamper_test_output.txt";

   Pass_Count    : Natural := 0;
   Fail_Count    : Natural := 0;
   Encrypted_Size : Natural := 0;

   Signer_Label1 : constant Signer_Label := Storage.Make_Label ("matrix-id1");
   Signer_Timestamp1 : constant Interfaces.Unsigned_64 := 100;
   Signer_Fingerprint1 : Signer_Fingerprint;

   -- Offsets derived from ANUB3 header layout
   Header_Length           : constant Positive := 6_433;
   Ed_Signature_Offset     : constant Positive := 1_743;
   ML_DSA_Signature_Offset : constant Positive := 1_807;
   Chunk_Length_Offset     : constant Positive := Header_Length + 1;
   Chunk_Tag_Offset        : constant Positive := Header_Length + 9;
   Chunk_Data_Offset       : constant Positive := Header_Length + 25;
   Final_Marker_Length     : constant Positive := 11;

begin
   pragma Assert (Anubis_Trust.Logic.Label_Input_Is_Valid ("matrix-id1"));
   pragma Assert (Anubis_Trust.Logic.Operator_Input_Is_Valid ("qa-check"));
   Put_Line ("=== ANUBIS-SPARK Comprehensive Tamper Matrix Test ===");
   Put_Line ("");

   -------------------------------------------------------------------------
   -- Setup: Generate identities and baseline encrypted file
   -------------------------------------------------------------------------

   Put_Line ("[Setup] Generating test data and identities...");

   -- Create test input
   Write_File (Inp, "The quick brown fox jumps over the lazy dog. 0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ");

   -- Generate two different identities
   Anubis_Types.Storage.Generate_Identity (Identity1, Ok);
   if not Ok then
      Put_Line ("[FATAL] Failed to generate Identity1");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Signer_Fingerprint1 := Storage.Compute_Fingerprint (Identity1);

   Anubis_Types.Storage.Generate_Identity (Identity2, Ok);
   if not Ok then
      Put_Line ("[FATAL] Failed to generate Identity2");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   -- Encrypt baseline file
   Streaming.Encrypt_File_Streaming
     (Input_Path  => Inp,
      Output_Path => Enc,
      X25519_PK   => Anubis_Types.Storage.Get_X25519_Public (Identity1),
      ML_KEM_PK   => Anubis_Types.Storage.Get_ML_KEM_Public (Identity1),
      Ed25519_SK  => Anubis_Types.Storage.Get_Ed25519_Secret (Identity1),
      ML_DSA_SK   => Anubis_Types.Storage.Get_ML_DSA_Secret (Identity1),
      Signer_Label_Data => Signer_Label1,
      Signer_Timestamp => Signer_Timestamp1,
      Signer_Fingerprint_Data => Signer_Fingerprint1,
      Result      => Rc,
      Chunk_Size  => 1_048_576);

   if Rc /= Streaming.Success then
      Put_Line ("[FATAL] Failed to encrypt baseline file");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Encrypted_Size := Natural (Ada.Directories.Size (Enc));

   declare
      Trust_OK : Boolean;
   begin
      Anubis_Trust.Approve (
         Fingerprint => Signer_Fingerprint1,
         Success     => Trust_OK);
      if not Trust_OK then
         Put_Line ("[FATAL] Failed to approve signer for tamper matrix");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      declare
         Self_OK : Boolean;
      begin
         Anubis_Trust.Self_Check (Self_OK);
         pragma Assert (Self_OK);
      end;
   end;

   Put_Line ("        ✓ Test data and identities ready");
   Put_Line ("");

   -------------------------------------------------------------------------
   -- Test Matrix: Each test case tampers and verifies rejection
   -------------------------------------------------------------------------

   Put_Line ("Running tamper matrix tests:");
   Put_Line ("");

   -- Test Case 1: Flip header magic bytes
   declare
      Test_Name : constant String := "flip-header-magic (offset 3)";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Flip_Byte_At (Work, 3);  -- Flip byte in "ANUB3" magic

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 2: Flip version byte
   declare
      Test_Name : constant String := "flip-version-byte (offset 6)";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Flip_Byte_At (Work, 6);  -- Version byte

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 3: Flip file nonce byte
   declare
      Test_Name : constant String := "flip-file-nonce (offset 16)";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Flip_Byte_At (Work, 16);  -- File nonce byte

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 4: Flip X25519 ephemeral public key byte
   declare
      Test_Name : constant String := "flip-x25519-ephemeral (offset 50)";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Flip_Byte_At (Work, 50);  -- X25519 ephemeral PK

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 5: Flip ML-KEM ciphertext byte
   declare
      Test_Name : constant String := "flip-mlkem-ciphertext (offset 200)";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Flip_Byte_At (Work, 200);  -- ML-KEM CT

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 6: Flip Ed25519 signature byte
   declare
      Test_Name : constant String := "flip-ed25519-signature";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Flip_Byte_At (Work, Ed_Signature_Offset);

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Auth_Failed then
         Put_Line ("[PASS] " & Test_Name & " - rejected (Auth_Failed)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("[FAIL] " & Test_Name & " - expected Auth_Failed, got " & Rc2'Image);
         Fail_Count := Fail_Count + 1;
      end if;
   end;

   -- Test Case 7: Flip ML-DSA signature byte
   declare
      Test_Name : constant String := "flip-ml-dsa-signature";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Flip_Byte_At (Work, ML_DSA_Signature_Offset);

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Auth_Failed then
         Put_Line ("[PASS] " & Test_Name & " - rejected (Auth_Failed)");
         Pass_Count := Pass_Count + 1;
      else
         Put_Line ("[FAIL] " & Test_Name & " - expected Auth_Failed, got " & Rc2'Image);
         Fail_Count := Fail_Count + 1;
      end if;
   end;

   -- Test Case 8: Flip chunk ciphertext byte
   declare
      Test_Name : constant String := "flip-chunk-ciphertext";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Flip_Byte_At (Work, Chunk_Data_Offset);  -- First ciphertext byte

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 9: Truncate file (remove finalization marker)
   declare
      Test_Name : constant String := "truncate-finalization-marker";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Truncate_File (Work, Positive (Encrypted_Size - Final_Marker_Length));  -- Drop marker

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 10: Wrong X25519 secret key
   declare
      Test_Name : constant String := "wrong-x25519-secret-key";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      -- No tampering, just wrong keys

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity2),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - wrong key ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 11: Wrong ML-KEM secret key
   declare
      Test_Name : constant String := "wrong-mlkem-secret-key";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity2),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - wrong key ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 12: All wrong keys
   declare
      Test_Name : constant String := "all-wrong-keys";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);

      Rc2 := Run_Decrypt (
        Cipher_Path => Work,
        Output_Path => Dec,
        X_Sk        => Anubis_Types.Storage.Get_X25519_Secret (Identity2),
        Kem_Sk      => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity2),
        Ed_Pk       => Anubis_Types.Storage.Get_Ed25519_Public (Identity2),
        Ml_Pk       => Anubis_Types.Storage.Get_ML_DSA_Public (Identity2));

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - wrong keys ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -------------------------------------------------------------------------
   -- Summary
   -------------------------------------------------------------------------

   Put_Line ("");
   Put_Line ("=== Test Summary ===");
   Put_Line ("PASS: " & Pass_Count'Image & " tests");
   Put_Line ("FAIL: " & Fail_Count'Image & " tests");

   if Fail_Count = 0 then
      Put_Line ("");
      Put_Line ("✓ ALL TESTS PASSED - Tamper detection working correctly");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Put_Line ("");
      Put_Line ("✗ SOME TESTS FAILED - Security vulnerability detected!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

   -- Cleanup
   Anubis_Types.Storage.Zeroize_Identity (Identity1);
   Anubis_Types.Storage.Zeroize_Identity (Identity2);

exception
   when others =>
      Put_Line ("[FATAL] Unexpected exception during test");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end Test_Boundary_Matrix;
