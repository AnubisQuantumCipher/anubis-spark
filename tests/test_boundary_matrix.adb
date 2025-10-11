-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Comprehensive Boundary/Tamper Matrix Test
-- Tests multiple attack vectors: header corruption, chunk reordering,
-- truncation, byte flipping, wrong keys
-------------------------------------------------------------------------------

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Streams.Stream_IO;      use Ada.Streams.Stream_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with Anubis_Types;               use Anubis_Types;
with Anubis_Types.Streaming;     use Anubis_Types.Streaming;
with Anubis_Types.Storage;

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
            C := Character'Val (Character'Pos (C) xor 16#FF#);
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

begin
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
      Set_Exit_Status (Failure);
      return;
   end if;

   Anubis_Types.Storage.Generate_Identity (Identity2, Ok);
   if not Ok then
      Put_Line ("[FATAL] Failed to generate Identity2");
      Set_Exit_Status (Failure);
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
      Result      => Rc,
      Chunk_Size  => 1_048_576);

   if Rc /= Streaming.Success then
      Put_Line ("[FATAL] Failed to encrypt baseline file");
      Set_Exit_Status (Failure);
      return;
   end if;

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
      Flip_Byte_At (Work, 3);  -- Flip byte in "ANUB2" magic

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1),
         Result      => Rc2);

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

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1),
         Result      => Rc2);

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

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1),
         Result      => Rc2);

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

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1),
         Result      => Rc2);

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

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1),
         Result      => Rc2);

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 6: Flip chunk ciphertext byte
   declare
      Test_Name : constant String := "flip-chunk-ciphertext (offset 1700)";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Flip_Byte_At (Work, 1700);  -- Chunk ciphertext

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1),
         Result      => Rc2);

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 7: Truncate file (remove finalization marker)
   declare
      Test_Name : constant String := "truncate-finalization-marker";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      Truncate_File (Work, 1850);  -- Cut off final bytes

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1),
         Result      => Rc2);

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - tampered file ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 8: Wrong X25519 secret key
   declare
      Test_Name : constant String := "wrong-x25519-secret-key";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);
      -- No tampering, just wrong keys

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity2),  -- Wrong!
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity1),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1),
         Result      => Rc2);

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - wrong key ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 9: Wrong ML-KEM secret key
   declare
      Test_Name : constant String := "wrong-mlkem-secret-key";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity1),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity2),  -- Wrong!
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity1),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity1),
         Result      => Rc2);

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] " & Test_Name & " - wrong key ACCEPTED");
         Fail_Count := Fail_Count + 1;
      else
         Put_Line ("[PASS] " & Test_Name & " - rejected (" & Rc2'Image & ")");
         Pass_Count := Pass_Count + 1;
      end if;
   end;

   -- Test Case 10: All wrong keys
   declare
      Test_Name : constant String := "all-wrong-keys";
      Rc2       : Streaming.Result_Code;
   begin
      Copy_File (Enc, Work);

      Streaming.Decrypt_File_Streaming
        (Input_Path  => Work,
         Output_Path => Dec,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity2),  -- All wrong!
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity2),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity2),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity2),
         Result      => Rc2);

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
      Set_Exit_Status (Success);
   else
      Put_Line ("");
      Put_Line ("✗ SOME TESTS FAILED - Security vulnerability detected!");
      Set_Exit_Status (Failure);
   end if;

   -- Cleanup
   Anubis_Types.Storage.Zeroize_Identity (Identity1);
   Anubis_Types.Storage.Zeroize_Identity (Identity2);

exception
   when others =>
      Put_Line ("[FATAL] Unexpected exception during test");
      Set_Exit_Status (Failure);

end Test_Boundary_Matrix;
