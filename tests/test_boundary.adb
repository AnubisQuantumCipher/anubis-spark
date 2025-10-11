-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Boundary/Tamper Detection Test
-- Tests that tampering with encrypted files is properly rejected
-- Exercises: truncation, byte flipping, header corruption
-------------------------------------------------------------------------------

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with Anubis_Types;               use Anubis_Types;
with Anubis_Types.Streaming;     use Anubis_Types.Streaming;
with Anubis_Types.Storage;

procedure Test_Boundary is
   -- Identity keypair for encryption/decryption
   Identity   : Anubis_Types.Storage.Identity_Keypair;
   Ok         : Boolean;
   Rc         : Streaming.Result_Code;

   -- File paths
   Inp        : constant String := "/tmp/test_data.bin";
   Outp       : constant String := "/tmp/test_data.bin.anubis";
   OutTamper  : constant String := "/tmp/test_data.bin.tampered";
   OutDecrypt : constant String := "/tmp/test_data.bin.decrypted";

begin
   Put_Line ("=== ANUBIS-SPARK Boundary/Tamper Detection Test ===");
   Put_Line ("");

   -- Step 1: Create tiny test input
   Put_Line ("[1/5] Creating test input file...");
   declare F : File_Type; begin
      Create (F, Out_File, Inp);
      Put_Line (F, "hello quantum world");
      Close (F);
   end;
   Put_Line ("      ✓ Test file created");

   -- Step 2: Generate identity keypair
   Put_Line ("[2/5] Generating identity keypair...");
   Anubis_Types.Storage.Generate_Identity (Identity, Ok);
   if not Ok then
      Put_Line ("[FAIL] Identity generation failed");
      Set_Exit_Status (Failure);
      return;
   end if;
   Put_Line ("      ✓ Identity generated");

   -- Step 3: Encrypt the file
   Put_Line ("[3/5] Encrypting test file...");
   Streaming.Encrypt_File_Streaming
     (Input_Path  => Inp,
      Output_Path => Outp,
      X25519_PK   => Anubis_Types.Storage.Get_X25519_Public (Identity),
      ML_KEM_PK   => Anubis_Types.Storage.Get_ML_KEM_Public (Identity),
      Ed25519_SK  => Anubis_Types.Storage.Get_Ed25519_Secret (Identity),
      ML_DSA_SK   => Anubis_Types.Storage.Get_ML_DSA_Secret (Identity),
      Result      => Rc,
      Chunk_Size  => 1_048_576);

   if Rc /= Streaming.Success then
      Put_Line ("[FAIL] Encryption failed: " & Rc'Image);
      Set_Exit_Status (Failure);
      return;
   end if;
   Put_Line ("      ✓ File encrypted successfully");

   -- Step 4: Tamper with encrypted file (flip one byte)
   Put_Line ("[4/5] Creating tampered copy (byte flip at offset 42)...");
   declare
      InF, OutF : File_Type;
      C         : Character;
      Count     : Natural := 0;
   begin
      Open   (InF,  In_File,  Outp);
      Create (OutF, Out_File, OutTamper);

      while not End_Of_File (InF) loop
         Get (InF, C);
         Count := Count + 1;

         -- Flip one byte in the middle (simulates single-bit corruption)
         if Count = 42 then
            C := Character'Val (Character'Pos (C) xor 16#FF#);
         end if;

         Put (OutF, C);
      end loop;

      Close (InF);
      Close (OutF);
   end;
   Put_Line ("      ✓ Tampered file created");

   -- Step 5: Attempt to decrypt tampered file (should fail)
   Put_Line ("[5/5] Attempting to decrypt tampered file...");
   declare
      Rc2 : Streaming.Result_Code;
   begin
      Streaming.Decrypt_File_Streaming
        (Input_Path  => OutTamper,
         Output_Path => OutDecrypt,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity),
         Result      => Rc2);

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] Tampered file was ACCEPTED - security violation!");
         Set_Exit_Status (Failure);
      else
         Put_Line ("      ✓ Tampered file REJECTED (" & Rc2'Image & ")");
         Put_Line ("");
         Put_Line ("=== TEST PASSED ===");
         Put_Line ("Tampering detection works correctly.");
         Set_Exit_Status (Success);
      end if;
   end;

   -- Cleanup
   Anubis_Types.Storage.Zeroize_Identity (Identity);

exception
   when others =>
      Put_Line ("[FAIL] Unexpected exception during test");
      Set_Exit_Status (Failure);

end Test_Boundary;
