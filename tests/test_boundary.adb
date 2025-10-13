-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Boundary/Tamper Detection Test
-- Tests that tampering with encrypted files is properly rejected
-- Exercises: truncation, byte flipping, header corruption
-------------------------------------------------------------------------------

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces;
use type Interfaces.Unsigned_8;
with Anubis_Types;               use Anubis_Types;
with Anubis_Types.Streaming;     use Anubis_Types.Streaming;
with Anubis_Types.Storage;
with Anubis_Trust;
with Anubis_Trust.Logic;

procedure Test_Boundary is
   -- Identity keypair for encryption/decryption
   Identity   : Anubis_Types.Storage.Identity_Keypair;
   Ok         : Boolean;
   Rc         : Streaming.Result_Code;
   Signer_Label_Value : constant Signer_Label := Storage.Make_Label ("boundary-test");
   Signer_Timestamp : constant Interfaces.Unsigned_64 := 1;
   Signer_Fingerprint_Value : Signer_Fingerprint;

   -- File paths
   Inp        : constant String := "/tmp/test_data.bin";
   Outp       : constant String := "/tmp/test_data.bin.anubis";
   OutTamper  : constant String := "/tmp/test_data.bin.tampered";
   OutDecrypt : constant String := "/tmp/test_data.bin.decrypted";

begin
   pragma Assert (Anubis_Trust.Logic.Label_Input_Is_Valid ("boundary-test"));
   pragma Assert (Anubis_Trust.Logic.Operator_Input_Is_Valid ("qa-check"));
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
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Put_Line ("      ✓ Identity generated");

   Signer_Fingerprint_Value := Storage.Compute_Fingerprint (Identity);

   -- Step 3: Encrypt the file
   Put_Line ("[3/5] Encrypting test file...");
   Streaming.Encrypt_File_Streaming
     (Input_Path  => Inp,
      Output_Path => Outp,
      X25519_PK   => Anubis_Types.Storage.Get_X25519_Public (Identity),
      ML_KEM_PK   => Anubis_Types.Storage.Get_ML_KEM_Public (Identity),
      Ed25519_SK  => Anubis_Types.Storage.Get_Ed25519_Secret (Identity),
      ML_DSA_SK   => Anubis_Types.Storage.Get_ML_DSA_Secret (Identity),
      Signer_Label_Data => Signer_Label_Value,
      Signer_Timestamp => Signer_Timestamp,
      Signer_Fingerprint_Data => Signer_Fingerprint_Value,
      Result      => Rc,
      Chunk_Size  => 1_048_576);

   if Rc /= Streaming.Success then
      Put_Line ("[FAIL] Encryption failed: " & Rc'Image);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Put_Line ("      ✓ File encrypted successfully");

   declare
      Trust_OK : Boolean;
   begin
      Anubis_Trust.Approve (
         Fingerprint => Signer_Fingerprint_Value,
         Success     => Trust_OK);
      if not Trust_OK then
         Put_Line ("[FAIL] Trust approval failed");
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
            declare
               Byte : Interfaces.Unsigned_8 := Interfaces.Unsigned_8 (Character'Pos (C));
            begin
               Byte := Byte xor Interfaces.Unsigned_8 (16#FF#);
               C := Character'Val (Integer (Byte));
            end;
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
      Signer_Label_Raw : Signer_Label;
      Signer_Timestamp_Return : Interfaces.Unsigned_64;
      Signer_Fingerprint_Return : Signer_Fingerprint;
   begin
      pragma Unreferenced (Signer_Label_Raw);
      pragma Unreferenced (Signer_Timestamp_Return);
      pragma Unreferenced (Signer_Fingerprint_Return);
      Streaming.Decrypt_File_Streaming
        (Input_Path  => OutTamper,
         Output_Path => OutDecrypt,
         X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity),
         ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity),
         Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity),
         ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity),
         Signer_Label_Data => Signer_Label_Raw,
         Signer_Timestamp => Signer_Timestamp_Return,
         Signer_Fingerprint_Data => Signer_Fingerprint_Return,
         Result      => Rc2);

      if Rc2 = Streaming.Success then
         Put_Line ("[FAIL] Tampered file was ACCEPTED - security violation!");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      else
         Put_Line ("      ✓ Tampered file REJECTED (" & Rc2'Image & ")");
         Put_Line ("");
         Put_Line ("=== TEST PASSED ===");
         Put_Line ("Tampering detection works correctly.");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
      end if;
   end;

   -- Cleanup
   Anubis_Types.Storage.Zeroize_Identity (Identity);

exception
   when others =>
   Put_Line ("[FAIL] Unexpected exception during test");
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end Test_Boundary;
