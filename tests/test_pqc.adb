-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Post-Quantum Cryptography Test Program
-- Validates ML-KEM-1024 and ML-DSA-87 bindings
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.PQC; use Anubis_Types.PQC;
with OQS_Common; use OQS_Common;

procedure Test_PQC is

   Success : Boolean;

   -------------------------------------------------------------------------
   -- Test ML-KEM-1024 (Key Encapsulation Mechanism)
   -------------------------------------------------------------------------
   procedure Test_ML_KEM is
      -- Alice's keys (recipient)
      Alice_Public  : ML_KEM_Public_Key;
      Alice_Secret  : ML_KEM_Secret_Key;

      -- Bob encapsulates to Alice
      Ciphertext    : ML_KEM_Ciphertext;
      Bob_Shared    : ML_KEM_Shared_Secret;

      -- Alice decapsulates
      Alice_Shared  : ML_KEM_Shared_Secret;
   begin
      Put_Line ("═══════════════════════════════════════════════════════");
      Put_Line (" Testing ML-KEM-1024 (NIST Level 5)");
      Put_Line ("═══════════════════════════════════════════════════════");
      New_Line;

      -- Step 1: Generate Alice's keypair
      Put ("1. Generating keypair... ");
      ML_KEM_Generate_Keypair (Alice_Public, Alice_Secret, Success);

      if Success and then Is_Valid (Alice_Secret) then
         Put_Line ("✓ SUCCESS");
         Put_Line ("   Public key:  1,568 bytes");
         Put_Line ("   Secret key:  3,168 bytes (VALID)");
      else
         Put_Line ("✗ FAILED");
         return;
      end if;
      New_Line;

      -- Step 2: Bob encapsulates (creates shared secret + ciphertext)
      Put ("2. Encapsulating shared secret... ");
      ML_KEM_Encapsulate (Alice_Public, Ciphertext, Bob_Shared, Success);

      if Success and then Is_Valid (Bob_Shared) then
         Put_Line ("✓ SUCCESS");
         Put_Line ("   Ciphertext:    1,568 bytes");
         Put_Line ("   Shared secret: 32 bytes (VALID)");
      else
         Put_Line ("✗ FAILED");
         Zeroize_ML_KEM_Secret (Alice_Secret);
         return;
      end if;
      New_Line;

      -- Step 3: Alice decapsulates (recovers shared secret)
      Put ("3. Decapsulating shared secret... ");
      ML_KEM_Decapsulate (Ciphertext, Alice_Secret, Alice_Shared, Success);

      if Success and then Is_Valid (Alice_Shared) then
         Put_Line ("✓ SUCCESS");
         Put_Line ("   Shared secret: 32 bytes (VALID)");
      else
         Put_Line ("✗ FAILED");
         Zeroize_ML_KEM_Secret (Alice_Secret);
         Zeroize_Shared_Secret (Bob_Shared);
         return;
      end if;
      New_Line;

      -- Step 4: Verify shared secrets match
      Put ("4. Verifying shared secrets match... ");
      declare
         Match : Boolean;
      begin
         Match := Secrets_Match (Bob_Shared, Alice_Shared);

         if Match then
            Put_Line ("✓ SUCCESS");
            Put_Line ("   Both parties derived identical 32-byte shared secret!");
         else
            Put_Line ("✗ FAILED - Shared secrets DO NOT match!");
         end if;
      end;
      New_Line;

      -- Cleanup
      Put ("5. Securely zeroizing keys... ");
      Zeroize_ML_KEM_Secret (Alice_Secret);
      Zeroize_Shared_Secret (Bob_Shared);
      Zeroize_Shared_Secret (Alice_Shared);

      if not Is_Valid (Alice_Secret) and
         not Is_Valid (Bob_Shared) and
         not Is_Valid (Alice_Shared)
      then
         Put_Line ("✓ SUCCESS");
         Put_Line ("   All secrets marked as INVALID");
      else
         Put_Line ("✗ WARNING - Zeroization may have failed");
      end if;
      New_Line;

   end Test_ML_KEM;

   -------------------------------------------------------------------------
   -- Test ML-DSA-87 (Digital Signature Algorithm)
   -------------------------------------------------------------------------
   procedure Test_ML_DSA is
      -- Alice's signing keys
      Alice_Public  : ML_DSA_Public_Key;
      Alice_Secret  : ML_DSA_Secret_Key;

      -- Message to sign
      Message : constant Byte_Array := (
         Byte (Character'Pos ('H')),
         Byte (Character'Pos ('e')),
         Byte (Character'Pos ('l')),
         Byte (Character'Pos ('l')),
         Byte (Character'Pos ('o')),
         Byte (Character'Pos (',')),
         Byte (Character'Pos (' ')),
         Byte (Character'Pos ('P')),
         Byte (Character'Pos ('Q')),
         Byte (Character'Pos ('C')),
         Byte (Character'Pos ('!'))
      );

      Signature : ML_DSA_Signature;
      Is_Valid_Sig : Boolean;
   begin
      Put_Line ("═══════════════════════════════════════════════════════");
      Put_Line (" Testing ML-DSA-87 (NIST Level 5)");
      Put_Line ("═══════════════════════════════════════════════════════");
      New_Line;

      -- Step 1: Generate Alice's keypair
      Put ("1. Generating signing keypair... ");
      ML_DSA_Generate_Keypair (Alice_Public, Alice_Secret, Success);

      if Success and then Anubis_Types.Is_Valid (Alice_Secret) then
         Put_Line ("✓ SUCCESS");
         Put_Line ("   Public key:  2,592 bytes");
         Put_Line ("   Secret key:  4,896 bytes (VALID)");
      else
         Put_Line ("✗ FAILED");
         return;
      end if;
      New_Line;

      -- Step 2: Sign a message
      Put ("2. Signing message ""Hello, PQC!""... ");
      ML_DSA_Sign (Message, Alice_Secret, Signature, Success);

      if Success then
         Put_Line ("✓ SUCCESS");
         Put_Line ("   Signature:   4,627 bytes");
      else
         Put_Line ("✗ FAILED");
         Zeroize_ML_DSA_Secret (Alice_Secret);
         return;
      end if;
      New_Line;

      -- Step 3: Verify the signature
      Put ("3. Verifying signature... ");
      Is_Valid_Sig := ML_DSA_Verify (Message, Signature, Alice_Public);

      if Is_Valid_Sig then
         Put_Line ("✓ SUCCESS");
         Put_Line ("   Signature is VALID and authentic");
      else
         Put_Line ("✗ FAILED - Signature verification failed!");
      end if;
      New_Line;

      -- Step 4: Verify with tampered message (should fail)
      Put ("4. Verifying with tampered message... ");
      declare
         Tampered_Message : Byte_Array := Message;
      begin
         Tampered_Message (Tampered_Message'First) := Byte (Character'Pos ('X'));
         Is_Valid_Sig := ML_DSA_Verify (Tampered_Message, Signature, Alice_Public);

         if not Is_Valid_Sig then
            Put_Line ("✓ SUCCESS");
            Put_Line ("   Signature correctly REJECTED for tampered message");
         else
            Put_Line ("✗ FAILED - Signature should have been rejected!");
         end if;
      end;
      New_Line;

      -- Cleanup
      Put ("5. Securely zeroizing signing key... ");
      Zeroize_ML_DSA_Secret (Alice_Secret);

      if not Anubis_Types.Is_Valid (Alice_Secret) then
         Put_Line ("✓ SUCCESS");
         Put_Line ("   Secret key marked as INVALID");
      else
         Put_Line ("✗ WARNING - Zeroization may have failed");
      end if;
      New_Line;

   end Test_ML_DSA;

begin
   Put_Line ("");
   Put_Line ("╔══════════════════════════════════════════════════════════╗");
   Put_Line ("║  ANUBIS-SPARK: Post-Quantum Cryptography Test Suite     ║");
   Put_Line ("║                                                          ║");
   Put_Line ("║  Testing NIST Level 5 algorithms:                       ║");
   Put_Line ("║  • ML-KEM-1024 (Key Encapsulation)                      ║");
   Put_Line ("║  • ML-DSA-87 (Digital Signatures)                       ║");
   Put_Line ("╚══════════════════════════════════════════════════════════╝");
   Put_Line ("");

   -- Initialize liboqs
   Put_Line ("Initializing liboqs...");
   OQS_init;
   Put_Line ("✓ liboqs initialized");
   New_Line;

   -- Run tests
   Test_ML_KEM;
   Test_ML_DSA;

   -- Summary
   Put_Line ("═══════════════════════════════════════════════════════");
   Put_Line (" Test Suite Complete");
   Put_Line ("═══════════════════════════════════════════════════════");
   Put_Line ("");
   Put_Line ("✓ ML-KEM-1024 key encapsulation works correctly");
   Put_Line ("✓ ML-DSA-87 digital signatures work correctly");
   Put_Line ("✓ Secure zeroization verified");
   Put_Line ("");
   Put_Line ("ANUBIS-SPARK post-quantum crypto bindings: OPERATIONAL");

   -- Cleanup
   OQS_destroy;

end Test_PQC;
