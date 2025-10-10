-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Test Hybrid Signatures
-- Tests Ed25519 + ML-DSA-87 hybrid signature operations
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- Test code doesn't need SPARK verification

with Ada.Text_IO; use Ada.Text_IO;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.Classical;
with Anubis_Types.PQC;

procedure Test_Hybrid_Sign is
   -- Test message
   Test_Message : constant Byte_Array := (
      72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33  -- "Hello, World!"
   );

   -- Classical keys (Ed25519)
   Ed25519_Public  : Ed25519_Public_Key;
   Ed25519_Secret  : Ed25519_Secret_Key;

   -- Post-quantum keys (ML-DSA-87)
   ML_DSA_Public   : ML_DSA_Public_Key;
   ML_DSA_Secret   : ML_DSA_Secret_Key;

   -- Hybrid signature
   Signature       : PQC.Hybrid_Signature;

   -- Success flags
   Success         : Boolean;
   Verify_Result   : Boolean;

begin
   Put_Line ("=============================================================");
   Put_Line ("ANUBIS-SPARK: Hybrid Signature Test (Ed25519 + ML-DSA-87)");
   Put_Line ("PLATINUM LEVEL: Dual signatures for quantum resistance");
   Put_Line ("=============================================================");
   New_Line;

   -- Step 1: Generate Ed25519 keypair
   Put_Line ("1. Generating Ed25519 keypair...");
   Classical.Ed25519_Generate_Keypair (
      Public_Key  => Ed25519_Public,
      Secret_Key  => Ed25519_Secret,
      Success     => Success
   );

   if not Success then
      Put_Line ("   [FAIL] Ed25519 keypair generation failed");
      return;
   end if;
   Put_Line ("   [OK] Ed25519 keypair generated");
   New_Line;

   -- Step 2: Generate ML-DSA-87 keypair
   Put_Line ("2. Generating ML-DSA-87 keypair...");
   PQC.ML_DSA_Generate_Keypair (
      Public_Key  => ML_DSA_Public,
      Secret_Key  => ML_DSA_Secret,
      Success     => Success
   );

   if not Success then
      Put_Line ("   [FAIL] ML-DSA-87 keypair generation failed");
      Classical.Zeroize_Ed25519_Secret (Ed25519_Secret);
      return;
   end if;
   Put_Line ("   [OK] ML-DSA-87 keypair generated");
   New_Line;

   -- Step 3: Sign message with hybrid signature
   Put_Line ("3. Creating hybrid signature...");
   Put_Line ("   Message: ""Hello, World!""");
   PQC.Hybrid_Sign (
      Message     => Test_Message,
      Ed25519_SK  => Ed25519_Secret,
      ML_DSA_SK   => ML_DSA_Secret,
      Signature   => Signature,
      Success     => Success
   );

   if not Success then
      Put_Line ("   [FAIL] Hybrid signature creation failed");
      Classical.Zeroize_Ed25519_Secret (Ed25519_Secret);
      PQC.Zeroize_ML_DSA_Secret (ML_DSA_Secret);
      return;
   end if;
   Put_Line ("   [OK] Hybrid signature created");
   Put_Line ("   - Ed25519 signature: 64 bytes");
   Put_Line ("   - ML-DSA-87 signature: 4627 bytes");
   Put_Line ("   - Total: 4691 bytes");
   New_Line;

   -- Step 4: Verify hybrid signature
   Put_Line ("4. Verifying hybrid signature...");
   Verify_Result := PQC.Hybrid_Verify (
      Message     => Test_Message,
      Signature   => Signature,
      Ed25519_PK  => Ed25519_Public,
      ML_DSA_PK   => ML_DSA_Public
   );

   if not Verify_Result then
      Put_Line ("   [FAIL] Hybrid signature verification failed");
      Classical.Zeroize_Ed25519_Secret (Ed25519_Secret);
      PQC.Zeroize_ML_DSA_Secret (ML_DSA_Secret);
      return;
   end if;
   Put_Line ("   [OK] Hybrid signature verified successfully");
   Put_Line ("   - Both Ed25519 AND ML-DSA-87 verified");
   New_Line;

   -- Step 5: Test invalid signature (tamper detection)
   Put_Line ("5. Testing tamper detection...");
   declare
      Tampered_Message : Byte_Array := Test_Message;
   begin
      -- Tamper with the message
      Tampered_Message (1) := Tampered_Message (1) + 1;

      Verify_Result := PQC.Hybrid_Verify (
         Message     => Tampered_Message,
         Signature   => Signature,
         Ed25519_PK  => Ed25519_Public,
         ML_DSA_PK   => ML_DSA_Public
      );

      if Verify_Result then
         Put_Line ("   [FAIL] Tampered message was accepted!");
         Classical.Zeroize_Ed25519_Secret (Ed25519_Secret);
         PQC.Zeroize_ML_DSA_Secret (ML_DSA_Secret);
         return;
      end if;
      Put_Line ("   [OK] Tampered message correctly rejected");
   end;
   New_Line;

   -- Step 6: Secure cleanup
   Put_Line ("6. Secure cleanup...");
   Classical.Zeroize_Ed25519_Secret (Ed25519_Secret);
   PQC.Zeroize_ML_DSA_Secret (ML_DSA_Secret);
   Put_Line ("   [OK] Secret keys zeroized");
   New_Line;

   -- Summary
   Put_Line ("=============================================================");
   Put_Line ("PLATINUM LEVEL HYBRID SIGNATURE TEST: PASSED");
   Put_Line ("=============================================================");
   Put_Line ("Security Properties:");
   Put_Line ("  - Quantum Resistant: ML-DSA-87 (NIST Level 5)");
   Put_Line ("  - Classical Security: Ed25519 (256-bit)");
   Put_Line ("  - Defense in Depth: BOTH must be broken");
   Put_Line ("  - Tamper Detection: Verified");
   Put_Line ("  - Memory Safety: SPARK-proven zeroization");
   Put_Line ("=============================================================");

end Test_Hybrid_Sign;
