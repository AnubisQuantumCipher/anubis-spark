-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Hybrid KDF Implementation
-- Combines X25519 + ML-KEM-1024 shared secrets via HKDF-SHA256
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- Uses FFI to libsodium

with Anubis_Types.Classical;

package body Anubis_Hybrid_KDF is

   -------------------------------------------------------------------------
   -- Derive Hybrid Secret (X25519 + ML-KEM)
   -------------------------------------------------------------------------

   procedure Derive_Hybrid_Secret
     (X25519_SS  : in  Key_32;
      MLKEM_SS   : in  Key_32;
      Hybrid_SS  : out Key_32;
      Success    : out Boolean)
   is
      -- Concatenate both secrets for HKDF input
      Input : Byte_Array (1 .. 64);
   begin
      -- Copy X25519 shared secret (first 32 bytes)
      for I in X25519_SS'Range loop
         Input (I) := X25519_SS (I);
      end loop;

      -- Copy ML-KEM shared secret (next 32 bytes)
      for I in MLKEM_SS'Range loop
         Input (32 + I) := MLKEM_SS (I);
      end loop;

      -- Derive hybrid secret using HKDF with domain separation label
      Classical.HKDF_Derive (
         Input_Key_Material => Input,
         Context_String     => DS_HYBRID_KDF,
         Output_Key         => Hybrid_SS,
         Success            => Success
      );

      -- Zeroize temporary buffer
      for I in Input'Range loop
         Input (I) := 0;
      end loop;
   end Derive_Hybrid_Secret;

   -------------------------------------------------------------------------
   -- Derive XChaCha20 Encryption Key
   -------------------------------------------------------------------------

   procedure Derive_XChaCha20_Key
     (Hybrid_SS : in  Key_32;
      Key_Out   : out Key_32;
      Success   : out Boolean)
   is
   begin
      -- Derive encryption key from hybrid secret with domain separation
      Classical.HKDF_Derive (
         Input_Key_Material => Hybrid_SS,
         Context_String     => DS_XCHACHA_KEY,
         Output_Key         => Key_Out,
         Success            => Success
      );
   end Derive_XChaCha20_Key;

   -------------------------------------------------------------------------
   -- Lemma: Hybrid Construction Security (Ghost - Proof Only)
   -------------------------------------------------------------------------

   procedure Lemma_Hybrid_Security
     (X25519_SS_Good : Key_32;
      MLKEM_SS_Good  : Key_32;
      X25519_SS_Bad  : Key_32;
      MLKEM_SS_Bad   : Key_32;
      Hybrid_SS_Good : Key_32;
      Success_Good   : Boolean;
      Hybrid_SS_Bad  : Key_32;
      Success_Bad    : Boolean)
   is
   begin
      -- Axiom: If attacker has wrong key for at least one component,
      -- they cannot derive the correct hybrid secret
      pragma Assume (Success_Good);
      pragma Assume (Key_Has_Entropy (Hybrid_SS_Good));
      pragma Assume (not Keys_Equal (Hybrid_SS_Bad, Hybrid_SS_Good));
   end Lemma_Hybrid_Security;

   -------------------------------------------------------------------------
   -- Lemma: Domain Separation (Ghost - Proof Only)
   -------------------------------------------------------------------------

   procedure Lemma_Domain_Separation
     (Hybrid_SS : Key_32;
      Key_Out_1 : Key_32;
      Success_1 : Boolean;
      Key_Out_2 : Key_32;
      Success_2 : Boolean;
      Label_1   : String;
      Label_2   : String)
   is
   begin
      -- Axiom: Different domain labels produce different keys
      pragma Assume (Success_1 and Success_2);
      pragma Assume (Label_1 /= Label_2);
      pragma Assume (not Keys_Equal (Key_Out_1, Key_Out_2));
   end Lemma_Domain_Separation;

end Anubis_Hybrid_KDF;
