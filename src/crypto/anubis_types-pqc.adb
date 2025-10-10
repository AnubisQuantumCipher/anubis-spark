-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Post-Quantum Cryptography Wrapper Implementation
-- Bridges SPARK-safe interface with liboqs C library
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- C FFI not provable in SPARK

with Interfaces.C; use Interfaces.C;
with System;
with OQS_Common; use OQS_Common;
with OQS_KEM_ML_KEM;
with OQS_SIG_ML_DSA;

package body Anubis_Types.PQC is

   -------------------------------------------------------------------------
   -- Hybrid Shared Secret Validation
   -------------------------------------------------------------------------

   function Is_Valid (Secret : Hybrid_Shared_Secret) return Boolean is
   begin
      return Secret.Valid;
   end Is_Valid;

   -------------------------------------------------------------------------
   -- ML-KEM-1024 Implementation
   -------------------------------------------------------------------------

   procedure ML_KEM_Generate_Keypair (
      Public_Key  : out ML_KEM_Public_Key;
      Secret_Key  : out ML_KEM_Secret_Key;
      Success     : out Boolean
   ) is
      Status : OQS_STATUS;
   begin
      -- Call liboqs to generate keypair
      Status := OQS_KEM_ML_KEM.OQS_KEM_ml_kem_1024_keypair (
         public_key => Public_Key.Data (Public_Key.Data'First)'Address,
         secret_key => Secret_Key.Data (Secret_Key.Data'First)'Address
      );

      if Status = OQS_SUCCESS then
         Secret_Key.Valid := True;
         Success := True;
      else
         -- Zeroize on failure
         Secret_Key.Valid := False;
         for I in Secret_Key.Data'Range loop
            Secret_Key.Data (I) := 0;
         end loop;
         Success := False;
      end if;
   end ML_KEM_Generate_Keypair;

   procedure ML_KEM_Encapsulate (
      Recipient_Public_Key : in     ML_KEM_Public_Key;
      Ciphertext           : out    ML_KEM_Ciphertext;
      Shared_Secret        : out    ML_KEM_Shared_Secret;
      Success              : out    Boolean
   ) is
      Status : OQS_STATUS;
   begin
      -- Call liboqs to encapsulate
      Status := OQS_KEM_ML_KEM.OQS_KEM_ml_kem_1024_encaps (
         ciphertext    => Ciphertext.Data (Ciphertext.Data'First)'Address,
         shared_secret => Shared_Secret.Data (Shared_Secret.Data'First)'Address,
         public_key    => Recipient_Public_Key.Data (Recipient_Public_Key.Data'First)'Address
      );

      if Status = OQS_SUCCESS then
         Shared_Secret.Valid := True;
         Success := True;
      else
         -- Zeroize shared secret on failure
         Shared_Secret.Valid := False;
         for I in Shared_Secret.Data'Range loop
            Shared_Secret.Data (I) := 0;
         end loop;
         Success := False;
      end if;
   end ML_KEM_Encapsulate;

   procedure ML_KEM_Decapsulate (
      Ciphertext    : in     ML_KEM_Ciphertext;
      Secret_Key    : in     ML_KEM_Secret_Key;
      Shared_Secret : out    ML_KEM_Shared_Secret;
      Success       : out    Boolean
   ) is
      Status : OQS_STATUS;
   begin
      -- Call liboqs to decapsulate
      Status := OQS_KEM_ML_KEM.OQS_KEM_ml_kem_1024_decaps (
         shared_secret => Shared_Secret.Data (Shared_Secret.Data'First)'Address,
         ciphertext    => Ciphertext.Data (Ciphertext.Data'First)'Address,
         secret_key    => Secret_Key.Data (Secret_Key.Data'First)'Address
      );

      if Status = OQS_SUCCESS then
         Shared_Secret.Valid := True;
         Success := True;
      else
         -- Zeroize shared secret on failure
         Shared_Secret.Valid := False;
         for I in Shared_Secret.Data'Range loop
            Shared_Secret.Data (I) := 0;
         end loop;
         Success := False;
      end if;
   end ML_KEM_Decapsulate;

   -------------------------------------------------------------------------
   -- ML-DSA-87 Implementation
   -------------------------------------------------------------------------

   procedure ML_DSA_Generate_Keypair (
      Public_Key  : out ML_DSA_Public_Key;
      Secret_Key  : out ML_DSA_Secret_Key;
      Success     : out Boolean
   ) is
      Status : OQS_STATUS;
   begin
      -- Call liboqs to generate keypair
      Status := OQS_SIG_ML_DSA.OQS_SIG_ml_dsa_87_keypair (
         public_key => Public_Key.Data (Public_Key.Data'First)'Address,
         secret_key => Secret_Key.Data (Secret_Key.Data'First)'Address
      );

      if Status = OQS_SUCCESS then
         Secret_Key.Valid := True;
         Success := True;
      else
         -- Zeroize on failure
         Secret_Key.Valid := False;
         for I in Secret_Key.Data'Range loop
            Secret_Key.Data (I) := 0;
         end loop;
         Success := False;
      end if;
   end ML_DSA_Generate_Keypair;

   procedure ML_DSA_Sign (
      Message        : in     Byte_Array;
      Secret_Key     : in     ML_DSA_Secret_Key;
      Signature      : out    ML_DSA_Signature;
      Success        : out    Boolean
   ) is
      Status        : OQS_STATUS;
      Signature_Len : aliased Interfaces.C.size_t := Signature.Data'Length;
   begin
      -- Call liboqs to sign
      Status := OQS_SIG_ML_DSA.OQS_SIG_ml_dsa_87_sign (
         signature     => Signature.Data (Signature.Data'First)'Address,
         signature_len => Signature_Len'Access,
         message       => Message (Message'First)'Address,
         message_len   => Message'Length,
         secret_key    => Secret_Key.Data (Secret_Key.Data'First)'Address
      );

      Success := (Status = OQS_SUCCESS);
   end ML_DSA_Sign;

   function ML_DSA_Verify (
      Message     : Byte_Array;
      Signature   : ML_DSA_Signature;
      Public_Key  : ML_DSA_Public_Key
   ) return Boolean
   is
      Status : OQS_STATUS;
   begin
      -- Call liboqs to verify
      Status := OQS_SIG_ML_DSA.OQS_SIG_ml_dsa_87_verify (
         message       => Message (Message'First)'Address,
         message_len   => Message'Length,
         signature     => Signature.Data (Signature.Data'First)'Address,
         signature_len => Signature.Data'Length,
         public_key    => Public_Key.Data (Public_Key.Data'First)'Address
      );

      return Status = OQS_SUCCESS;
   end ML_DSA_Verify;

   -------------------------------------------------------------------------
   -- Constant-Time Comparison
   -------------------------------------------------------------------------

   function Secrets_Match (
      Secret_A : ML_KEM_Shared_Secret;
      Secret_B : ML_KEM_Shared_Secret
   ) return Boolean
   is
      use System;
      Addr_A : constant System.Address := Secret_A.Data (Secret_A.Data'First)'Address;
      Addr_B : constant System.Address := Secret_B.Data (Secret_B.Data'First)'Address;
      Result : Interfaces.C.int;
   begin
      -- Use liboqs constant-time comparison (returns 0 if equal)
      Result := OQS_MEM_secure_bcmp (Addr_A, Addr_B, Secret_A.Data'Length);
      return Result = 0;
   end Secrets_Match;

   -------------------------------------------------------------------------
   -- Secure Zeroization (using liboqs secure cleanse)
   -------------------------------------------------------------------------

   procedure Zeroize_Shared_Secret (
      Secret : in out ML_KEM_Shared_Secret
   ) is
      Secret_Addr : constant System.Address := Secret.Data (Secret.Data'First)'Address;
   begin
      -- Use liboqs secure cleanse (protected against compiler optimization)
      OQS_MEM_cleanse (Secret_Addr, Secret.Data'Length);
      Secret.Valid := False;
   end Zeroize_Shared_Secret;

   procedure Zeroize_ML_KEM_Secret (
      Secret_Key : in out ML_KEM_Secret_Key
   ) is
      Key_Addr : constant System.Address := Secret_Key.Data (Secret_Key.Data'First)'Address;
   begin
      OQS_MEM_cleanse (Key_Addr, Secret_Key.Data'Length);
      Secret_Key.Valid := False;
   end Zeroize_ML_KEM_Secret;

   procedure Zeroize_ML_DSA_Secret (
      Secret_Key : in out ML_DSA_Secret_Key
   ) is
      Key_Addr : constant System.Address := Secret_Key.Data (Secret_Key.Data'First)'Address;
   begin
      OQS_MEM_cleanse (Key_Addr, Secret_Key.Data'Length);
      Secret_Key.Valid := False;
   end Zeroize_ML_DSA_Secret;

   -------------------------------------------------------------------------
   -- Hybrid Operations (Classical + Post-Quantum)
   -------------------------------------------------------------------------

   procedure Hybrid_Encapsulate (
      X25519_Public  : in     X25519_Public_Key;
      ML_KEM_Public  : in     ML_KEM_Public_Key;
      X25519_Ephemeral_Secret : out X25519_Secret_Key;
      Ciphertext     : out ML_KEM_Ciphertext;
      Hybrid_Secret  : out    Hybrid_Shared_Secret;
      Success        : out    Boolean
   ) is
      pragma Unreferenced (X25519_Public);
      pragma Unreferenced (X25519_Ephemeral_Secret);
      ML_KEM_Shared : ML_KEM_Shared_Secret;
      PQ_Success    : Boolean;
   begin
      -- TODO: Implement X25519 key exchange (needs SPARKNaCl or similar)
      -- For now, this is a placeholder showing the architecture

      -- Step 1: Perform ML-KEM-1024 encapsulation (post-quantum)
      ML_KEM_Encapsulate (
         Recipient_Public_Key => ML_KEM_Public,
         Ciphertext           => Ciphertext,
         Shared_Secret        => ML_KEM_Shared,
         Success              => PQ_Success
      );

      if not PQ_Success then
         Success := False;
         Hybrid_Secret.Valid := False;
         return;
      end if;

      -- Step 2: Would perform X25519 key exchange here
      -- X25519_ECDH (X25519_Public, X25519_Ephemeral_Secret, Classical_Shared)

      -- Step 3: Combine both secrets using HKDF
      -- For now, copy PQ secret (will be properly combined later)
      Hybrid_Secret.PQ_Secret := ML_KEM_Shared.Data;
      Hybrid_Secret.Valid := True;
      Success := True;

      -- Zeroize intermediate secret
      Zeroize_Shared_Secret (ML_KEM_Shared);
   end Hybrid_Encapsulate;

   procedure Derive_Encryption_Key (
      Hybrid_Secret : in     Hybrid_Shared_Secret;
      Encryption_Key : out    XChaCha20_Key;
      Success        : out    Boolean
   ) is
   begin
      -- TODO: Implement proper HKDF key derivation
      -- For now, use first 32 bytes of hybrid secret
      -- SECURITY NOTE: This is a placeholder - real implementation must use HKDF

      if not Hybrid_Secret.Valid then
         Success := False;
         Encryption_Key.Valid := False;
         return;
      end if;

      -- Derive encryption key from hybrid secret
      -- In production: HKDF-SHA256(Classical || PQ, "anubis-encryption", 32)
      Encryption_Key.Data := Hybrid_Secret.PQ_Secret;
      Encryption_Key.Valid := True;
      Success := True;
   end Derive_Encryption_Key;

end Anubis_Types.PQC;
