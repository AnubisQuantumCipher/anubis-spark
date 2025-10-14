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
with Anubis_Types.Classical;

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
         -- SECURITY: Zeroize BOTH keys on failure
         Secret_Key.Valid := False;
         for I in Secret_Key.Data'Range loop
            Secret_Key.Data (I) := 0;
         end loop;
         -- Also zero public key (defense in depth)
         for I in Public_Key.Data'Range loop
            Public_Key.Data (I) := 0;
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
         -- SECURITY: Zeroize BOTH keys on failure
         Secret_Key.Valid := False;
         for I in Secret_Key.Data'Range loop
            Secret_Key.Data (I) := 0;
         end loop;
         -- Also zero public key (defense in depth)
         for I in Public_Key.Data'Range loop
            Public_Key.Data (I) := 0;
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
      X25519_Ephemeral_Public : out X25519_Public_Key;
      X25519_Ephemeral_Secret : out X25519_Secret_Key;
      Ciphertext     : out ML_KEM_Ciphertext;
      Hybrid_Secret  : out    Hybrid_Shared_Secret;
      Success        : out    Boolean
   ) is
      ML_KEM_Shared     : ML_KEM_Shared_Secret;
      X25519_Shared     : X25519_Shared_Secret;
      PQ_Success        : Boolean;
      Classical_Success : Boolean;
      Combined_Input    : Byte_Array (1 .. 64);  -- 32 bytes classical + 32 bytes PQ
   begin
      -- Step 1: Generate ephemeral X25519 keypair
      Classical.X25519_Generate_Keypair (
         Public_Key  => X25519_Ephemeral_Public,
         Secret_Key  => X25519_Ephemeral_Secret,
         Success     => Classical_Success
      );

      if not Classical_Success then
         Success := False;
         Hybrid_Secret.Valid := False;
         return;
      end if;

      -- Step 2: Perform X25519 ECDH (classical key exchange)
      Classical.X25519_Compute_Shared (
         Our_Secret_Key   => X25519_Ephemeral_Secret,
         Their_Public_Key => X25519_Public,
         Shared_Secret    => X25519_Shared,
         Success          => Classical_Success
      );

      if not Classical_Success then
         Success := False;
         Hybrid_Secret.Valid := False;
         Classical.Zeroize_X25519_Secret (X25519_Ephemeral_Secret);
         return;
      end if;

      -- Step 3: Perform ML-KEM-1024 encapsulation (post-quantum)
      ML_KEM_Encapsulate (
         Recipient_Public_Key => ML_KEM_Public,
         Ciphertext           => Ciphertext,
         Shared_Secret        => ML_KEM_Shared,
         Success              => PQ_Success
      );

      if not PQ_Success then
         Success := False;
         Hybrid_Secret.Valid := False;
         Classical.Zeroize_X25519_Secret (X25519_Ephemeral_Secret);
         Classical.Zeroize_X25519_Shared (X25519_Shared);
         return;
      end if;

      -- Step 4: Combine both secrets using HKDF-SHA256
      -- Input: Classical (32 bytes) || PQ (32 bytes)
      Combined_Input (1 .. 32) := X25519_Shared.Data;
      Combined_Input (33 .. 64) := ML_KEM_Shared.Data;

      -- Derive hybrid secret using HKDF
      declare
         Temp_Key : Byte_Array (1 .. 32);
      begin
         Classical.HKDF_Derive (
            Input_Key_Material => Combined_Input,
            Context_String     => "anubis-hybrid-kem-v1",
            Output_Key         => Temp_Key,
            Success            => Success
         );
         Hybrid_Secret.PQ_Secret := Temp_Key;
      end;

      if Success then
         Hybrid_Secret.Valid := True;
      else
         Hybrid_Secret.Valid := False;
      end if;

      -- Step 5: Secure cleanup
      Zeroize_Shared_Secret (ML_KEM_Shared);
      Classical.Zeroize_X25519_Shared (X25519_Shared);
      for I in Combined_Input'Range loop
         Combined_Input (I) := 0;
      end loop;
   end Hybrid_Encapsulate;

   procedure Hybrid_Decapsulate (
      X25519_Secret       : in     X25519_Secret_Key;
      ML_KEM_Secret       : in     ML_KEM_Secret_Key;
      X25519_Ephemeral    : in     X25519_Public_Key;
      ML_KEM_CT           : in     ML_KEM_Ciphertext;
      Hybrid_Secret       : out    Hybrid_Shared_Secret;
      Success             : out    Boolean
   ) is
      ML_KEM_Shared     : ML_KEM_Shared_Secret;
      X25519_Shared     : X25519_Shared_Secret;
      PQ_Success        : Boolean;
      Classical_Success : Boolean;
      Combined_Input    : Byte_Array (1 .. 64);  -- 32 bytes classical + 32 bytes PQ
   begin
      -- Step 1: Perform X25519 ECDH with ephemeral public key (classical key exchange)
      Classical.X25519_Compute_Shared (
         Our_Secret_Key   => X25519_Secret,
         Their_Public_Key => X25519_Ephemeral,
         Shared_Secret    => X25519_Shared,
         Success          => Classical_Success
      );

      if not Classical_Success then
         Success := False;
         Hybrid_Secret.Valid := False;
         return;
      end if;

      -- Step 2: Perform ML-KEM-1024 decapsulation (post-quantum)
      ML_KEM_Decapsulate (
         Ciphertext    => ML_KEM_CT,
         Secret_Key    => ML_KEM_Secret,
         Shared_Secret => ML_KEM_Shared,
         Success       => PQ_Success
      );

      if not PQ_Success then
         Success := False;
         Hybrid_Secret.Valid := False;
         Classical.Zeroize_X25519_Shared (X25519_Shared);
         return;
      end if;

      -- Step 3: Combine both secrets using HKDF-SHA256 (same as encapsulation)
      -- Input: Classical (32 bytes) || PQ (32 bytes)
      Combined_Input (1 .. 32) := X25519_Shared.Data;
      Combined_Input (33 .. 64) := ML_KEM_Shared.Data;

      -- Derive hybrid secret using HKDF
      declare
         Temp_Key : Byte_Array (1 .. 32);
      begin
         Classical.HKDF_Derive (
            Input_Key_Material => Combined_Input,
            Context_String     => "anubis-hybrid-kem-v1",
            Output_Key         => Temp_Key,
            Success            => Success
         );
         Hybrid_Secret.PQ_Secret := Temp_Key;
      end;

      if Success then
         Hybrid_Secret.Valid := True;
      else
         Hybrid_Secret.Valid := False;
      end if;

      -- Step 4: Secure cleanup
      Zeroize_Shared_Secret (ML_KEM_Shared);
      Classical.Zeroize_X25519_Shared (X25519_Shared);
      for I in Combined_Input'Range loop
         Combined_Input (I) := 0;
      end loop;
   end Hybrid_Decapsulate;

   procedure Derive_Encryption_Key (
      Hybrid_Secret : in     Hybrid_Shared_Secret;
      Encryption_Key : out    XChaCha20_Key;
      Success        : out    Boolean
   ) is
   begin
      if not Hybrid_Secret.Valid then
         Success := False;
         Encryption_Key.Valid := False;
         return;
      end if;

      -- Derive XChaCha20 encryption key from hybrid secret using HKDF
      declare
         Temp_Input : Byte_Array (1 .. 32);
         Temp_Key   : Byte_Array (1 .. 32);
      begin
         -- Copy from volatile record to local variable
         Temp_Input := Hybrid_Secret.PQ_Secret;

         Classical.HKDF_Derive (
            Input_Key_Material => Temp_Input,
            Context_String     => "anubis-xchacha20-key-v1",
            Output_Key         => Temp_Key,
            Success            => Success
         );
         Encryption_Key.Data := Temp_Key;
      end;

      if Success then
         Encryption_Key.Valid := True;
      else
         Encryption_Key.Valid := False;
      end if;
   end Derive_Encryption_Key;

   -------------------------------------------------------------------------
   -- Ghost Function Implementations
   -------------------------------------------------------------------------

   function Hybrid_Signature_Zeroed (Sig : Hybrid_Signature) return Boolean is
   begin
      for I in Sig.Ed25519_Sig.Data'Range loop
         if Sig.Ed25519_Sig.Data (I) /= 0 then
            return False;
         end if;
      end loop;
      for I in Sig.ML_DSA_Sig.Data'Range loop
         if Sig.ML_DSA_Sig.Data (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Hybrid_Signature_Zeroed;

   function Both_Signatures_Present (Sig : Hybrid_Signature) return Boolean is
      Ed_Present : Boolean := False;
      DSA_Present : Boolean := False;
   begin
      for I in Sig.Ed25519_Sig.Data'Range loop
         if Sig.Ed25519_Sig.Data (I) /= 0 then
            Ed_Present := True;
            exit;
         end if;
      end loop;
      for I in Sig.ML_DSA_Sig.Data'Range loop
         if Sig.ML_DSA_Sig.Data (I) /= 0 then
            DSA_Present := True;
            exit;
         end if;
      end loop;
      return Ed_Present and DSA_Present;
   end Both_Signatures_Present;

   function Hybrid_Secret_Well_Formed (Secret : Hybrid_Shared_Secret) return Boolean is
      Classical_NonZero : Boolean := False;
      PQ_NonZero : Boolean := False;
   begin
      -- Check if classical secret has non-zero bytes
      for I in Secret.Classical_Secret'Range loop
         if Secret.Classical_Secret (I) /= 0 then
            Classical_NonZero := True;
            exit;
         end if;
      end loop;
      -- Check if PQ secret has non-zero bytes
      for I in Secret.PQ_Secret'Range loop
         if Secret.PQ_Secret (I) /= 0 then
            PQ_NonZero := True;
            exit;
         end if;
      end loop;
      -- Well-formed if both secrets present and valid flag set
      return Classical_NonZero and PQ_NonZero and Secret.Valid;
   end Hybrid_Secret_Well_Formed;

   -------------------------------------------------------------------------
   -- Hybrid Signature Accessors
   -------------------------------------------------------------------------

   procedure Get_Signature_Components (
      Sig         : in     Hybrid_Signature;
      Ed25519_Sig : out    Ed25519_Signature;
      ML_DSA_Sig  : out    ML_DSA_Signature
   ) is
   begin
      Ed25519_Sig := Sig.Ed25519_Sig;
      ML_DSA_Sig := Sig.ML_DSA_Sig;
   end Get_Signature_Components;

   procedure Set_Signature_Components (
      Sig         : out    Hybrid_Signature;
      Ed25519_Sig : in     Ed25519_Signature;
      ML_DSA_Sig  : in     ML_DSA_Signature
   ) is
   begin
      Sig.Ed25519_Sig := Ed25519_Sig;
      Sig.ML_DSA_Sig := ML_DSA_Sig;
   end Set_Signature_Components;

   -------------------------------------------------------------------------
   -- Hybrid Signatures (Classical + Post-Quantum)
   -- PLATINUM LEVEL: Both signatures must verify for overall validity
   -------------------------------------------------------------------------

   procedure Hybrid_Sign (
      Message     : in     Byte_Array;
      Ed25519_SK  : in     Ed25519_Secret_Key;
      ML_DSA_SK   : in     ML_DSA_Secret_Key;
      Signature   : out    Hybrid_Signature;
      Success     : out    Boolean
   ) is
      Ed25519_Success : Boolean;
      ML_DSA_Success  : Boolean;
   begin
      -- Step 1: Sign with Ed25519 (classical)
      Classical.Ed25519_Sign (
         Message    => Message,
         Secret_Key => Ed25519_SK,
         Signature  => Signature.Ed25519_Sig,
         Success    => Ed25519_Success
      );

      if not Ed25519_Success then
         -- Zeroize Ed25519 signature on failure
         for I in Signature.Ed25519_Sig.Data'Range loop
            Signature.Ed25519_Sig.Data (I) := 0;
         end loop;
         -- Zeroize ML-DSA signature (contractually required)
         for I in Signature.ML_DSA_Sig.Data'Range loop
            Signature.ML_DSA_Sig.Data (I) := 0;
         end loop;
         Success := False;
         return;
      end if;

      -- Step 2: Sign with ML-DSA-87 (post-quantum)
      ML_DSA_Sign (
         Message    => Message,
         Secret_Key => ML_DSA_SK,
         Signature  => Signature.ML_DSA_Sig,
         Success    => ML_DSA_Success
      );

      if not ML_DSA_Success then
         -- Zeroize both signatures on failure
         for I in Signature.Ed25519_Sig.Data'Range loop
            Signature.Ed25519_Sig.Data (I) := 0;
         end loop;
         for I in Signature.ML_DSA_Sig.Data'Range loop
            Signature.ML_DSA_Sig.Data (I) := 0;
         end loop;
         Success := False;
         return;
      end if;

      -- Both signatures succeeded
      Success := True;
   end Hybrid_Sign;

   function Hybrid_Verify (
      Message     : Byte_Array;
      Signature   : Hybrid_Signature;
      Ed25519_PK  : Ed25519_Public_Key;
      ML_DSA_PK   : ML_DSA_Public_Key
   ) return Boolean
   is
      Ed25519_Valid : Boolean;
      ML_DSA_Valid  : Boolean;
   begin
      -- SECURITY: Always verify BOTH signatures (constant-time)
      -- No short-circuit evaluation to prevent timing attacks

      -- Step 1: Verify Ed25519 signature (classical)
      Ed25519_Valid := Classical.Ed25519_Verify (
         Message    => Message,
         Signature  => Signature.Ed25519_Sig,
         Public_Key => Ed25519_PK
      );

      -- Step 2: Verify ML-DSA-87 signature (post-quantum)
      -- ALWAYS executed regardless of Ed25519 result
      ML_DSA_Valid := ML_DSA_Verify (
         Message    => Message,
         Signature  => Signature.ML_DSA_Sig,
         Public_Key => ML_DSA_PK
      );

      -- PLATINUM LEVEL: BOTH signatures must verify
      -- Return conjunction after checking both (timing-safe)
      return Ed25519_Valid and then ML_DSA_Valid;
   end Hybrid_Verify;

end Anubis_Types.PQC;
