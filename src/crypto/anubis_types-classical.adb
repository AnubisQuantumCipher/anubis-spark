-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Classical Cryptography Wrapper Implementation
-- Bridges SPARK-safe interface with libsodium C library
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- C FFI not provable in SPARK

with Interfaces.C; use Interfaces.C;
with System;
with Sodium_Common; use Sodium_Common;
with Sodium_Scalarmult;
with Sodium_Sign;
with Sodium_AEAD;
with Sodium_Pwhash;
with Sodium_KDF;

package body Anubis_Types.Classical is

   -------------------------------------------------------------------------
   -- PLATINUM LEVEL: Ghost Function Bodies
   -------------------------------------------------------------------------

   function Auth_Tag_Valid (Tag : XChaCha20_Auth_Tag) return Boolean is
      All_Zero : Boolean := True;
   begin
      for I in Tag.Data'Range loop
         if Tag.Data (I) /= 0 then
            All_Zero := False;
            exit;
         end if;
      end loop;
      return not All_Zero;
   end Auth_Tag_Valid;

   -------------------------------------------------------------------------
   -- X25519 Operations (Elliptic Curve Diffie-Hellman)
   -------------------------------------------------------------------------

   procedure X25519_Generate_Keypair (
      Public_Key  : out X25519_Public_Key;
      Secret_Key  : out X25519_Secret_Key;
      Success     : out Boolean
   ) is
      Status : Interfaces.C.int;
   begin
      -- Generate X25519 keypair using libsodium
      Status := Sodium_Scalarmult.crypto_box_keypair (
         public_key => Public_Key.Data (Public_Key.Data'First)'Address,
         secret_key => Secret_Key.Data (Secret_Key.Data'First)'Address
      );

      if Status = SODIUM_SUCCESS then
         Secret_Key.Valid := True;
         Success := True;
      else
         -- SECURITY: Zeroize BOTH keys on failure
         Secret_Key.Valid := False;
         for I in Secret_Key.Data'Range loop
            pragma Loop_Invariant (for all J in Secret_Key.Data'First .. I - 1 =>
                                     Secret_Key.Data (J) = 0);
            pragma Loop_Variant (Increases => I);
            Secret_Key.Data (I) := 0;
         end loop;
         -- Also zero public key (defense in depth)
         for I in Public_Key.Data'Range loop
            pragma Loop_Invariant (for all J in Public_Key.Data'First .. I - 1 =>
                                     Public_Key.Data (J) = 0);
            pragma Loop_Variant (Increases => I);
            Public_Key.Data (I) := 0;
         end loop;
         Success := False;
      end if;
   end X25519_Generate_Keypair;

   procedure X25519_Compute_Shared (
      Our_Secret_Key   : in     X25519_Secret_Key;
      Their_Public_Key : in     X25519_Public_Key;
      Shared_Secret    : out    X25519_Shared_Secret;
      Success          : out    Boolean
   ) is
      Status : Interfaces.C.int;
   begin
      -- Compute X25519 shared secret (ECDH)
      Status := Sodium_Scalarmult.crypto_scalarmult (
         shared_secret    => Shared_Secret.Data (Shared_Secret.Data'First)'Address,
         our_secret_key   => Our_Secret_Key.Data (Our_Secret_Key.Data'First)'Address,
         their_public_key => Their_Public_Key.Data (Their_Public_Key.Data'First)'Address
      );

      if Status = SODIUM_SUCCESS then
         Shared_Secret.Valid := True;
         Success := True;
      else
         -- Zeroize on failure
         Shared_Secret.Valid := False;
         for I in Shared_Secret.Data'Range loop
            pragma Loop_Invariant (for all J in Shared_Secret.Data'First .. I - 1 =>
                                     Shared_Secret.Data (J) = 0);
            pragma Loop_Variant (Increases => I);
            Shared_Secret.Data (I) := 0;
         end loop;
         Success := False;
      end if;
   end X25519_Compute_Shared;

   procedure Zeroize_X25519_Secret (
      Secret_Key : in out X25519_Secret_Key
   ) is
      Key_Addr : constant System.Address := Secret_Key.Data (Secret_Key.Data'First)'Address;
   begin
      sodium_memzero (Key_Addr, Secret_Key.Data'Length);
      Secret_Key.Valid := False;
   end Zeroize_X25519_Secret;

   procedure Zeroize_X25519_Shared (
      Shared_Secret : in out X25519_Shared_Secret
   ) is
      Secret_Addr : constant System.Address := Shared_Secret.Data (Shared_Secret.Data'First)'Address;
   begin
      sodium_memzero (Secret_Addr, Shared_Secret.Data'Length);
      Shared_Secret.Valid := False;
   end Zeroize_X25519_Shared;

   -------------------------------------------------------------------------
   -- Ed25519 Operations (Digital Signatures)
   -------------------------------------------------------------------------

   procedure Ed25519_Generate_Keypair (
      Public_Key  : out Ed25519_Public_Key;
      Secret_Key  : out Ed25519_Secret_Key;
      Success     : out Boolean
   ) is
      Status : Interfaces.C.int;
      -- Ed25519 secret key in libsodium is 64 bytes (32-byte seed + 32-byte public key)
      -- but we only store the 32-byte seed in our type
      Full_Secret : array (1 .. 64) of Byte;
   begin
      -- Generate Ed25519 keypair
      Status := Sodium_Sign.crypto_sign_keypair (
         public_key => Public_Key.Data (Public_Key.Data'First)'Address,
         secret_key => Full_Secret (Full_Secret'First)'Address
      );

      if Status = SODIUM_SUCCESS then
         -- Extract the 32-byte seed from the 64-byte secret key
         for I in Secret_Key.Data'Range loop
            pragma Warnings (Off, "Secret_Key.Data* may be referenced before it has a value");
            pragma Loop_Invariant
              (I = Secret_Key.Data'First or else
               (for all J in Secret_Key.Data'First .. I - 1 =>
                  Secret_Key.Data (J) = Full_Secret (J)));
            pragma Loop_Variant (Increases => I);
            pragma Warnings (On, "Secret_Key.Data* may be referenced before it has a value");
            Secret_Key.Data (I) := Full_Secret (I);
         end loop;
         Secret_Key.Valid := True;
         Success := True;

         -- Zeroize temporary full secret
         sodium_memzero (Full_Secret (Full_Secret'First)'Address, Full_Secret'Length);
      else
         -- SECURITY: Zeroize BOTH keys on failure
         Secret_Key.Valid := False;
         for I in Secret_Key.Data'Range loop
            pragma Loop_Invariant (for all J in Secret_Key.Data'First .. I - 1 =>
                                     Secret_Key.Data (J) = 0);
            pragma Loop_Variant (Increases => I);
            Secret_Key.Data (I) := 0;
         end loop;
         -- Also zero public key (defense in depth)
         for I in Public_Key.Data'Range loop
            pragma Loop_Invariant (for all J in Public_Key.Data'First .. I - 1 =>
                                     Public_Key.Data (J) = 0);
            pragma Loop_Variant (Increases => I);
            Public_Key.Data (I) := 0;
         end loop;
         -- Zeroize temporary full secret
         sodium_memzero (Full_Secret (Full_Secret'First)'Address, Full_Secret'Length);
         Success := False;
      end if;
   end Ed25519_Generate_Keypair;

   procedure Ed25519_Sign (
      Message    : in     Byte_Array;
      Secret_Key : in     Ed25519_Secret_Key;
      Signature  : out    Ed25519_Signature;
      Success    : out    Boolean
   ) is
      Status        : Interfaces.C.int;
      Signature_Len : aliased Interfaces.C.unsigned_long := Signature.Data'Length;
      -- Reconstruct 64-byte secret key from our 32-byte seed
      Full_Secret   : array (1 .. 64) of Byte;
      Temp_Public   : array (1 .. 32) of Byte;
   begin
      -- Derive the 64-byte secret key from the 32-byte seed
      Status := Sodium_Sign.crypto_sign_seed_keypair (
         public_key => Temp_Public (Temp_Public'First)'Address,
         secret_key => Full_Secret (Full_Secret'First)'Address,
         seed       => Secret_Key.Data (Secret_Key.Data'First)'Address
      );

      if Status /= SODIUM_SUCCESS then
         Success := False;
         return;
      end if;

      -- Sign message
      Status := Sodium_Sign.crypto_sign_detached (
         signature     => Signature.Data (Signature.Data'First)'Address,
         signature_len => Signature_Len'Access,
         message       => Message (Message'First)'Address,
         message_len   => Interfaces.C.unsigned_long (Message'Length),
         secret_key    => Full_Secret (Full_Secret'First)'Address
      );

      Success := (Status = SODIUM_SUCCESS);

      -- Zeroize temporary keys
      sodium_memzero (Full_Secret (Full_Secret'First)'Address, Full_Secret'Length);
      sodium_memzero (Temp_Public (Temp_Public'First)'Address, Temp_Public'Length);
   end Ed25519_Sign;

   function Ed25519_Verify (
      Message    : Byte_Array;
      Signature  : Ed25519_Signature;
      Public_Key : Ed25519_Public_Key
   ) return Boolean
   is
      Status : Interfaces.C.int;
   begin
      -- Verify signature
      Status := Sodium_Sign.crypto_sign_verify_detached (
         signature   => Signature.Data (Signature.Data'First)'Address,
         message     => Message (Message'First)'Address,
         message_len => Interfaces.C.unsigned_long (Message'Length),
         public_key  => Public_Key.Data (Public_Key.Data'First)'Address
      );

      return Status = SODIUM_SUCCESS;
   end Ed25519_Verify;

   procedure Zeroize_Ed25519_Secret (
      Secret_Key : in out Ed25519_Secret_Key
   ) is
      Key_Addr : constant System.Address := Secret_Key.Data (Secret_Key.Data'First)'Address;
   begin
      sodium_memzero (Key_Addr, Secret_Key.Data'Length);
      Secret_Key.Valid := False;
   end Zeroize_Ed25519_Secret;

   -------------------------------------------------------------------------
   -- XChaCha20-Poly1305 AEAD Encryption
   -------------------------------------------------------------------------

   procedure XChaCha20_Generate_Key (
      Key     : out XChaCha20_Key;
      Success : out Boolean
   ) is
   begin
      -- Generate random XChaCha20 key
      Sodium_AEAD.crypto_aead_xchacha20poly1305_ietf_keygen (
         key => Key.Data (Key.Data'First)'Address
      );

      Key.Valid := True;
      Success := True;
   end XChaCha20_Generate_Key;

   procedure XChaCha20_Encrypt (
      Plaintext  : in     Byte_Array;
      Key        : in     XChaCha20_Key;
      Nonce      : in     XChaCha20_Nonce;
      AAD        : in     Byte_Array;
      Ciphertext : out    Byte_Array;
      Auth_Tag   : out    XChaCha20_Auth_Tag;
      Success    : out    Boolean
   ) is
      Status         : Interfaces.C.int;
      Ciphertext_Len : aliased Interfaces.C.unsigned_long;
      AAD_Addr       : System.Address;
      AAD_Len        : Interfaces.C.unsigned_long;
   begin
      -- Set AAD address and length (use Null_Address if AAD is empty)
      if AAD'Length > 0 then
         AAD_Addr := AAD (AAD'First)'Address;
         AAD_Len := Interfaces.C.unsigned_long (AAD'Length);
      else
         AAD_Addr := System.Null_Address;
         AAD_Len := 0;
      end if;

      -- Encrypt with detached authentication tag (writes directly to Ciphertext)
      Status := Sodium_AEAD.crypto_aead_xchacha20poly1305_ietf_encrypt_detached (
         ciphertext          => Ciphertext (Ciphertext'First)'Address,
         mac                 => Auth_Tag.Data (Auth_Tag.Data'First)'Address,
         mac_len             => Ciphertext_Len'Access,
         message             => Plaintext (Plaintext'First)'Address,
         message_len         => Interfaces.C.unsigned_long (Plaintext'Length),
         additional_data     => AAD_Addr,
         additional_data_len => AAD_Len,
         nsec                => System.Null_Address,
         nonce               => Nonce.Data (Nonce.Data'First)'Address,
         key                 => Key.Data (Key.Data'First)'Address
      );

      if Status = SODIUM_SUCCESS then
         Success := True;
      else
         -- Zeroize on failure
         for I in Ciphertext'Range loop
            pragma Loop_Invariant (for all J in Ciphertext'First .. I - 1 =>
                                     Ciphertext (J) = 0);
            pragma Loop_Variant (Increases => I);
            Ciphertext (I) := 0;
         end loop;
         for I in Auth_Tag.Data'Range loop
            pragma Loop_Invariant (for all J in Auth_Tag.Data'First .. I - 1 =>
                                     Auth_Tag.Data (J) = 0);
            pragma Loop_Variant (Increases => I);
            Auth_Tag.Data (I) := 0;
         end loop;
         Success := False;
      end if;
   end XChaCha20_Encrypt;

   procedure XChaCha20_Decrypt (
      Ciphertext : in     Byte_Array;
      Auth_Tag   : in     XChaCha20_Auth_Tag;
      Key        : in     XChaCha20_Key;
      Nonce      : in     XChaCha20_Nonce;
      AAD        : in     Byte_Array;
      Plaintext  : out    Byte_Array;
      Success    : out    Boolean
   ) is
      Status   : Interfaces.C.int;
      AAD_Addr : System.Address;
      AAD_Len  : Interfaces.C.unsigned_long;
   begin
      -- Set AAD address and length (use Null_Address if AAD is empty)
      if AAD'Length > 0 then
         AAD_Addr := AAD (AAD'First)'Address;
         AAD_Len := Interfaces.C.unsigned_long (AAD'Length);
      else
         AAD_Addr := System.Null_Address;
         AAD_Len := 0;
      end if;

      -- Decrypt and verify authentication tag
      Status := Sodium_AEAD.crypto_aead_xchacha20poly1305_ietf_decrypt_detached (
         message             => Plaintext (Plaintext'First)'Address,
         nsec                => System.Null_Address,
         ciphertext          => Ciphertext (Ciphertext'First)'Address,
         ciphertext_len      => Interfaces.C.unsigned_long (Ciphertext'Length),
         mac                 => Auth_Tag.Data (Auth_Tag.Data'First)'Address,
         additional_data     => AAD_Addr,
         additional_data_len => AAD_Len,
         nonce               => Nonce.Data (Nonce.Data'First)'Address,
         key                 => Key.Data (Key.Data'First)'Address
      );

      if Status = SODIUM_SUCCESS then
         Success := True;
      else
         -- Zeroize plaintext on failure (invalid MAC)
         for I in Plaintext'Range loop
            pragma Loop_Invariant (for all J in Plaintext'First .. I - 1 =>
                                     Plaintext (J) = 0);
            pragma Loop_Variant (Increases => I);
            Plaintext (I) := 0;
         end loop;
         Success := False;
      end if;
   end XChaCha20_Decrypt;

   procedure Zeroize_XChaCha20_Key (
      Key : in out XChaCha20_Key
   ) is
      Key_Addr : constant System.Address := Key.Data (Key.Data'First)'Address;
   begin
      sodium_memzero (Key_Addr, Key.Data'Length);
      Key.Valid := False;
   end Zeroize_XChaCha20_Key;

   -------------------------------------------------------------------------
   -- Argon2id Key Derivation
   -------------------------------------------------------------------------

   procedure Argon2id_Derive_Key (
      Passphrase : in     String;
      Salt       : in     Argon2_Salt;
      Key        : out    Argon2_Derived_Key;
      Success    : out    Boolean
   ) is
      Status : Interfaces.C.int;
      -- Convert Ada String to C-compatible buffer
      Pass_Bytes : Byte_Array (1 .. Passphrase'Length);
   begin
      -- Copy passphrase to byte array
      for I in Passphrase'Range loop
         pragma Warnings (Off, "Pass_Bytes* may be referenced before it has a value");
         pragma Loop_Invariant
           (I = Passphrase'First or else
            (for all J in Passphrase'First .. I - 1 =>
               Pass_Bytes (J - Passphrase'First + 1) =
                 Byte (Character'Pos (Passphrase (J)))));
         pragma Loop_Variant (Increases => I);
         pragma Warnings (On, "Pass_Bytes* may be referenced before it has a value");
         Pass_Bytes (I - Passphrase'First + 1) := Byte (Character'Pos (Passphrase (I)));
      end loop;

      -- Derive key using Argon2id with MODERATE parameters (256 MiB, 3 iterations)
      Status := Sodium_Pwhash.crypto_pwhash (
         out_key      => Key.Data (Key.Data'First)'Address,
         outlen       => Interfaces.C.unsigned_long (Key.Data'Length),
         password     => Pass_Bytes (Pass_Bytes'First)'Address,
         password_len => Interfaces.C.unsigned_long (Pass_Bytes'Length),
         salt         => Salt.Data (Salt.Data'First)'Address,
         opslimit     => Interfaces.C.unsigned_long (Sodium_Pwhash.crypto_pwhash_OPSLIMIT_MODERATE),
         memlimit     => Interfaces.C.size_t (Sodium_Pwhash.crypto_pwhash_MEMLIMIT_MODERATE),
         alg          => Interfaces.C.int (Sodium_Pwhash.crypto_pwhash_ALG_ARGON2ID13)
      );

      if Status = SODIUM_SUCCESS then
         Key.Valid := True;
         Success := True;
      else
         -- Zeroize on failure
         Key.Valid := False;
         for I in Key.Data'Range loop
            pragma Loop_Invariant (for all J in Key.Data'First .. I - 1 =>
                                     Key.Data (J) = 0);
            pragma Loop_Variant (Increases => I);
            Key.Data (I) := 0;
         end loop;
         Success := False;
      end if;

      -- Zeroize passphrase bytes
      sodium_memzero (Pass_Bytes (Pass_Bytes'First)'Address, Pass_Bytes'Length);
   end Argon2id_Derive_Key;

   procedure Zeroize_Argon2_Key (
      Key : in out Argon2_Derived_Key
   ) is
      Key_Addr : constant System.Address := Key.Data (Key.Data'First)'Address;
   begin
      sodium_memzero (Key_Addr, Key.Data'Length);
      Key.Valid := False;
   end Zeroize_Argon2_Key;

   -------------------------------------------------------------------------
   -- HKDF Key Derivation (Two-Phase: Extract then Expand)
   -------------------------------------------------------------------------

   procedure HKDF_Derive (
      Input_Key_Material : in     Byte_Array;
      Context_String     : in     String;
      Output_Key         : out    Byte_Array;
      Success            : out    Boolean
   ) is
      Status : Interfaces.C.int;
      -- Pseudorandom key from extract phase (32 bytes for SHA256)
      PRK : array (1 .. 32) of Byte;
      -- Convert Ada String to C-compatible buffer
      Ctx_Bytes : Byte_Array (1 .. Context_String'Length);
   begin
      -- Copy context string to byte array
      for I in Context_String'Range loop
         pragma Warnings (Off, "Ctx_Bytes* may be referenced before it has a value");
         pragma Loop_Invariant
           (I = Context_String'First or else
            (for all J in Context_String'First .. I - 1 =>
               Ctx_Bytes (J - Context_String'First + 1) =
                 Byte (Character'Pos (Context_String (J)))));
         pragma Loop_Variant (Increases => I);
         pragma Warnings (On, "Ctx_Bytes* may be referenced before it has a value");
         Ctx_Bytes (I - Context_String'First + 1) := Byte (Character'Pos (Context_String (I)));
      end loop;

      -- Phase 1: Extract - derive PRK from input key material
      Status := Sodium_KDF.crypto_kdf_hkdf_sha256_extract (
         prk      => PRK (PRK'First)'Address,
         salt     => System.Null_Address,  -- No salt (optional)
         salt_len => 0,
         ikm      => Input_Key_Material (Input_Key_Material'First)'Address,
         ikm_len  => Interfaces.C.size_t (Input_Key_Material'Length)
      );

      if Status /= SODIUM_SUCCESS then
         -- Zeroize PRK and output on failure
         sodium_memzero (PRK (PRK'First)'Address, PRK'Length);
         for I in Output_Key'Range loop
            pragma Warnings (Off, "Output_Key* may be referenced before it has a value");
            pragma Loop_Invariant
              (I = Output_Key'First or else
               (for all J in Output_Key'First .. I - 1 => Output_Key (J) = 0));
            pragma Loop_Variant (Increases => I);
            pragma Warnings (On, "Output_Key* may be referenced before it has a value");
            Output_Key (I) := 0;
         end loop;
         Success := False;
         return;
      end if;

      -- Phase 2: Expand - derive output key from PRK using context
      Status := Sodium_KDF.crypto_kdf_hkdf_sha256_expand (
         out_key => Output_Key (Output_Key'First)'Address,
         out_len => Interfaces.C.size_t (Output_Key'Length),
         ctx     => Ctx_Bytes (Ctx_Bytes'First)'Address,
         ctx_len => Interfaces.C.size_t (Ctx_Bytes'Length),
         prk     => PRK (PRK'First)'Address
      );

      if Status = SODIUM_SUCCESS then
         Success := True;
      else
         -- Zeroize output on failure
         for I in Output_Key'Range loop
            pragma Loop_Invariant (for all J in Output_Key'First .. I - 1 =>
                                     Output_Key (J) = 0);
            pragma Loop_Variant (Increases => I);
            Output_Key (I) := 0;
         end loop;
         Success := False;
      end if;

      -- Always zeroize PRK after use (security critical)
      sodium_memzero (PRK (PRK'First)'Address, PRK'Length);
   end HKDF_Derive;

begin
   -- Package initialization: Initialize libsodium
   -- This must succeed for all cryptographic operations to work
   declare
      Init_Result : Interfaces.C.int;
   begin
      Init_Result := sodium_init;
      if Init_Result < 0 then
         raise Program_Error with "Failed to initialize libsodium";
      end if;
   end;

end Anubis_Types.Classical;
