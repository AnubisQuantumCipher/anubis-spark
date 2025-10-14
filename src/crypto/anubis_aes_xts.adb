-------------------------------------------------------------------------------
-- ANUBIS-SPARK: AES-256-XTS Implementation
-- Uses XChaCha20-Poly1305 as AES-XTS substitute (via libsodium)
-- Note: For production, would use OpenSSL EVP_aes_256_xts
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- Uses libsodium bindings

with Anubis_Types.Classical;
with Interfaces.C;

package body Anubis_AES_XTS is

   use Interfaces.C;

   -------------------------------------------------------------------------
   -- Libsodium FFI Bindings
   -------------------------------------------------------------------------

   -- XChaCha20-Poly1305 as AES-XTS substitute
   function crypto_aead_xchacha20poly1305_ietf_encrypt (
      c      : out Byte_Array;
      clen_p : access unsigned_long;
      m      : Byte_Array;
      mlen   : unsigned_long;
      ad     : Byte_Array;
      adlen  : unsigned_long;
      nsec   : System.Address;
      npub   : Byte_Array;
      k      : Byte_Array
   ) return int
   with Import, Convention => C,
        External_Name => "crypto_aead_xchacha20poly1305_ietf_encrypt";

   function crypto_aead_xchacha20poly1305_ietf_decrypt (
      m      : out Byte_Array;
      mlen_p : access unsigned_long;
      nsec   : System.Address;
      c      : Byte_Array;
      clen   : unsigned_long;
      ad     : Byte_Array;
      adlen  : unsigned_long;
      npub   : Byte_Array;
      k      : Byte_Array
   ) return int
   with Import, Convention => C,
        External_Name => "crypto_aead_xchacha20poly1305_ietf_decrypt";

   procedure randombytes_buf (
      buf  : out Byte_Array;
      size : size_t
   )
   with Import, Convention => C, External_Name => "randombytes_buf";

   function crypto_pwhash (
      out_key    : out Byte_Array;
      outlen     : unsigned_long;
      passwd     : char_array;
      passwdlen  : unsigned_long;
      salt       : Byte_Array;
      opslimit   : unsigned_long;
      memlimit   : size_t;
      alg        : int
   ) return int
   with Import, Convention => C, External_Name => "crypto_pwhash";

   crypto_pwhash_ALG_ARGON2ID13 : constant int := 2;

   -- Argon2id SENSITIVE parameters (LUKS2-inspired)
   ARGON2ID_OPSLIMIT : constant unsigned_long := 4;  -- 4 iterations
   ARGON2ID_MEMLIMIT : constant size_t := 1_073_741_824;  -- 1 GiB RAM

   -------------------------------------------------------------------------
   -- Implementation
   -------------------------------------------------------------------------

   procedure Generate_XTS_Key (
      Key     : out XTS_Key_Wrapper;
      Success : out Boolean
   ) is
   begin
      -- Generate cryptographically secure random key
      randombytes_buf (Key.Data, Key.Data'Length);

      -- Verify entropy (not all zeros)
      if Is_All_Zero (Key.Data) then
         -- Extremely rare RNG failure
         Key.Valid := False;
         for I in Key.Data'Range loop
            Key.Data (I) := 0;
         end loop;
         Success := False;
      else
         Key.Valid := True;
         Success := True;
      end if;
   exception
      when others =>
         Key.Valid := False;
         for I in Key.Data'Range loop
            Key.Data (I) := 0;
         end loop;
         Success := False;
   end Generate_XTS_Key;

   procedure AES_XTS_Encrypt (
      Plaintext  : in     Byte_Array;
      Key        : in     XTS_Key_Wrapper;
      IV         : in     XTS_IV;
      Ciphertext : out    Byte_Array;
      Auth_Tag   : out    XTS_Auth_Tag;
      Success    : out    Boolean
   ) is
      -- Use first 32 bytes of XTS key for XChaCha20
      XChaCha_Key : constant Byte_Array := Key.Data (1 .. 32);

      -- Combine IV with remaining key material as AAD
      AAD : constant Byte_Array := Key.Data (33 .. 64);

      -- Temporary buffer for ciphertext + tag
      Buffer : Byte_Array (1 .. Plaintext'Length + 16);
      Buffer_Len : aliased unsigned_long;

      Status : int;
   begin
      -- Encrypt using XChaCha20-Poly1305
      Status := crypto_aead_xchacha20poly1305_ietf_encrypt (
         c      => Buffer,
         clen_p => Buffer_Len'Access,
         m      => Plaintext,
         mlen   => unsigned_long (Plaintext'Length),
         ad     => AAD,
         adlen  => unsigned_long (AAD'Length),
         nsec   => System.Null_Address,
         npub   => IV & Byte_Array'(1 .. 8 => 0),  -- Extend to 24 bytes
         k      => XChaCha_Key
      );

      if Status = 0 then
         -- Split into ciphertext and auth tag
         Ciphertext := Buffer (1 .. Plaintext'Length);
         Auth_Tag := Buffer (Plaintext'Length + 1 .. Plaintext'Length + 16);
         Success := True;
      else
         -- Encryption failed - zero outputs
         for I in Ciphertext'Range loop
            Ciphertext (I) := 0;
         end loop;
         for I in Auth_Tag'Range loop
            Auth_Tag (I) := 0;
         end loop;
         Success := False;
      end if;
   exception
      when others =>
         for I in Ciphertext'Range loop
            Ciphertext (I) := 0;
         end loop;
         for I in Auth_Tag'Range loop
            Auth_Tag (I) := 0;
         end loop;
         Success := False;
   end AES_XTS_Encrypt;

   procedure AES_XTS_Decrypt (
      Ciphertext : in     Byte_Array;
      Auth_Tag   : in     XTS_Auth_Tag;
      Key        : in     XTS_Key_Wrapper;
      IV         : in     XTS_IV;
      Plaintext  : out    Byte_Array;
      Success    : out    Boolean
   ) is
      -- Use first 32 bytes of XTS key for XChaCha20
      XChaCha_Key : constant Byte_Array := Key.Data (1 .. 32);

      -- Combine IV with remaining key material as AAD
      AAD : constant Byte_Array := Key.Data (33 .. 64);

      -- Temporary buffer for ciphertext + tag
      Buffer : constant Byte_Array := Ciphertext & Auth_Tag;
      Plaintext_Len : aliased unsigned_long;

      Status : int;
   begin
      -- Decrypt and verify using XChaCha20-Poly1305
      Status := crypto_aead_xchacha20poly1305_ietf_decrypt (
         m      => Plaintext,
         mlen_p => Plaintext_Len'Access,
         nsec   => System.Null_Address,
         c      => Buffer,
         clen   => unsigned_long (Buffer'Length),
         ad     => AAD,
         adlen  => unsigned_long (AAD'Length),
         npub   => IV & Byte_Array'(1 .. 8 => 0),  -- Extend to 24 bytes
         k      => XChaCha_Key
      );

      if Status = 0 then
         -- Authentication passed
         Success := True;
      else
         -- Authentication failed - zero plaintext (SECURITY)
         for I in Plaintext'Range loop
            Plaintext (I) := 0;
         end loop;
         Success := False;
      end if;
   exception
      when others =>
         for I in Plaintext'Range loop
            Plaintext (I) := 0;
         end loop;
         Success := False;
   end AES_XTS_Decrypt;

   procedure Derive_XTS_Key_From_Passphrase (
      Passphrase : in     String;
      Salt       : in     Byte_Array;
      Key        : out    XTS_Key_Wrapper;
      Success    : out    Boolean
   ) is
      Pass_C : constant char_array := To_C (Passphrase);
      Status : int;
   begin
      -- Derive key using Argon2id SENSITIVE
      Status := crypto_pwhash (
         out_key   => Key.Data,
         outlen    => unsigned_long (Key.Data'Length),
         passwd    => Pass_C,
         passwdlen => unsigned_long (Passphrase'Length),
         salt      => Salt,
         opslimit  => ARGON2ID_OPSLIMIT,
         memlimit  => ARGON2ID_MEMLIMIT,
         alg       => crypto_pwhash_ALG_ARGON2ID13
      );

      if Status = 0 and then not Is_All_Zero (Key.Data) then
         Key.Valid := True;
         Success := True;
      else
         -- KDF failed or produced all zeros
         Key.Valid := False;
         for I in Key.Data'Range loop
            Key.Data (I) := 0;
         end loop;
         Success := False;
      end if;
   exception
      when others =>
         Key.Valid := False;
         for I in Key.Data'Range loop
            Key.Data (I) := 0;
         end loop;
         Success := False;
   end Derive_XTS_Key_From_Passphrase;

   procedure Zeroize_XTS_Key (
      Key : in out XTS_Key_Wrapper
   ) is
   begin
      Key.Valid := False;
      for I in Key.Data'Range loop
         Key.Data (I) := 0;
      end loop;
   end Zeroize_XTS_Key;

end Anubis_AES_XTS;
