-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Classical Cryptography Wrapper Specification
-- SPARK-safe interface to classical crypto (X25519, Ed25519, XChaCha20, Argon2id)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package Anubis_Types.Classical is

   -------------------------------------------------------------------------
   -- X25519 Operations (Elliptic Curve Diffie-Hellman)
   -------------------------------------------------------------------------

   -- Generate X25519 keypair
   procedure X25519_Generate_Keypair (
      Public_Key  : out X25519_Public_Key;
      Secret_Key  : out X25519_Secret_Key;
      Success     : out Boolean
   ) with
      Global => null,
      Post   => (if Success then Is_Valid (Secret_Key)
                 else not Is_Valid (Secret_Key));

   -- Compute shared secret (ECDH)
   procedure X25519_Compute_Shared (
      Our_Secret_Key   : in     X25519_Secret_Key;
      Their_Public_Key : in     X25519_Public_Key;
      Shared_Secret    : out    X25519_Shared_Secret;
      Success          : out    Boolean
   ) with
      Pre  => Is_Valid (Our_Secret_Key),
      Post => (if Success then Is_Valid (Shared_Secret)
               else not Is_Valid (Shared_Secret));

   -- Zeroize X25519 secret key
   procedure Zeroize_X25519_Secret (
      Secret_Key : in out X25519_Secret_Key
   ) with
      Post => not Is_Valid (Secret_Key);

   -- Zeroize X25519 shared secret
   procedure Zeroize_X25519_Shared (
      Shared_Secret : in out X25519_Shared_Secret
   ) with
      Post => not Is_Valid (Shared_Secret);

   -------------------------------------------------------------------------
   -- Ed25519 Operations (Digital Signatures)
   -------------------------------------------------------------------------

   -- Generate Ed25519 keypair
   procedure Ed25519_Generate_Keypair (
      Public_Key  : out Ed25519_Public_Key;
      Secret_Key  : out Ed25519_Secret_Key;
      Success     : out Boolean
   ) with
      Global => null,
      Post   => (if Success then Is_Valid (Secret_Key)
                 else not Is_Valid (Secret_Key));

   -- Sign message with Ed25519
   procedure Ed25519_Sign (
      Message    : in     Byte_Array;
      Secret_Key : in     Ed25519_Secret_Key;
      Signature  : out    Ed25519_Signature;
      Success    : out    Boolean
   ) with
      Pre => Is_Valid (Secret_Key);

   -- Verify Ed25519 signature
   function Ed25519_Verify (
      Message    : Byte_Array;
      Signature  : Ed25519_Signature;
      Public_Key : Ed25519_Public_Key
   ) return Boolean;

   -- Zeroize Ed25519 secret key
   procedure Zeroize_Ed25519_Secret (
      Secret_Key : in out Ed25519_Secret_Key
   ) with
      Post => not Is_Valid (Secret_Key);

   -------------------------------------------------------------------------
   -- XChaCha20-Poly1305 AEAD Encryption
   -------------------------------------------------------------------------

   -- Generate random XChaCha20 key
   procedure XChaCha20_Generate_Key (
      Key     : out XChaCha20_Key;
      Success : out Boolean
   ) with
      Global => null,
      Post   => (if Success then Is_Valid (Key)
                 else not Is_Valid (Key));

   -- Encrypt with XChaCha20-Poly1305
   procedure XChaCha20_Encrypt (
      Plaintext  : in     Byte_Array;
      Key        : in     XChaCha20_Key;
      Nonce      : in     XChaCha20_Nonce;
      Ciphertext : out    Byte_Array;
      Auth_Tag   : out    XChaCha20_Auth_Tag;
      Success    : out    Boolean
   ) with
      Pre  => Is_Valid (Key) and then
              Ciphertext'Length = Plaintext'Length;

   -- Decrypt with XChaCha20-Poly1305
   procedure XChaCha20_Decrypt (
      Ciphertext : in     Byte_Array;
      Auth_Tag   : in     XChaCha20_Auth_Tag;
      Key        : in     XChaCha20_Key;
      Nonce      : in     XChaCha20_Nonce;
      Plaintext  : out    Byte_Array;
      Success    : out    Boolean
   ) with
      Pre  => Is_Valid (Key) and then
              Plaintext'Length = Ciphertext'Length,
      Post => (if not Success then
                 (for all I in Plaintext'Range => Plaintext (I) = 0));

   -- Zeroize XChaCha20 key
   procedure Zeroize_XChaCha20_Key (
      Key : in out XChaCha20_Key
   ) with
      Post => not Is_Valid (Key);

   -------------------------------------------------------------------------
   -- Argon2id Key Derivation
   -------------------------------------------------------------------------

   -- Derive key from passphrase using Argon2id
   procedure Argon2id_Derive_Key (
      Passphrase : in     String;
      Salt       : in     Argon2_Salt;
      Key        : out    Argon2_Derived_Key;
      Success    : out    Boolean
   ) with
      Pre  => Passphrase'Length >= 8,
      Post => (if Success then Is_Valid (Key)
               else not Is_Valid (Key));

   -- Zeroize Argon2id derived key
   procedure Zeroize_Argon2_Key (
      Key : in out Argon2_Derived_Key
   ) with
      Post => not Is_Valid (Key);

   -------------------------------------------------------------------------
   -- HKDF Key Derivation
   -------------------------------------------------------------------------

   -- Derive key using HKDF-SHA256
   procedure HKDF_Derive (
      Input_Key_Material : in     Byte_Array;
      Context_String     : in     String;
      Output_Key         : out    Byte_Array;
      Success            : out    Boolean
   ) with
      Pre  => Output_Key'Length <= 8160,  -- HKDF-SHA256 max output
      Post => (if not Success then
                 (for all I in Output_Key'Range => Output_Key (I) = 0));

end Anubis_Types.Classical;
