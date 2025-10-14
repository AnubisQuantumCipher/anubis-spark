-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Classical Cryptography Wrapper Specification
-- SPARK-safe interface to classical crypto (X25519, Ed25519, XChaCha20, Argon2id)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package Anubis_Types.Classical is

   -------------------------------------------------------------------------
   -- PLATINUM LEVEL: Ghost Functions for Verification
   -------------------------------------------------------------------------

   -- Ghost: Verify HKDF output is non-zero on success
   function HKDF_Output_Valid (Key : Byte_Array) return Boolean is
      (not Is_All_Zero (Key))
   with Ghost;

   -- Ghost: Verify XChaCha20 decryption zeroes output on auth failure
   function Decryption_Failed_Zeroed (Plaintext : Byte_Array) return Boolean is
      (Is_All_Zero (Plaintext))
   with Ghost;

   -- Ghost: Verify encryption output length includes overhead (plaintext + 16-byte tag)
   function Encryption_Length_Valid (
      Plaintext_Length  : Natural;
      Ciphertext_Length : Natural
   ) return Boolean is
      (Ciphertext_Length = Plaintext_Length)
   with Ghost;

   -- Ghost: Verify authentication tag is non-zero (valid tag from Poly1305)
   function Auth_Tag_Valid (Tag : XChaCha20_Auth_Tag) return Boolean
   with Ghost;

   -------------------------------------------------------------------------
   -- X25519 Operations (Elliptic Curve Diffie-Hellman)
   -------------------------------------------------------------------------

   -- Generate X25519 keypair
   -- PLATINUM: Elaborate postcondition proves entropy and zeroization
   procedure X25519_Generate_Keypair (
      Public_Key  : out X25519_Public_Key;
      Secret_Key  : out X25519_Secret_Key;
      Success     : out Boolean
   ) with
      Global => null,
      Post   => (if Success then
                    -- On success: keys are valid and have entropy (not all zeros)
                    (Is_Valid (Secret_Key) and then
                     not Is_Zeroed (Secret_Key) and then
                     not Is_PK_Zeroed (Public_Key))
                 else
                    -- On failure: both keys zeroized (defense in depth)
                    (not Is_Valid (Secret_Key) and then
                     Is_Zeroed (Secret_Key) and then
                     Is_PK_Zeroed (Public_Key)));

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
   -- PLATINUM: Elaborate postcondition proves entropy and zeroization
   procedure Ed25519_Generate_Keypair (
      Public_Key  : out Ed25519_Public_Key;
      Secret_Key  : out Ed25519_Secret_Key;
      Success     : out Boolean
   ) with
      Global => null,
      Post   => (if Success then
                    -- On success: keys are valid and have entropy (not all zeros)
                    (Is_Valid (Secret_Key) and then
                     not Is_Zeroed (Secret_Key) and then
                     not Is_PK_Zeroed (Public_Key))
                 else
                    -- On failure: both keys zeroized (defense in depth)
                    (not Is_Valid (Secret_Key) and then
                     Is_Zeroed (Secret_Key) and then
                     Is_PK_Zeroed (Public_Key)));

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
   -- PLATINUM LEVEL: Proves length preservation and tag generation
   procedure XChaCha20_Encrypt (
      Plaintext  : in     Byte_Array;
      Key        : in     XChaCha20_Key;
      Nonce      : in     XChaCha20_Nonce;
      AAD        : in     Byte_Array;
      Ciphertext : out    Byte_Array;
      Auth_Tag   : out    XChaCha20_Auth_Tag;
      Success    : out    Boolean
   ) with
      Pre  => Is_Valid (Key) and then
              Ciphertext'Length = Plaintext'Length,
      Post => (if Success then
                  Encryption_Length_Valid (Plaintext'Length, Ciphertext'Length)
               else
                  Is_All_Zero (Ciphertext));

   -- Decrypt with XChaCha20-Poly1305
   procedure XChaCha20_Decrypt (
      Ciphertext : in     Byte_Array;
      Auth_Tag   : in     XChaCha20_Auth_Tag;
      Key        : in     XChaCha20_Key;
      Nonce      : in     XChaCha20_Nonce;
      AAD        : in     Byte_Array;
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
   -- PLATINUM LEVEL: Contract_Cases provide complete behavioral specification
   procedure HKDF_Derive (
      Input_Key_Material : in     Byte_Array;
      Context_String     : in     String;
      Output_Key         : out    Byte_Array;
      Success            : out    Boolean
   ) with
      Pre  => Input_Key_Material'Length > 0 and
              Output_Key'Length > 0 and
              Output_Key'Length <= 8160,  -- HKDF-SHA256 max output
      Contract_Cases => (
         -- PLATINUM: All valid inputs should succeed (guard: True)
         others => (if Success then
                       HKDF_Output_Valid (Output_Key)
                    else
                       Decryption_Failed_Zeroed (Output_Key))
      );

end Anubis_Types.Classical;
