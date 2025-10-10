-------------------------------------------------------------------------------
-- ANUBIS-SPARK: libsodium XChaCha20-Poly1305 C Bindings
-- Low-level FFI to XChaCha20-Poly1305 AEAD encryption
-------------------------------------------------------------------------------

pragma Ada_2012;

with Interfaces.C;
with System;

package Sodium_AEAD is
   pragma Preelaborate;

   -------------------------------------------------------------------------
   -- XChaCha20-Poly1305 (IETF variant)
   -- Authenticated Encryption with Associated Data (AEAD)
   -- Extended nonce version (192-bit) - no nonce reuse concerns
   -------------------------------------------------------------------------

   -- Key, nonce, and MAC sizes (bytes)
   crypto_aead_xchacha20poly1305_ietf_KEYBYTES  : constant := 32;
   crypto_aead_xchacha20poly1305_ietf_NPUBBYTES : constant := 24;  -- Nonce (192-bit)
   crypto_aead_xchacha20poly1305_ietf_ABYTES    : constant := 16;  -- Auth tag (MAC)

   -------------------------------------------------------------------------
   -- XChaCha20-Poly1305 Encryption
   -------------------------------------------------------------------------

   -- Encrypt and authenticate message
   -- ciphertext: output buffer (message_len + ABYTES bytes)
   -- ciphertext_len: actual ciphertext length (message_len + 16)
   -- message: plaintext to encrypt
   -- message_len: length of plaintext
   -- additional_data: optional associated data (can be NULL)
   -- additional_data_len: length of associated data (0 if NULL)
   -- nsec: reserved (must be NULL)
   -- nonce: 24 bytes (192-bit nonce, can be random)
   -- key: 32 bytes secret key
   -- Returns 0 on success, -1 on failure
   function crypto_aead_xchacha20poly1305_ietf_encrypt (
      ciphertext          : System.Address;
      ciphertext_len      : access Interfaces.C.unsigned_long;
      message             : System.Address;
      message_len         : Interfaces.C.unsigned_long;
      additional_data     : System.Address;
      additional_data_len : Interfaces.C.unsigned_long;
      nsec                : System.Address;  -- Must be NULL
      nonce               : System.Address;
      key                 : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_aead_xchacha20poly1305_ietf_encrypt";

   -------------------------------------------------------------------------
   -- XChaCha20-Poly1305 Decryption
   -------------------------------------------------------------------------

   -- Decrypt and verify message
   -- message: output buffer (ciphertext_len - ABYTES bytes)
   -- message_len: actual plaintext length (ciphertext_len - 16)
   -- nsec: reserved (must be NULL)
   -- ciphertext: encrypted message with auth tag
   -- ciphertext_len: length of ciphertext (plaintext_len + 16)
   -- additional_data: optional associated data (must match encryption)
   -- additional_data_len: length of associated data
   -- nonce: 24 bytes (must match encryption nonce)
   -- key: 32 bytes secret key
   -- Returns 0 on success (valid MAC), -1 on failure (invalid MAC/tampered)
   function crypto_aead_xchacha20poly1305_ietf_decrypt (
      message             : System.Address;
      message_len         : access Interfaces.C.unsigned_long;
      nsec                : System.Address;  -- Must be NULL
      ciphertext          : System.Address;
      ciphertext_len      : Interfaces.C.unsigned_long;
      additional_data     : System.Address;
      additional_data_len : Interfaces.C.unsigned_long;
      nonce               : System.Address;
      key                 : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_aead_xchacha20poly1305_ietf_decrypt";

   -------------------------------------------------------------------------
   -- XChaCha20-Poly1305 Detached Authentication
   -------------------------------------------------------------------------

   -- Encrypt with detached authentication tag
   -- ciphertext: output buffer (same length as message)
   -- mac: 16 bytes authentication tag output
   -- mac_len: actual MAC length (always 16)
   -- message: plaintext to encrypt
   -- message_len: length of plaintext
   -- additional_data: optional associated data
   -- additional_data_len: length of associated data
   -- nsec: reserved (must be NULL)
   -- nonce: 24 bytes
   -- key: 32 bytes
   -- Returns 0 on success
   function crypto_aead_xchacha20poly1305_ietf_encrypt_detached (
      ciphertext          : System.Address;
      mac                 : System.Address;
      mac_len             : access Interfaces.C.unsigned_long;
      message             : System.Address;
      message_len         : Interfaces.C.unsigned_long;
      additional_data     : System.Address;
      additional_data_len : Interfaces.C.unsigned_long;
      nsec                : System.Address;
      nonce               : System.Address;
      key                 : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_aead_xchacha20poly1305_ietf_encrypt_detached";

   -- Decrypt with detached authentication tag
   -- message: output plaintext
   -- nsec: reserved (must be NULL)
   -- ciphertext: encrypted message (without MAC)
   -- ciphertext_len: length of ciphertext
   -- mac: 16 bytes authentication tag
   -- additional_data: optional associated data (must match encryption)
   -- additional_data_len: length of associated data
   -- nonce: 24 bytes (must match encryption)
   -- key: 32 bytes
   -- Returns 0 if valid, -1 if MAC verification fails
   function crypto_aead_xchacha20poly1305_ietf_decrypt_detached (
      message             : System.Address;
      nsec                : System.Address;
      ciphertext          : System.Address;
      ciphertext_len      : Interfaces.C.unsigned_long;
      mac                 : System.Address;
      additional_data     : System.Address;
      additional_data_len : Interfaces.C.unsigned_long;
      nonce               : System.Address;
      key                 : System.Address
   ) return Interfaces.C.int with
      Import, Convention => C,
      External_Name => "crypto_aead_xchacha20poly1305_ietf_decrypt_detached";

   -------------------------------------------------------------------------
   -- Key Generation
   -------------------------------------------------------------------------

   -- Generate random 32-byte key
   procedure crypto_aead_xchacha20poly1305_ietf_keygen (
      key : System.Address
   ) with
      Import, Convention => C,
      External_Name => "crypto_aead_xchacha20poly1305_ietf_keygen";

end Sodium_AEAD;
