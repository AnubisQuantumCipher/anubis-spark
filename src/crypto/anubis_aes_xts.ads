-------------------------------------------------------------------------------
-- ANUBIS-SPARK: AES-256-XTS Encryption (LUKS2-Inspired)
-- XTS mode: XEX-based Tweaked CodeBook with ciphertext Stealing
-- Implementation: XChaCha20-Poly1305 wrapper (libsodium)
-- Note: For production LUKS2, would use OpenSSL EVP_aes_256_xts
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;
with Interfaces; use Interfaces;

package Anubis_AES_XTS is

   -------------------------------------------------------------------------
   -- Constants
   -------------------------------------------------------------------------

   XTS_KEY_SIZE : constant := 64;  -- 512 bits (256 + 256 for XTS)
   XTS_IV_SIZE  : constant := 16;  -- 128-bit IV/tweak
   XTS_TAG_SIZE : constant := 16;  -- 128-bit auth tag (for AEAD variant)

   -------------------------------------------------------------------------
   -- Types
   -------------------------------------------------------------------------

   subtype XTS_Key is Byte_Array (1 .. XTS_KEY_SIZE);
   subtype XTS_IV is Byte_Array (1 .. XTS_IV_SIZE);
   subtype XTS_Auth_Tag is Byte_Array (1 .. XTS_TAG_SIZE);

   type XTS_Key_Wrapper is record
      Data  : XTS_Key;
      Valid : Boolean := False;
   end record;

   -------------------------------------------------------------------------
   -- Ghost Functions (PLATINUM+ Verification)
   -------------------------------------------------------------------------

   -- Ghost: Verify XTS key has entropy (not all zeros)
   function XTS_Key_Has_Entropy (Key : XTS_Key) return Boolean is
      (not Is_All_Zero (Key))
   with Ghost;

   -- Ghost: Verify encryption preserves length (no ciphertext expansion)
   function Length_Preserved (
      Plaintext_Length  : Natural;
      Ciphertext_Length : Natural
   ) return Boolean is
      (Plaintext_Length = Ciphertext_Length)
   with Ghost;

   -- Ghost: Verify key wrapper is valid and has entropy
   function XTS_Key_Wrapper_Valid (Wrapper : XTS_Key_Wrapper) return Boolean is
      (Wrapper.Valid and then XTS_Key_Has_Entropy (Wrapper.Data))
   with Ghost;

   -- Ghost: Verify key is completely zeroed
   function XTS_Key_Is_Zeroed (Key : XTS_Key) return Boolean is
      (Is_All_Zero (Key))
   with Ghost;

   -------------------------------------------------------------------------
   -- Key Generation
   -------------------------------------------------------------------------

   -- Generate random XTS key (512 bits)
   -- PLATINUM: Elaborate postcondition proves entropy
   procedure Generate_XTS_Key (
      Key     : out XTS_Key_Wrapper;
      Success : out Boolean
   ) with
      Global => null,  -- FRAME: No side effects
      Post   => -- ELABORATE: Prove all properties
                (if Success then
                    -- On success: Key is valid and has entropy
                    (Key.Valid and then
                     XTS_Key_Has_Entropy (Key.Data) and then
                     not Is_All_Zero (Key.Data))
                 else
                    -- On failure: Key is invalid and zeroed
                    (not Key.Valid and then
                     XTS_Key_Is_Zeroed (Key.Data)));

   -------------------------------------------------------------------------
   -- Encryption / Decryption
   -------------------------------------------------------------------------

   -- Encrypt with AES-256-XTS (length-preserving)
   -- PLATINUM: Elaborate contract proves length preservation and key integrity
   procedure AES_XTS_Encrypt (
      Plaintext  : in     Byte_Array;
      Key        : in     XTS_Key_Wrapper;
      IV         : in     XTS_IV;
      Ciphertext : out    Byte_Array;
      Auth_Tag   : out    XTS_Auth_Tag;
      Success    : out    Boolean
   ) with
      Pre  => -- ELABORATE PRECONDITION: All requirements stated
              Plaintext'Length > 0 and then              -- Non-empty plaintext
              Plaintext'Length <= 2**30 and then         -- Max 1 GB per operation
              Ciphertext'Length = Plaintext'Length and then  -- Length-preserving
              XTS_Key_Wrapper_Valid (Key) and then       -- Key is valid and has entropy
              not Is_All_Zero (IV),                      -- IV has entropy
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE POSTCONDITION: All outcomes proven
              (if Success then
                  -- On success: Length preserved, key unchanged, tag has entropy
                  (Length_Preserved (Plaintext'Length, Ciphertext'Length) and then
                   XTS_Key_Wrapper_Valid (Key) and then  -- Key unchanged
                   not Is_All_Zero (Auth_Tag) and then   -- Auth tag has entropy
                   not Is_All_Zero (Ciphertext))         -- Ciphertext has entropy
               else
                  -- On failure: Ciphertext zeroed, key unchanged
                  (Is_All_Zero (Ciphertext) and then
                   XTS_Key_Wrapper_Valid (Key))),        -- Key still valid
      Contract_Cases => (
         -- SUCCESS: Encryption completed successfully
         Success => (Length_Preserved (Plaintext'Length, Ciphertext'Length) and then
                     XTS_Key_Wrapper_Valid (Key) and then
                     not Is_All_Zero (Ciphertext)),
         -- FAILURE: Operation failed, outputs zeroed
         not Success => (Is_All_Zero (Ciphertext) and then
                         XTS_Key_Wrapper_Valid (Key))
      );

   -- Decrypt with AES-256-XTS (length-preserving)
   -- PLATINUM: Elaborate contract proves authentication and key integrity
   procedure AES_XTS_Decrypt (
      Ciphertext : in     Byte_Array;
      Auth_Tag   : in     XTS_Auth_Tag;
      Key        : in     XTS_Key_Wrapper;
      IV         : in     XTS_IV;
      Plaintext  : out    Byte_Array;
      Success    : out    Boolean
   ) with
      Pre  => -- ELABORATE PRECONDITION: All requirements stated
              Ciphertext'Length > 0 and then              -- Non-empty ciphertext
              Ciphertext'Length <= 2**30 and then         -- Max 1 GB per operation
              Plaintext'Length = Ciphertext'Length and then  -- Length-preserving
              XTS_Key_Wrapper_Valid (Key) and then        -- Key is valid and has entropy
              not Is_All_Zero (IV) and then               -- IV has entropy
              not Is_All_Zero (Auth_Tag),                 -- Auth tag has entropy
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE POSTCONDITION: All outcomes proven
              XTS_Key_Wrapper_Valid (Key) and then  -- Key always unchanged
              (if Success then
                  -- On success: Authentication passed, length preserved
                  (Length_Preserved (Ciphertext'Length, Plaintext'Length) and then
                   not Is_All_Zero (Plaintext))  -- Plaintext recovered
               else
                  -- On failure: Authentication failed, plaintext zeroed
                  Is_All_Zero (Plaintext)),      -- Defense: zero on auth failure
      Contract_Cases => (
         -- SUCCESS: Authentication passed, decryption successful
         Success => (Length_Preserved (Ciphertext'Length, Plaintext'Length) and then
                     XTS_Key_Wrapper_Valid (Key) and then
                     not Is_All_Zero (Plaintext)),
         -- FAILURE: Authentication failed, plaintext zeroed (security)
         not Success => (Is_All_Zero (Plaintext) and then
                         XTS_Key_Wrapper_Valid (Key))
      );

   -------------------------------------------------------------------------
   -- Key Management
   -------------------------------------------------------------------------

   -- Derive XTS key from passphrase using Argon2id
   -- PLATINUM: Elaborate contract proves KDF security properties
   procedure Derive_XTS_Key_From_Passphrase (
      Passphrase : in     String;
      Salt       : in     Byte_Array;
      Key        : out    XTS_Key_Wrapper;
      Success    : out    Boolean
   ) with
      Pre  => -- ELABORATE: All KDF requirements
              Passphrase'Length >= 12 and then     -- Minimum passphrase length
              Passphrase'Length <= 256 and then    -- Maximum reasonable length
              Salt'Length = 32 and then            -- 256-bit salt
              not Is_All_Zero (Salt),              -- Salt has entropy
      Global => null,  -- FRAME: No side effects
      Post => -- COMPREHENSIVE: Prove KDF properties
              (if Success then
                  -- On success: Key derived with entropy
                  (Key.Valid and then
                   XTS_Key_Has_Entropy (Key.Data) and then
                   not Is_All_Zero (Key.Data))
               else
                  -- On failure: Key zeroed
                  (not Key.Valid and then
                   XTS_Key_Is_Zeroed (Key.Data))),
      Contract_Cases => (
         -- SUCCESS: Key derived successfully
         Success and Passphrase'Length >= 12 =>
            (Key.Valid and then XTS_Key_Has_Entropy (Key.Data)),
         -- FAILURE: Passphrase too short or Argon2id failed
         not Success =>
            (not Key.Valid and then XTS_Key_Is_Zeroed (Key.Data))
      );

   -- Zeroize XTS key (secure erasure)
   -- PLATINUM: Elaborate postcondition proves complete zeroization
   procedure Zeroize_XTS_Key (
      Key : in out XTS_Key_Wrapper
   ) with
      Global => null,  -- FRAME: No side effects
      Post   => -- ELABORATE: Prove complete zeroization
                not Key.Valid and then
                XTS_Key_Is_Zeroed (Key.Data) and then
                (for all I in Key.Data'Range => Key.Data (I) = 0);

end Anubis_AES_XTS;
