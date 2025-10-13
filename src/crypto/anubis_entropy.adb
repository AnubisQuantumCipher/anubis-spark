-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Secure Entropy Source Implementation
-- Uses libsodium's randombytes_buf (getrandom/arc4random)
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- Uses FFI to libsodium

with System;
with Interfaces.C;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.Classical;

package body Anubis_Entropy is

   -------------------------------------------------------------------------
   -- FFI to libsodium randombytes
   -------------------------------------------------------------------------

   procedure C_Randombytes_Buf (Buf : System.Address; Size : Interfaces.C.size_t)
   with
      Import => True,
      Convention => C,
      External_Name => "randombytes_buf";

   -------------------------------------------------------------------------
   -- Generate Random Bytes
   -------------------------------------------------------------------------

   procedure Generate_Random_Bytes (
      Output  : out Byte_Array;
      Success : out Boolean)
   is
   begin
      -- Call libsodium's secure RNG
      C_Randombytes_Buf (Output'Address, Output'Length);
      Success := True;

      -- Note: libsodium randombytes_buf never fails
      -- It uses getrandom() on Linux, arc4random() on BSD/macOS
   exception
      when others =>
         -- Zeroize output on failure
         for I in Output'Range loop
            Output (I) := 0;
         end loop;
         Success := False;
   end Generate_Random_Bytes;

   -------------------------------------------------------------------------
   -- Fill Random (Alias)
   -------------------------------------------------------------------------

   procedure Fill_Random (
      Buffer  : out Byte_Array;
      Success : out Boolean)
   is
   begin
      Generate_Random_Bytes (Buffer, Success);
   end Fill_Random;

   -------------------------------------------------------------------------
   -- Generate Random Salt
   -------------------------------------------------------------------------

   procedure Generate_Random_Salt (
      Salt    : out Argon2_Salt;
      Success : out Boolean)
   is
   begin
      -- Generate random bytes directly into the salt
      C_Randombytes_Buf (Salt'Address, ARGON2_SALT_SIZE);
      Success := True;
   exception
      when others =>
         Success := False;
   end Generate_Random_Salt;

   -------------------------------------------------------------------------
   -- Generate Random Nonce
   -------------------------------------------------------------------------

   procedure Generate_Random_Nonce (
      Nonce   : out XChaCha20_Nonce;
      Success : out Boolean)
   is
   begin
      -- Generate random bytes directly into the nonce
      C_Randombytes_Buf (Nonce'Address, XCHACHA20_NONCE_SIZE);
      Success := True;
   exception
      when others =>
         Success := False;
   end Generate_Random_Nonce;

   -------------------------------------------------------------------------
   -- Generate Random Key
   -------------------------------------------------------------------------

   procedure Generate_Random_Key (
      Key     : out XChaCha20_Key;
      Success : out Boolean)
   is
   begin
      -- Generate random bytes directly into the key
      C_Randombytes_Buf (Key'Address, XCHACHA20_KEY_SIZE);
      Success := True;
      -- Note: Can't set Valid field without accessing private components
      -- The caller should use Is_Valid or similar function
   exception
      when others =>
         Success := False;
   end Generate_Random_Key;

   -------------------------------------------------------------------------
   -- Test Entropy Source
   -------------------------------------------------------------------------

   function Test_Entropy_Source return Boolean is
      Test_Buf : Byte_Array (1 .. 32);
      Success : Boolean;
   begin
      Generate_Random_Bytes (Test_Buf, Success);

      -- Check that buffer is not all zeros
      if Success then
         for I in Test_Buf'Range loop
            if Test_Buf (I) /= 0 then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Test_Entropy_Source;

   -------------------------------------------------------------------------
   -- Get Entropy Source Name
   -------------------------------------------------------------------------

   function Get_Entropy_Source_Name return String is
   begin
      -- libsodium automatically selects best available source:
      -- - macOS: arc4random_buf (SecRandomCopyBytes)
      -- - Linux: getrandom() syscall
      -- - BSD: getentropy() or arc4random_buf
      -- - Fallback: /dev/urandom
      return "libsodium randombytes (getrandom/arc4random)";
   end Get_Entropy_Source_Name;

   -------------------------------------------------------------------------
   -- Has Hardware RNG
   -------------------------------------------------------------------------

   function Has_Hardware_RNG return Boolean is
   begin
      -- libsodium may use RDRAND/RDSEED on x86 if available
      -- Return conservative answer (not exposed by API)
      return False;
   end Has_Hardware_RNG;

   -------------------------------------------------------------------------
   -- Estimate Entropy Per Byte
   -------------------------------------------------------------------------

   function Estimate_Entropy_Per_Byte return Float is
   begin
      -- Assume full entropy (8.0 bits per byte) from OS RNG
      return 8.0;
   end Estimate_Entropy_Per_Byte;

   -------------------------------------------------------------------------
   -- Detect Entropy Source (Private)
   -------------------------------------------------------------------------

   function Detect_Entropy_Source return Entropy_Source_Type is
   begin
      -- libsodium handles this automatically
      return System_API;
   end Detect_Entropy_Source;

end Anubis_Entropy;
