-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Anti-Forensic (AF) Information Splitter Implementation
-- LUKS2-inspired 4000-stripe diffusion for master key protection
-- Best practices from SPARKNaCl: type-safety, complete zeroization
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- Uses libsodium bindings

with Anubis_Types; use Anubis_Types;
with Interfaces.C; use Interfaces.C;
with System;

package body Anubis_AF_Splitter is

   -------------------------------------------------------------------------
   -- Libsodium FFI Bindings
   -------------------------------------------------------------------------

   -- Cryptographically secure random number generator
   procedure randombytes_buf (
      buf  : out Byte_Array;
      size : size_t
   ) with Import, Convention => C, External_Name => "randombytes_buf";

   -- SHA-256 hashing for diffusion
   function crypto_hash_sha256 (
      out_hash : out Byte_Array;
      input    : Byte_Array;
      inlen    : unsigned_long
   ) return int
   with Import, Convention => C, External_Name => "crypto_hash_sha256";

   -------------------------------------------------------------------------
   -- Internal Helpers
   -------------------------------------------------------------------------

   -- XOR two byte arrays (constant-time)
   procedure XOR_Arrays (
      A      : in     Byte_Array;
      B      : in     Byte_Array;
      Result : out    Byte_Array
   ) with
      Pre => A'Length = B'Length and then
             B'Length = Result'Length;

   procedure XOR_Arrays (
      A      : in     Byte_Array;
      B      : in     Byte_Array;
      Result : out    Byte_Array
   ) is
   begin
      for I in A'Range loop
         Result (I) := A (I) xor B (I - A'First + B'First);
      end loop;
   end XOR_Arrays;

   -- Diffusion function using SHA-256
   -- SPARKNaCl principle: Type-safe cryptographic operations
   procedure Diffuse_Block (
      Input      : in     Byte_Array;
      Salt       : in     AF_Salt;
      Block_Idx  : in     Natural;
      Output     : out    Byte_Array;
      Success    : out    Boolean
   ) with
      Pre => Input'Length = 32 and then
             Output'Length = 32;

   procedure Diffuse_Block (
      Input      : in     Byte_Array;
      Salt       : in     AF_Salt;
      Block_Idx  : in     Natural;
      Output     : out    Byte_Array;
      Success    : out    Boolean
   ) is
      -- Construct input: Salt || Block_Idx || Input
      Hash_Input : Byte_Array (1 .. 32 + 4 + 32);
      Idx_Bytes  : constant Byte_Array (1 .. 4) := (
         Byte (Block_Idx / 16777216),
         Byte ((Block_Idx / 65536) mod 256),
         Byte ((Block_Idx / 256) mod 256),
         Byte (Block_Idx mod 256)
      );
      Status : int;
   begin
      -- Concatenate: Salt || Block_Idx || Input
      Hash_Input (1 .. 32) := Salt;
      Hash_Input (33 .. 36) := Idx_Bytes;
      Hash_Input (37 .. 68) := Input;

      -- Compute SHA-256 hash
      Status := crypto_hash_sha256 (
         out_hash => Output,
         input    => Hash_Input,
         inlen    => unsigned_long (Hash_Input'Length)
      );

      Success := (Status = 0);

      -- Zeroize on failure
      if not Success then
         for I in Output'Range loop
            Output (I) := 0;
         end loop;
      end if;
   exception
      when others =>
         for I in Output'Range loop
            Output (I) := 0;
         end loop;
         Success := False;
   end Diffuse_Block;

   -------------------------------------------------------------------------
   -- AF-Splitter Operations
   -------------------------------------------------------------------------

   procedure AF_Split (
      Master_Key : in     Master_Key_Data;
      Salt       : in     AF_Salt;
      Split_Data : out    AF_Split_Data;
      Success    : out    Boolean
   ) is
      -- Working buffer for diffusion
      Working_Block : Byte_Array (1 .. MASTER_KEY_SIZE);
      Temp_Block    : Byte_Array (1 .. MASTER_KEY_SIZE);
      Diffuse_OK    : Boolean;
   begin
      -- Initialize to zeros (defense-in-depth)
      for I in Split_Data'Range loop
         Split_Data (I) := 0;
      end loop;

      -- Step 1: Generate random data for first 3999 stripes (all-or-nothing)
      -- SPARKNaCl principle: Use system RNG for key material
      begin
         randombytes_buf (
            buf  => Split_Data (1 .. MASTER_KEY_SIZE * (STRIPE_COUNT - 1)),
            size => size_t (MASTER_KEY_SIZE * (STRIPE_COUNT - 1))
         );
      exception
         when others =>
            Success := False;
            return;
      end;

      -- Verify entropy (RNG sanity check)
      if Is_All_Zero (Split_Data (1 .. MASTER_KEY_SIZE * (STRIPE_COUNT - 1))) then
         -- Extremely rare: RNG failure
         for I in Split_Data'Range loop
            Split_Data (I) := 0;
         end loop;
         Success := False;
         return;
      end if;

      -- Step 2: Compute final stripe using diffusion
      -- Final stripe = Master_Key XOR Diffuse(Stripe_0 XOR Stripe_1 XOR ... XOR Stripe_3998)

      -- Initialize working block with master key
      Working_Block := Master_Key;

      -- XOR all previous stripes into working block
      for Stripe_Idx in 0 .. STRIPE_COUNT - 2 loop
         declare
            Stripe_Start : constant Natural := 1 + (Stripe_Idx * MASTER_KEY_SIZE);
            Stripe_End   : constant Natural := Stripe_Start + MASTER_KEY_SIZE - 1;
            Stripe_Data  : constant Byte_Array := Split_Data (Stripe_Start .. Stripe_End);
         begin
            -- Diffuse stripe with salt and index
            Diffuse_Block (
               Input     => Stripe_Data,
               Salt      => Salt,
               Block_Idx => Stripe_Idx,
               Output    => Temp_Block,
               Success   => Diffuse_OK
            );

            if not Diffuse_OK then
               -- Diffusion failed - abort and zeroize
               for I in Split_Data'Range loop
                  Split_Data (I) := 0;
               end loop;
               Success := False;
               return;
            end if;

            -- XOR diffused block into working block
            XOR_Arrays (
               A      => Working_Block,
               B      => Temp_Block,
               Result => Working_Block
            );
         end;
      end loop;

      -- Step 3: Store final stripe (completes diffusion)
      declare
         Final_Stripe_Start : constant Natural := 1 + ((STRIPE_COUNT - 1) * MASTER_KEY_SIZE);
         Final_Stripe_End   : constant Natural := Final_Stripe_Start + MASTER_KEY_SIZE - 1;
      begin
         Split_Data (Final_Stripe_Start .. Final_Stripe_End) := Working_Block;
      end;

      -- Step 4: Verify diffusion property (master key not directly present)
      -- Check first 32 bytes don't match master key
      declare
         Diffusion_Verified : Boolean := True;
      begin
         for I in Master_Key'Range loop
            if Split_Data (I) = Master_Key (I) then
               Diffusion_Verified := False;
               exit;
            end if;
         end loop;

         if not Diffusion_Verified then
            -- Diffusion failed - this should never happen unless RNG is broken
            for I in Split_Data'Range loop
               Split_Data (I) := 0;
            end loop;
            Success := False;
            return;
         end if;
      end;

      -- Success: Master key diffused across 4000 stripes
      Success := True;

   exception
      when others =>
         -- Zeroize on any exception (SPARKNaCl principle: fail-safe)
         for I in Split_Data'Range loop
            Split_Data (I) := 0;
         end loop;
         Success := False;
   end AF_Split;

   procedure AF_Merge (
      Split_Data : in     AF_Split_Data;
      Salt       : in     AF_Salt;
      Master_Key : out    Master_Key_Data;
      Success    : out    Boolean
   ) is
      -- Working buffer for recovery
      Working_Block : Byte_Array (1 .. MASTER_KEY_SIZE);
      Temp_Block    : Byte_Array (1 .. MASTER_KEY_SIZE);
      Diffuse_OK    : Boolean;
   begin
      -- Initialize to zeros (defense-in-depth)
      for I in Master_Key'Range loop
         Master_Key (I) := 0;
      end loop;

      -- Initialize working block with final stripe
      declare
         Final_Stripe_Start : constant Natural := 1 + ((STRIPE_COUNT - 1) * MASTER_KEY_SIZE);
         Final_Stripe_End   : constant Natural := Final_Stripe_Start + MASTER_KEY_SIZE - 1;
      begin
         Working_Block := Split_Data (Final_Stripe_Start .. Final_Stripe_End);
      end;

      -- Step 1: XOR all diffused stripes (reverse diffusion)
      for Stripe_Idx in 0 .. STRIPE_COUNT - 2 loop
         declare
            Stripe_Start : constant Natural := 1 + (Stripe_Idx * MASTER_KEY_SIZE);
            Stripe_End   : constant Natural := Stripe_Start + MASTER_KEY_SIZE - 1;
            Stripe_Data  : constant Byte_Array := Split_Data (Stripe_Start .. Stripe_End);
         begin
            -- Diffuse stripe with salt and index (same as split)
            Diffuse_Block (
               Input     => Stripe_Data,
               Salt      => Salt,
               Block_Idx => Stripe_Idx,
               Output    => Temp_Block,
               Success   => Diffuse_OK
            );

            if not Diffuse_OK then
               -- Diffusion failed - abort and zeroize
               for I in Master_Key'Range loop
                  Master_Key (I) := 0;
               end loop;
               Success := False;
               return;
            end if;

            -- XOR diffused block (undoes the XOR from split)
            XOR_Arrays (
               A      => Working_Block,
               B      => Temp_Block,
               Result => Working_Block
            );
         end;
      end loop;

      -- Step 2: Working block now contains recovered master key
      Master_Key := Working_Block;

      -- Step 3: Verify recovery (master key has entropy)
      if Is_All_Zero (Master_Key) then
         -- Recovery failed: master key is all zeros
         Success := False;
         return;
      end if;

      -- Success: Master key recovered
      Success := True;

   exception
      when others =>
         -- Zeroize on any exception (SPARKNaCl principle: fail-safe)
         for I in Master_Key'Range loop
            Master_Key (I) := 0;
         end loop;
         Success := False;
   end AF_Merge;

   -------------------------------------------------------------------------
   -- Verification Functions
   -------------------------------------------------------------------------

   procedure Verify_AF_Roundtrip (
      Original_Key  : in     Master_Key_Data;
      Salt          : in     AF_Salt;
      Recovered_Key : out    Master_Key_Data;
      Success       : out    Boolean
   ) is
      Split_Data   : AF_Split_Data;
      Split_OK     : Boolean;
      Merge_OK     : Boolean;
   begin
      -- Initialize recovered key to zeros
      for I in Recovered_Key'Range loop
         Recovered_Key (I) := 0;
      end loop;

      -- Step 1: Split original key
      AF_Split (
         Master_Key => Original_Key,
         Salt       => Salt,
         Split_Data => Split_Data,
         Success    => Split_OK
      );

      if not Split_OK then
         Success := False;
         return;
      end if;

      -- Step 2: Merge split data back
      AF_Merge (
         Split_Data => Split_Data,
         Salt       => Salt,
         Master_Key => Recovered_Key,
         Success    => Merge_OK
      );

      if not Merge_OK then
         Success := False;
         return;
      end if;

      -- Step 3: Verify recovered key matches original
      for I in Original_Key'Range loop
         if Recovered_Key (I) /= Original_Key (I) then
            -- Roundtrip verification failed
            for J in Recovered_Key'Range loop
               Recovered_Key (J) := 0;
            end loop;
            Success := False;
            return;
         end if;
      end loop;

      -- Success: Roundtrip verified
      Success := True;

   exception
      when others =>
         for I in Recovered_Key'Range loop
            Recovered_Key (I) := 0;
         end loop;
         Success := False;
   end Verify_AF_Roundtrip;

end Anubis_AF_Splitter;
