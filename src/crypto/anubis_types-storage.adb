-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Key Storage Implementation
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;
with Interfaces; use Interfaces;
with Interfaces.C;
with Sodium_Hash;
with Anubis_Types.Classical;
with Anubis_Types.PQC;
with Sodium_Pwhash;
with Sodium_Common;
with System;

package body Anubis_Types.Storage is

   MAGIC : constant Byte_Array (1 .. 8) := (65, 78, 85, 66, 73, 83, 75, 1);  -- "ANUBISK\x01"
   VERSION : constant Byte_Array (1 .. 2) := (0, 1);
   KEY_TYPE_IDENTITY : constant Byte := 1;

   -------------------------------------------------------------------------
   -- Generate Identity
   -------------------------------------------------------------------------

   procedure Generate_Identity (
      Identity : out    Identity_Keypair;
      Success  : out    Boolean
   ) is
      Op_Success : Boolean;
   begin
      -- Generate X25519 keypair
      Classical.X25519_Generate_Keypair (
         Public_Key => Identity.X25519_PK,
         Secret_Key => Identity.X25519_SK,
         Success    => Op_Success
      );

      if not Op_Success then
         Success := False;
         return;
      end if;

      -- Generate Ed25519 keypair
      Classical.Ed25519_Generate_Keypair (
         Public_Key => Identity.Ed25519_PK,
         Secret_Key => Identity.Ed25519_SK,
         Success    => Op_Success
      );

      if not Op_Success then
         Classical.Zeroize_X25519_Secret (Identity.X25519_SK);
         Success := False;
         return;
      end if;

      -- Generate ML-KEM-1024 keypair
      PQC.ML_KEM_Generate_Keypair (
         Public_Key => Identity.ML_KEM_PK,
         Secret_Key => Identity.ML_KEM_SK,
         Success    => Op_Success
      );

      if not Op_Success then
         Classical.Zeroize_X25519_Secret (Identity.X25519_SK);
         Classical.Zeroize_Ed25519_Secret (Identity.Ed25519_SK);
         Success := False;
         return;
      end if;

      -- Generate ML-DSA-87 keypair
      PQC.ML_DSA_Generate_Keypair (
         Public_Key => Identity.ML_DSA_PK,
         Secret_Key => Identity.ML_DSA_SK,
         Success    => Op_Success
      );

      if not Op_Success then
         Classical.Zeroize_X25519_Secret (Identity.X25519_SK);
         Classical.Zeroize_Ed25519_Secret (Identity.Ed25519_SK);
         PQC.Zeroize_ML_KEM_Secret (Identity.ML_KEM_SK);
         Success := False;
         return;
      end if;

      Identity.Valid := True;
      Success := True;
   end Generate_Identity;

   -------------------------------------------------------------------------
   -- Save Identity
   -------------------------------------------------------------------------

   procedure Save_Identity (
      Identity : in     Identity_Keypair;
      Filename : in     String;
      Success  : out    Boolean
   ) is
      File_Handle : File_Type;
      File_Stream : Stream_Access;
   begin
      if not Identity.Valid then
         Success := False;
         return;
      end if;

      begin
         Create (File_Handle, Out_File, Filename);
      exception
         when others =>
            Success := False;
            return;
      end;

      File_Stream := Stream (File_Handle);

      -- Write magic
      for I in MAGIC'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (MAGIC (I)));
      end loop;

      -- Write version
      for I in VERSION'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (VERSION (I)));
      end loop;

      -- Write key type
      Stream_Element'Write (File_Stream, Stream_Element (KEY_TYPE_IDENTITY));

      -- Write X25519 keys
      for I in Identity.X25519_PK.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Identity.X25519_PK.Data (I)));
      end loop;
      for I in Identity.X25519_SK.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Identity.X25519_SK.Data (I)));
      end loop;

      -- Write ML-KEM keys
      for I in Identity.ML_KEM_PK.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Identity.ML_KEM_PK.Data (I)));
      end loop;
      for I in Identity.ML_KEM_SK.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Identity.ML_KEM_SK.Data (I)));
      end loop;

      -- Write Ed25519 keys
      for I in Identity.Ed25519_PK.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Identity.Ed25519_PK.Data (I)));
      end loop;
      for I in Identity.Ed25519_SK.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Identity.Ed25519_SK.Data (I)));
      end loop;

      -- Write ML-DSA keys
      for I in Identity.ML_DSA_PK.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Identity.ML_DSA_PK.Data (I)));
      end loop;
      for I in Identity.ML_DSA_SK.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Identity.ML_DSA_SK.Data (I)));
      end loop;

      Close (File_Handle);
      Success := True;
   end Save_Identity;

   -------------------------------------------------------------------------
   -- Load Identity
   -------------------------------------------------------------------------

   procedure Load_Identity (
      Filename : in     String;
      Identity : out    Identity_Keypair;
      Success  : out    Boolean
   ) is
      File_Handle : File_Type;
      File_Stream : Stream_Access;
      Magic_Bytes : Byte_Array (1 .. 8);
      Version_Bytes : Byte_Array (1 .. 2);
      Key_Type : Byte;
      Stream_Byte : Stream_Element;
   begin
      begin
         Open (File_Handle, In_File, Filename);
      exception
         when others =>
            Success := False;
            return;
      end;

      File_Stream := Stream (File_Handle);

      -- Read and verify magic
      for I in Magic_Bytes'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Magic_Bytes (I) := Byte (Stream_Byte);
      end loop;

      if Magic_Bytes /= MAGIC then
         Close (File_Handle);
         Success := False;
         return;
      end if;

      -- Read and verify version
      for I in Version_Bytes'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Version_Bytes (I) := Byte (Stream_Byte);
      end loop;

      if Version_Bytes /= VERSION then
         Close (File_Handle);
         Success := False;
         return;
      end if;

      -- Read and verify key type
      Stream_Element'Read (File_Stream, Stream_Byte);
      Key_Type := Byte (Stream_Byte);

      if Key_Type /= KEY_TYPE_IDENTITY then
         Close (File_Handle);
         Success := False;
         return;
      end if;

      -- Read X25519 keys
      for I in Identity.X25519_PK.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Identity.X25519_PK.Data (I) := Byte (Stream_Byte);
      end loop;
      for I in Identity.X25519_SK.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Identity.X25519_SK.Data (I) := Byte (Stream_Byte);
      end loop;
      Identity.X25519_SK.Valid := True;

      -- Read ML-KEM keys
      for I in Identity.ML_KEM_PK.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Identity.ML_KEM_PK.Data (I) := Byte (Stream_Byte);
      end loop;
      for I in Identity.ML_KEM_SK.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Identity.ML_KEM_SK.Data (I) := Byte (Stream_Byte);
      end loop;
      Identity.ML_KEM_SK.Valid := True;

      -- Read Ed25519 keys
      for I in Identity.Ed25519_PK.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Identity.Ed25519_PK.Data (I) := Byte (Stream_Byte);
      end loop;
      for I in Identity.Ed25519_SK.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Identity.Ed25519_SK.Data (I) := Byte (Stream_Byte);
      end loop;
      Identity.Ed25519_SK.Valid := True;

      -- Read ML-DSA keys
      for I in Identity.ML_DSA_PK.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Identity.ML_DSA_PK.Data (I) := Byte (Stream_Byte);
      end loop;
      for I in Identity.ML_DSA_SK.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Identity.ML_DSA_SK.Data (I) := Byte (Stream_Byte);
      end loop;
      Identity.ML_DSA_SK.Valid := True;

      Close (File_Handle);
      Identity.Valid := True;
      Success := True;
   end Load_Identity;

   -------------------------------------------------------------------------
   -- Accessors
   -------------------------------------------------------------------------

   function Get_X25519_Public (Identity : Identity_Keypair) return X25519_Public_Key is
   begin
      return Identity.X25519_PK;
   end Get_X25519_Public;

   function Get_X25519_Secret (Identity : Identity_Keypair) return X25519_Secret_Key is
   begin
      return Identity.X25519_SK;
   end Get_X25519_Secret;

   function Get_ML_KEM_Public (Identity : Identity_Keypair) return ML_KEM_Public_Key is
   begin
      return Identity.ML_KEM_PK;
   end Get_ML_KEM_Public;

   function Get_ML_KEM_Secret (Identity : Identity_Keypair) return ML_KEM_Secret_Key is
   begin
      return Identity.ML_KEM_SK;
   end Get_ML_KEM_Secret;

   function Get_Ed25519_Public (Identity : Identity_Keypair) return Ed25519_Public_Key is
   begin
      return Identity.Ed25519_PK;
   end Get_Ed25519_Public;

   function Get_Ed25519_Secret (Identity : Identity_Keypair) return Ed25519_Secret_Key is
   begin
      return Identity.Ed25519_SK;
   end Get_Ed25519_Secret;

   function Get_ML_DSA_Public (Identity : Identity_Keypair) return ML_DSA_Public_Key is
   begin
      return Identity.ML_DSA_PK;
   end Get_ML_DSA_Public;

   function Get_ML_DSA_Secret (Identity : Identity_Keypair) return ML_DSA_Secret_Key is
   begin
      return Identity.ML_DSA_SK;
   end Get_ML_DSA_Secret;

   function Is_Valid_Label_Input (Source : String) return Boolean is
      function Is_Printable (C : Character) return Boolean is
      begin
         declare
            Code : constant Natural := Character'Pos (C);
         begin
            return Code >= 16#20# and Code <= 16#7E#;
         end;
      end Is_Printable;
   begin
      if Source'Length > SIGNER_LABEL_SIZE then
         return False;
      end if;

      for C of Source loop
         if not Is_Printable (C) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Label_Input;

   function Make_Label (Source : String) return Signer_Label is
      Result : Signer_Label := (others => 0);
      Copy_Length : constant Natural :=
        (if Source'Length < SIGNER_LABEL_SIZE then Source'Length else SIGNER_LABEL_SIZE);
   begin
      for I in 1 .. Copy_Length loop
         Result (I) := Byte (Character'Pos (Source (Source'First + I - 1)));
      end loop;
      return Result;
   end Make_Label;

   function Default_Label return Signer_Label is
   begin
      return Make_Label ("default");
   end Default_Label;

   function Label_To_String (Label : Signer_Label) return String is
      use type Byte;
      Last_Index : Natural := 0;
   begin
      for I in Label'Range loop
         exit when Label (I) = 0;
         Last_Index := I;
      end loop;

      if Last_Index = 0 then
         return "";
      else
         declare
            Result_Str : String (1 .. Last_Index);
         begin
            for I in 1 .. Last_Index loop
               Result_Str (I) := Character'Val (Integer (Label (I)));
            end loop;
            return Result_Str;
         end;
      end if;
   end Label_To_String;

   function Compute_Fingerprint (Identity : Identity_Keypair) return Signer_Fingerprint is
      use type Interfaces.C.int;
      Buffer_Length : constant Natural :=
        X25519_KEY_SIZE +
        ML_KEM_1024_PUBLIC_KEY_SIZE +
        ED25519_KEY_SIZE +
        ML_DSA_87_PUBLIC_KEY_SIZE;
      Buffer : Byte_Array (1 .. Buffer_Length);
      Offset : Natural := Buffer'First;
      Fingerprint : Signer_Fingerprint := (others => 0);
      Rc : Interfaces.C.int;
   begin
      if not Identity.Valid then
         return Fingerprint;
      end if;

      Buffer (Offset .. Offset + X25519_KEY_SIZE - 1) := Identity.X25519_PK.Data;
      Offset := Offset + X25519_KEY_SIZE;

      Buffer (Offset .. Offset + ML_KEM_1024_PUBLIC_KEY_SIZE - 1) := Identity.ML_KEM_PK.Data;
      Offset := Offset + ML_KEM_1024_PUBLIC_KEY_SIZE;

      Buffer (Offset .. Offset + ED25519_KEY_SIZE - 1) := Identity.Ed25519_PK.Data;
      Offset := Offset + ED25519_KEY_SIZE;

      Buffer (Offset .. Offset + ML_DSA_87_PUBLIC_KEY_SIZE - 1) := Identity.ML_DSA_PK.Data;

      Rc := Sodium_Hash.crypto_generichash (
         Output     => Fingerprint (Fingerprint'First)'Address,
         Output_Len => Interfaces.C.size_t (Fingerprint'Length),
         Input      => Buffer (Buffer'First)'Address,
         Input_Len  => Interfaces.C.unsigned_long_long (Buffer'Length),
         Key        => System.Null_Address,
         Key_Len    => 0
      );

      if Rc /= 0 then
         Fingerprint := (others => 0);
      end if;

      return Fingerprint;
   end Compute_Fingerprint;

   -------------------------------------------------------------------------
   -- Encrypted Keystore (Argon2id + XChaCha20-Poly1305)
   -------------------------------------------------------------------------

   procedure Save_Identity_Encrypted (
      Identity   : in     Identity_Keypair;
      Filename   : in     String;
      Passphrase : in     String;
      Success    : out    Boolean
   ) is
      use Interfaces.C;
      use Sodium_Pwhash;
      use Sodium_Common;
      use Classical;

      File_Handle : File_Type;
      File_Stream : Stream_Access;

      -- Magic and version for encrypted format
      MAGIC_ENCRYPTED : constant Byte_Array (1 .. 8) := (65, 78, 85, 66, 73, 83, 75, 50);  -- "ANUBISK2"
      VERSION_ENCRYPTED : constant Byte_Array (1 .. 2) := (0, 2);

      -- KDF parameters (SENSITIVE: 1 GiB RAM, 4 iterations)
      Opslimit : constant unsigned_long := unsigned_long (crypto_pwhash_OPSLIMIT_SENSITIVE);
      Memlimit : constant size_t := size_t (crypto_pwhash_MEMLIMIT_SENSITIVE);

      -- Cryptographic materials
      Salt : Byte_Array (1 .. 16);
      Nonce : XChaCha20_Nonce;
      KEK : XChaCha20_Key;  -- Key Encryption Key derived from passphrase

      -- Plaintext identity data (12,352 bytes)
      Plaintext_Size : constant := 12_352;
      Plaintext : Byte_Array (1 .. Plaintext_Size);
      Ciphertext : Byte_Array (1 .. Plaintext_Size);
      Auth_Tag : Poly1305_Tag;

      Idx : Natural := 1;
      Rc : int;
      Op_Success : Boolean;

   begin
      if not Identity.Valid then
         Success := False;
         return;
      end if;

      -- Step 1: Serialize identity to plaintext buffer
      -- X25519 keys
      Plaintext (Idx .. Idx + 31) := Identity.X25519_PK.Data;
      Idx := Idx + 32;
      Plaintext (Idx .. Idx + 31) := Identity.X25519_SK.Data;
      Idx := Idx + 32;

      -- ML-KEM keys
      Plaintext (Idx .. Idx + 1567) := Identity.ML_KEM_PK.Data;
      Idx := Idx + 1568;
      Plaintext (Idx .. Idx + 3167) := Identity.ML_KEM_SK.Data;
      Idx := Idx + 3168;

      -- Ed25519 keys
      Plaintext (Idx .. Idx + 31) := Identity.Ed25519_PK.Data;
      Idx := Idx + 32;
      Plaintext (Idx .. Idx + 31) := Identity.Ed25519_SK.Data;
      Idx := Idx + 32;

      -- ML-DSA keys
      Plaintext (Idx .. Idx + 2591) := Identity.ML_DSA_PK.Data;
      Idx := Idx + 2592;
      Plaintext (Idx .. Idx + 4895) := Identity.ML_DSA_SK.Data;

      -- Step 2: Generate random salt and nonce
      randombytes_buf (
         buf  => Salt (Salt'First)'Address,
         size => size_t (Salt'Length)
      );

      randombytes_buf (
         buf  => Nonce.Data (Nonce.Data'First)'Address,
         size => size_t (Nonce.Data'Length)
      );

      -- Step 3: Derive KEK from passphrase using Argon2id
      Rc := crypto_pwhash (
         out_key    => KEK.Data (KEK.Data'First)'Address,
         outlen     => unsigned_long (KEK.Data'Length),
         password   => Passphrase (Passphrase'First)'Address,
         password_len => unsigned_long (Passphrase'Length),
         salt       => Salt (Salt'First)'Address,
         opslimit   => Opslimit,
         memlimit   => Memlimit,
         alg        => crypto_pwhash_ALG_DEFAULT
      );

      if Rc /= 0 then
         -- Argon2id failed (out of memory or invalid params)
         Zeroize_XChaCha20_Key (KEK);
         Success := False;
         return;
      end if;

      KEK.Valid := True;

      -- Step 4: Encrypt plaintext with KEK
      XChaCha20_Encrypt (
         Plaintext  => Plaintext,
         Key        => KEK,
         Nonce      => Nonce,
         AAD        => Salt,  -- Use salt as AAD for additional binding
         Ciphertext => Ciphertext,
         Auth_Tag   => Auth_Tag,
         Success    => Op_Success
      );

      Zeroize_XChaCha20_Key (KEK);

      if not Op_Success then
         Success := False;
         return;
      end if;

      -- Step 5: Write encrypted keystore to file
      begin
         Create (File_Handle, Out_File, Filename);
      exception
         when others =>
            Success := False;
            return;
      end;

      File_Stream := Stream (File_Handle);

      -- Write magic "ANUBISK2"
      for I in MAGIC_ENCRYPTED'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (MAGIC_ENCRYPTED (I)));
      end loop;

      -- Write version 0x0002
      for I in VERSION_ENCRYPTED'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (VERSION_ENCRYPTED (I)));
      end loop;

      -- Write KDF parameters (16 bytes: opslimit u64, memlimit u64)
      declare
         Params : Byte_Array (1 .. 16);
         OpsU64 : constant Unsigned_64 := Unsigned_64 (Opslimit);
         MemU64 : constant Unsigned_64 := Unsigned_64 (Memlimit);
      begin
         -- Opslimit (8 bytes, big-endian)
         for I in 0 .. 7 loop
            Params (I + 1) := Byte (Shift_Right (OpsU64, 56 - I * 8) and 16#FF#);
         end loop;

         -- Memlimit (8 bytes, big-endian)
         for I in 0 .. 7 loop
            Params (I + 9) := Byte (Shift_Right (MemU64, 56 - I * 8) and 16#FF#);
         end loop;

         for I in Params'Range loop
            Stream_Element'Write (File_Stream, Stream_Element (Params (I)));
         end loop;
      end;

      -- Write salt (16 bytes)
      for I in Salt'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Salt (I)));
      end loop;

      -- Write nonce (24 bytes)
      for I in Nonce.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Nonce.Data (I)));
      end loop;

      -- Write ciphertext length (4 bytes, u32)
      declare
         Len : constant Unsigned_32 := Unsigned_32 (Ciphertext'Length);
      begin
         Stream_Element'Write (File_Stream, Stream_Element (Shift_Right (Len, 24) and 16#FF#));
         Stream_Element'Write (File_Stream, Stream_Element (Shift_Right (Len, 16) and 16#FF#));
         Stream_Element'Write (File_Stream, Stream_Element (Shift_Right (Len, 8) and 16#FF#));
         Stream_Element'Write (File_Stream, Stream_Element (Len and 16#FF#));
      end;

      -- Write ciphertext
      for I in Ciphertext'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Ciphertext (I)));
      end loop;

      -- Write authentication tag (16 bytes)
      for I in Auth_Tag.Data'Range loop
         Stream_Element'Write (File_Stream, Stream_Element (Auth_Tag.Data (I)));
      end loop;

      Close (File_Handle);
      Success := True;

   end Save_Identity_Encrypted;

   procedure Load_Identity_Encrypted (
      Filename   : in     String;
      Passphrase : in     String;
      Identity   : out    Identity_Keypair;
      Success    : out    Boolean
   ) is
      use Interfaces.C;
      use Sodium_Pwhash;
      use Classical;

      File_Handle : File_Type;
      File_Stream : Stream_Access;
      Stream_Byte : Stream_Element;

      -- Expected magic and version
      MAGIC_ENCRYPTED : constant Byte_Array (1 .. 8) := (65, 78, 85, 66, 73, 83, 75, 50);  -- "ANUBISK2"
      VERSION_ENCRYPTED : constant Byte_Array (1 .. 2) := (0, 2);

      -- File header data
      Magic_Bytes : Byte_Array (1 .. 8);
      Version_Bytes : Byte_Array (1 .. 2);
      KDF_Params : Byte_Array (1 .. 16);
      Salt : Byte_Array (1 .. 16);
      Nonce : XChaCha20_Nonce;
      CT_Len_Bytes : Byte_Array (1 .. 4);
      CT_Len : Unsigned_32;

      -- Cryptographic materials
      Opslimit : unsigned_long;
      Memlimit : size_t;
      KEK : XChaCha20_Key;

      -- Ciphertext and plaintext buffers
      Plaintext_Size : constant := 12_352;
      Ciphertext : Byte_Array (1 .. Plaintext_Size);
      Auth_Tag : Poly1305_Tag;
      Plaintext : Byte_Array (1 .. Plaintext_Size);

      Idx : Natural := 1;
      Rc : int;
      Op_Success : Boolean;

   begin
      -- Step 1: Open and read file header
      begin
         Open (File_Handle, In_File, Filename);
      exception
         when others =>
            Success := False;
            return;
      end;

      File_Stream := Stream (File_Handle);

      -- Read and verify magic
      for I in Magic_Bytes'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Magic_Bytes (I) := Byte (Stream_Byte);
      end loop;

      if Magic_Bytes /= MAGIC_ENCRYPTED then
         Close (File_Handle);
         Success := False;
         return;
      end if;

      -- Read and verify version
      for I in Version_Bytes'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Version_Bytes (I) := Byte (Stream_Byte);
      end loop;

      if Version_Bytes /= VERSION_ENCRYPTED then
         Close (File_Handle);
         Success := False;
         return;
      end if;

      -- Read KDF parameters
      for I in KDF_Params'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         KDF_Params (I) := Byte (Stream_Byte);
      end loop;

      -- Parse opslimit and memlimit (big-endian u64)
      declare
         OpsU64 : Unsigned_64 := 0;
         MemU64 : Unsigned_64 := 0;
      begin
         for I in 0 .. 7 loop
            OpsU64 := Shift_Left (OpsU64, 8) or Unsigned_64 (KDF_Params (I + 1));
         end loop;

         for I in 0 .. 7 loop
            MemU64 := Shift_Left (MemU64, 8) or Unsigned_64 (KDF_Params (I + 9));
         end loop;

         Opslimit := unsigned_long (OpsU64);
         Memlimit := size_t (MemU64);
      end;

      -- Read salt
      for I in Salt'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Salt (I) := Byte (Stream_Byte);
      end loop;

      -- Read nonce
      for I in Nonce.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Nonce.Data (I) := Byte (Stream_Byte);
      end loop;

      -- Read ciphertext length
      for I in CT_Len_Bytes'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         CT_Len_Bytes (I) := Byte (Stream_Byte);
      end loop;

      CT_Len := Shift_Left (Unsigned_32 (CT_Len_Bytes (1)), 24) or
                Shift_Left (Unsigned_32 (CT_Len_Bytes (2)), 16) or
                Shift_Left (Unsigned_32 (CT_Len_Bytes (3)), 8) or
                Unsigned_32 (CT_Len_Bytes (4));

      if CT_Len /= Plaintext_Size then
         Close (File_Handle);
         Success := False;
         return;
      end if;

      -- Read ciphertext
      for I in Ciphertext'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Ciphertext (I) := Byte (Stream_Byte);
      end loop;

      -- Read authentication tag
      for I in Auth_Tag.Data'Range loop
         Stream_Element'Read (File_Stream, Stream_Byte);
         Auth_Tag.Data (I) := Byte (Stream_Byte);
      end loop;

      Close (File_Handle);

      -- Step 2: Derive KEK from passphrase
      Rc := crypto_pwhash (
         out_key    => KEK.Data (KEK.Data'First)'Address,
         outlen     => unsigned_long (KEK.Data'Length),
         password   => Passphrase (Passphrase'First)'Address,
         password_len => unsigned_long (Passphrase'Length),
         salt       => Salt (Salt'First)'Address,
         opslimit   => Opslimit,
         memlimit   => Memlimit,
         alg        => crypto_pwhash_ALG_DEFAULT
      );

      if Rc /= 0 then
         Zeroize_XChaCha20_Key (KEK);
         Success := False;
         return;
      end if;

      KEK.Valid := True;

      -- Step 3: Decrypt and verify
      XChaCha20_Decrypt (
         Ciphertext => Ciphertext,
         Auth_Tag   => Auth_Tag,
         Key        => KEK,
         Nonce      => Nonce,
         AAD        => Salt,
         Plaintext  => Plaintext,
         Success    => Op_Success
      );

      Zeroize_XChaCha20_Key (KEK);

      if not Op_Success then
         -- Wrong passphrase or corrupted file
         Success := False;
         return;
      end if;

      -- Step 4: Deserialize plaintext into Identity_Keypair
      -- X25519 keys
      Identity.X25519_PK.Data := Plaintext (Idx .. Idx + 31);
      Idx := Idx + 32;
      Identity.X25519_SK.Data := Plaintext (Idx .. Idx + 31);
      Idx := Idx + 32;
      Identity.X25519_SK.Valid := True;

      -- ML-KEM keys
      Identity.ML_KEM_PK.Data := Plaintext (Idx .. Idx + 1567);
      Idx := Idx + 1568;
      Identity.ML_KEM_SK.Data := Plaintext (Idx .. Idx + 3167);
      Idx := Idx + 3168;
      Identity.ML_KEM_SK.Valid := True;

      -- Ed25519 keys
      Identity.Ed25519_PK.Data := Plaintext (Idx .. Idx + 31);
      Idx := Idx + 32;
      Identity.Ed25519_SK.Data := Plaintext (Idx .. Idx + 31);
      Idx := Idx + 32;
      Identity.Ed25519_SK.Valid := True;

      -- ML-DSA keys
      Identity.ML_DSA_PK.Data := Plaintext (Idx .. Idx + 2591);
      Idx := Idx + 2592;
      Identity.ML_DSA_SK.Data := Plaintext (Idx .. Idx + 4895);
      Identity.ML_DSA_SK.Valid := True;

      Identity.Valid := True;
      Success := True;

   end Load_Identity_Encrypted;

   -------------------------------------------------------------------------
   -- Secure Cleanup
   -------------------------------------------------------------------------

   procedure Zeroize_Identity (Identity : in out Identity_Keypair) is
   begin
      Classical.Zeroize_X25519_Secret (Identity.X25519_SK);
      Classical.Zeroize_Ed25519_Secret (Identity.Ed25519_SK);
      PQC.Zeroize_ML_KEM_Secret (Identity.ML_KEM_SK);
      PQC.Zeroize_ML_DSA_Secret (Identity.ML_DSA_SK);
      Identity.Valid := False;
   end Zeroize_Identity;

end Anubis_Types.Storage;
