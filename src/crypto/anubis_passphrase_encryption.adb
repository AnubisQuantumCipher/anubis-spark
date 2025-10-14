-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Passphrase-Based File Encryption Implementation
-- Mode B: Direct passphrase → file encryption (no identity keys)
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- Uses libsodium bindings and file I/O

with Ada.Streams.Stream_IO;
with Interfaces.C; use Interfaces.C;
with System;

package body Anubis_Passphrase_Encryption is

   use Ada.Streams.Stream_IO;

   -------------------------------------------------------------------------
   -- Libsodium FFI Bindings
   -------------------------------------------------------------------------

   procedure randombytes_buf (
      buf  : out Byte_Array;
      size : size_t
   ) with Import, Convention => C, External_Name => "randombytes_buf";

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

   crypto_pwhash_ALG_ARGON2ID13 : constant int := 2;
   ARGON2ID_OPSLIMIT : constant unsigned_long := 4;
   ARGON2ID_MEMLIMIT : constant size_t := 1_073_741_824;  -- 1 GiB

   CHUNK_SIZE : constant := 65536;  -- 64 KB chunks

   -------------------------------------------------------------------------
   -- Implementation
   -------------------------------------------------------------------------

   procedure Encrypt_File_With_Passphrase (
      Input_Path  : in     String;
      Output_Path : in     String;
      Passphrase  : in     String;
      Success     : out    Boolean
   ) is
      Input_File  : File_Type;
      Output_File : File_Type;

      Salt   : File_Salt;
      Nonce  : File_Nonce;
      Key    : Byte_Array (1 .. 32);

      Pass_C : constant char_array := To_C (Passphrase);
      Status : int;

      Chunk       : Byte_Array (1 .. CHUNK_SIZE);
      Cipher_Chunk : Byte_Array (1 .. CHUNK_SIZE + 16);
      Bytes_Read  : Stream_Element_Offset;
      Cipher_Len  : aliased unsigned_long;
   begin
      Success := False;

      -- Generate random salt and nonce
      begin
         randombytes_buf (Salt, size_t (Salt'Length));
         randombytes_buf (Nonce, size_t (Nonce'Length));
      exception
         when others =>
            return;
      end;

      -- Derive key from passphrase
      Status := crypto_pwhash (
         out_key   => Key,
         outlen    => unsigned_long (Key'Length),
         passwd    => Pass_C,
         passwdlen => unsigned_long (Passphrase'Length),
         salt      => Salt,
         opslimit  => ARGON2ID_OPSLIMIT,
         memlimit  => ARGON2ID_MEMLIMIT,
         alg       => crypto_pwhash_ALG_ARGON2ID13
      );

      if Status /= 0 then
         for I in Key'Range loop
            Key (I) := 0;
         end loop;
         return;
      end if;

      -- Open files
      begin
         Open (Input_File, In_File, Input_Path);
      exception
         when others =>
            for I in Key'Range loop
               Key (I) := 0;
            end loop;
            return;
      end;

      begin
         Create (Output_File, Out_File, Output_Path);
      exception
         when others =>
            Close (Input_File);
            for I in Key'Range loop
               Key (I) := 0;
            end loop;
            return;
      end;

      -- Write header
      declare
         Header : constant Passphrase_File_Header := (
            Magic => (Byte (Character'Pos ('A')),
                     Byte (Character'Pos ('N')),
                     Byte (Character'Pos ('U')),
                     Byte (Character'Pos ('B')),
                     Byte (Character'Pos ('F')),
                     Byte (Character'Pos ('1'))),
            Salt  => Salt,
            Nonce => Nonce
         );
      begin
         for B of Header.Magic loop
            Stream_Element'Write (Stream (Output_File), Stream_Element (B));
         end loop;
         for B of Header.Salt loop
            Stream_Element'Write (Stream (Output_File), Stream_Element (B));
         end loop;
         for B of Header.Nonce loop
            Stream_Element'Write (Stream (Output_File), Stream_Element (B));
         end loop;
      exception
         when others =>
            Close (Input_File);
            Close (Output_File);
            for I in Key'Range loop
               Key (I) := 0;
            end loop;
            return;
      end;

      -- Encrypt file in chunks
      loop
         begin
            Bytes_Read := 0;
            for I in 1 .. CHUNK_SIZE loop
               declare
                  SE : Stream_Element;
               begin
                  Stream_Element'Read (Stream (Input_File), SE);
                  Chunk (I) := Byte (SE);
                  Bytes_Read := Bytes_Read + 1;
               end;
            end loop;
         exception
            when End_Error =>
               exit when Bytes_Read = 0;
         end;

         -- Encrypt chunk
         declare
            Plain : constant Byte_Array := Chunk (1 .. Natural (Bytes_Read));
            AAD : constant Byte_Array (1 .. 0) := (others => 0);
         begin
            Status := crypto_aead_xchacha20poly1305_ietf_encrypt (
               c      => Cipher_Chunk,
               clen_p => Cipher_Len'Access,
               m      => Plain,
               mlen   => unsigned_long (Plain'Length),
               ad     => AAD,
               adlen  => 0,
               nsec   => System.Null_Address,
               npub   => Nonce,
               k      => Key
            );

            if Status /= 0 then
               Close (Input_File);
               Close (Output_File);
               for I in Key'Range loop
                  Key (I) := 0;
               end loop;
               return;
            end if;

            -- Write encrypted chunk
            for I in 1 .. Natural (Cipher_Len) loop
               Stream_Element'Write (Stream (Output_File),
                                    Stream_Element (Cipher_Chunk (I)));
            end loop;
         end;

         exit when Bytes_Read < CHUNK_SIZE;
      end loop;

      Close (Input_File);
      Close (Output_File);

      -- Zeroize key
      for I in Key'Range loop
         Key (I) := 0;
      end loop;

      Success := True;

   exception
      when others =>
         begin
            if Is_Open (Input_File) then
               Close (Input_File);
            end if;
         exception
            when others => null;
         end;
         begin
            if Is_Open (Output_File) then
               Close (Output_File);
            end if;
         exception
            when others => null;
         end;
         for I in Key'Range loop
            Key (I) := 0;
         end loop;
         Success := False;
   end Encrypt_File_With_Passphrase;

   procedure Decrypt_File_With_Passphrase (
      Input_Path  : in     String;
      Output_Path : in     String;
      Passphrase  : in     String;
      Success     : out    Boolean
   ) is
      Input_File  : File_Type;
      Output_File : File_Type;

      Header : Passphrase_File_Header;
      Key    : Byte_Array (1 .. 32);

      Pass_C : constant char_array := To_C (Passphrase);
      Status : int;

      Cipher_Chunk : Byte_Array (1 .. CHUNK_SIZE + 16);
      Plain_Chunk  : Byte_Array (1 .. CHUNK_SIZE);
      Bytes_Read   : Stream_Element_Offset;
      Plain_Len    : aliased unsigned_long;
   begin
      Success := False;

      -- Open input file
      begin
         Open (Input_File, In_File, Input_Path);
      exception
         when others =>
            return;
      end;

      -- Read header
      begin
         for I in Header.Magic'Range loop
            declare
               SE : Stream_Element;
            begin
               Stream_Element'Read (Stream (Input_File), SE);
               Header.Magic (I) := Byte (SE);
            end;
         end loop;
         for I in Header.Salt'Range loop
            declare
               SE : Stream_Element;
            begin
               Stream_Element'Read (Stream (Input_File), SE);
               Header.Salt (I) := Byte (SE);
            end;
         end loop;
         for I in Header.Nonce'Range loop
            declare
               SE : Stream_Element;
            begin
               Stream_Element'Read (Stream (Input_File), SE);
               Header.Nonce (I) := Byte (SE);
            end;
         end loop;
      exception
         when others =>
            Close (Input_File);
            return;
      end;

      -- Validate header
      if not Header_Valid (Header) then
         Close (Input_File);
         return;
      end if;

      -- Derive key from passphrase
      Status := crypto_pwhash (
         out_key   => Key,
         outlen    => unsigned_long (Key'Length),
         passwd    => Pass_C,
         passwdlen => unsigned_long (Passphrase'Length),
         salt      => Header.Salt,
         opslimit  => ARGON2ID_OPSLIMIT,
         memlimit  => ARGON2ID_MEMLIMIT,
         alg       => crypto_pwhash_ALG_ARGON2ID13
      );

      if Status /= 0 then
         Close (Input_File);
         for I in Key'Range loop
            Key (I) := 0;
         end loop;
         return;
      end if;

      -- Open output file
      begin
         Create (Output_File, Out_File, Output_Path);
      exception
         when others =>
            Close (Input_File);
            for I in Key'Range loop
               Key (I) := 0;
            end loop;
            return;
      end;

      -- Decrypt file in chunks
      loop
         begin
            Bytes_Read := 0;
            for I in 1 .. CHUNK_SIZE + 16 loop
               declare
                  SE : Stream_Element;
               begin
                  Stream_Element'Read (Stream (Input_File), SE);
                  Cipher_Chunk (I) := Byte (SE);
                  Bytes_Read := Bytes_Read + 1;
               end;
            end loop;
         exception
            when End_Error =>
               exit when Bytes_Read = 0;
         end;

         -- Decrypt chunk
         declare
            Cipher : constant Byte_Array := Cipher_Chunk (1 .. Natural (Bytes_Read));
            AAD : constant Byte_Array (1 .. 0) := (others => 0);
         begin
            Status := crypto_aead_xchacha20poly1305_ietf_decrypt (
               m      => Plain_Chunk,
               mlen_p => Plain_Len'Access,
               nsec   => System.Null_Address,
               c      => Cipher,
               clen   => unsigned_long (Cipher'Length),
               ad     => AAD,
               adlen  => 0,
               npub   => Header.Nonce,
               k      => Key
            );

            if Status /= 0 then
               Close (Input_File);
               Close (Output_File);
               for I in Key'Range loop
                  Key (I) := 0;
               end loop;
               return;
            end if;

            -- Write decrypted chunk
            for I in 1 .. Natural (Plain_Len) loop
               Stream_Element'Write (Stream (Output_File),
                                    Stream_Element (Plain_Chunk (I)));
            end loop;
         end;

         exit when Bytes_Read < CHUNK_SIZE + 16;
      end loop;

      Close (Input_File);
      Close (Output_File);

      -- Zeroize key
      for I in Key'Range loop
         Key (I) := 0;
      end loop;

      Success := True;

   exception
      when others =>
         begin
            if Is_Open (Input_File) then
               Close (Input_File);
            end if;
         exception
            when others => null;
         end;
         begin
            if Is_Open (Output_File) then
               Close (Output_File);
            end if;
         exception
            when others => null;
         end;
         for I in Key'Range loop
            Key (I) := 0;
         end loop;
         Success := False;
   end Decrypt_File_With_Passphrase;

end Anubis_Passphrase_Encryption;
