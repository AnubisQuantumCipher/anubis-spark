-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Key Storage Implementation
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;
with Anubis_Types.Classical;
with Anubis_Types.PQC;

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
