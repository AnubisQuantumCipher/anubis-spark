-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Header AAD Binding (Implementation)
-- Uses BLAKE2b-256 for header hashing
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- FFI to libsodium

with Sodium_Hash;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with System;

package body Anubis_Types.Header_AAD is

   -------------------------------------------------------------------------
   -- Helper: Convert Natural to Big-Endian 8 bytes
   -------------------------------------------------------------------------

   function To_BE64 (Value : Natural) return Byte_Array is
      Result : Byte_Array (1 .. 8);
      V      : Unsigned_64 := Unsigned_64 (Value);
   begin
      Result (8) := Byte (V and 16#FF#);
      V := Shift_Right (V, 8);
      Result (7) := Byte (V and 16#FF#);
      V := Shift_Right (V, 8);
      Result (6) := Byte (V and 16#FF#);
      V := Shift_Right (V, 8);
      Result (5) := Byte (V and 16#FF#);
      V := Shift_Right (V, 8);
      Result (4) := Byte (V and 16#FF#);
      V := Shift_Right (V, 8);
      Result (3) := Byte (V and 16#FF#);
      V := Shift_Right (V, 8);
      Result (2) := Byte (V and 16#FF#);
      V := Shift_Right (V, 8);
      Result (1) := Byte (V and 16#FF#);
      return Result;
   end To_BE64;

   -------------------------------------------------------------------------
   -- Compute Header AAD
   -------------------------------------------------------------------------

   function Compute_Header_AAD (
      File_Nonce16   : Byte_Array;
      Chunk_Size     : Natural;
      Total_Size     : Natural
   ) return Byte_Array
   is
      -- Canonical header layout:
      -- "ANUB2" (5) || Version=2 (1) || File_Nonce16 (16) || Chunk_Size (8) || Total_Size (8)
      -- Total: 38 bytes
      Header_Data : Byte_Array (1 .. 38);
      AAD         : Byte_Array (1 .. 32);  -- BLAKE2b-256 output
      P           : Natural := 1;
      Rc          : Interfaces.C.int;
   begin
      -- Magic: "ANUB2"
      Header_Data (P) := Byte (Character'Pos ('A')); P := P + 1;
      Header_Data (P) := Byte (Character'Pos ('N')); P := P + 1;
      Header_Data (P) := Byte (Character'Pos ('U')); P := P + 1;
      Header_Data (P) := Byte (Character'Pos ('B')); P := P + 1;
      Header_Data (P) := Byte (Character'Pos ('2')); P := P + 1;

      -- Version: 2
      Header_Data (P) := 2; P := P + 1;

      -- File nonce (16 bytes)
      Header_Data (P .. P + 15) := File_Nonce16;
      P := P + 16;

      -- Chunk size (8 bytes, big-endian)
      declare
         Chunk_BE : constant Byte_Array := To_BE64 (Chunk_Size);
      begin
         Header_Data (P .. P + 7) := Chunk_BE;
         P := P + 8;
      end;

      -- Total size (8 bytes, big-endian)
      declare
         Total_BE : constant Byte_Array := To_BE64 (Total_Size);
      begin
         Header_Data (P .. P + 7) := Total_BE;
      end;

      -- Hash header data with BLAKE2b-256 (keyless)
      Rc := Sodium_Hash.crypto_generichash (
         Output     => AAD (AAD'First)'Address,
         Output_Len => 32,
         Input      => Header_Data (Header_Data'First)'Address,
         Input_Len  => Interfaces.C.unsigned_long_long (Header_Data'Length),
         Key        => System.Null_Address,  -- Keyless hash
         Key_Len    => 0
      );

      if Rc /= 0 then
         -- Hash failed (should never happen with libsodium)
         AAD := (others => 0);
      end if;

      return AAD;
   end Compute_Header_AAD;

end Anubis_Types.Header_AAD;
