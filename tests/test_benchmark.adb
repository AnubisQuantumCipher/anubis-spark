-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Performance Benchmark Suite
-- Measures throughput (MB/s) and latency (ms) for all operations
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.Classical;
with Anubis_Types.PQC;
with Anubis_Types.Storage;
with Anubis_Types.Streaming; use Anubis_Types.Streaming;
with Anubis_Entropy;
with Interfaces; use Interfaces;

procedure Test_Benchmark is

   -- Timing helper
   function Measure_Time (Action : access procedure) return Duration is
      Start_Time : constant Time := Clock;
   begin
      Action.all;
      return Clock - Start_Time;
   end Measure_Time;

   -- Format duration as milliseconds
   function Format_Ms (D : Duration) return String is
      Ms : constant Long_Long_Float := Long_Long_Float (D) * 1000.0;
   begin
      return Long_Long_Float'Image (Ms) & " ms";
   end Format_Ms;

   -- Format throughput as MB/s
   function Format_Throughput (Bytes : Natural; D : Duration) return String is
      MB : constant Long_Long_Float := Long_Long_Float (Bytes) / 1_000_000.0;
      Seconds : constant Long_Long_Float := Long_Long_Float (D);
      MBps : constant Long_Long_Float := MB / Seconds;
   begin
      if Seconds > 0.0 then
         return Long_Long_Float'Image (MBps) & " MB/s";
      else
         return "N/A";
      end if;
   end Format_Throughput;

   procedure Print_Banner is
   begin
      Put_Line ("╔═══════════════════════════════════════════════════════════════╗");
      Put_Line ("║  ANUBIS-SPARK Performance Benchmark Suite                    ║");
      Put_Line ("║  Measures throughput (MB/s) and latency (ms)                 ║");
      Put_Line ("╚═══════════════════════════════════════════════════════════════╝");
      New_Line;
   end Print_Banner;

   ---------------------------------------------------------------------------
   -- Benchmark 1: Key Generation
   ---------------------------------------------------------------------------

   procedure Benchmark_Key_Generation is
      Iterations : constant := 100;
      Start_Time : Time;
      End_Time   : Time;
      Elapsed    : Duration;

      -- Classical keys
      X25519_PK  : Anubis_Types.X25519_Public_Key;
      X25519_SK  : Anubis_Types.X25519_Secret_Key;
      Ed25519_PK : Anubis_Types.Ed25519_Public_Key;
      Ed25519_SK : Anubis_Types.Ed25519_Secret_Key;

      -- PQC keys
      ML_KEM_PK  : Anubis_Types.ML_KEM_Public_Key;
      ML_KEM_SK  : Anubis_Types.ML_KEM_Secret_Key;
      ML_DSA_PK  : Anubis_Types.ML_DSA_Public_Key;
      ML_DSA_SK  : Anubis_Types.ML_DSA_Secret_Key;

      Success : Boolean;
   begin
      Put_Line ("═══════════════════════════════════════════════════════════════");
      Put_Line ("Benchmark 1: Key Generation (averaged over " & Integer'Image (Iterations) & " runs)");
      Put_Line ("═══════════════════════════════════════════════════════════════");
      New_Line;

      -- X25519 keypair generation
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         Classical.X25519_Generate_Keypair (X25519_PK, X25519_SK, Success);
      end loop;
      End_Time := Clock;
      Elapsed := (End_Time - Start_Time) / Iterations;
      Put_Line ("X25519 Keypair Generation:     " & Format_Ms (Elapsed));
      Classical.Zeroize_X25519_Secret (X25519_SK);

      -- Ed25519 keypair generation
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         Classical.Ed25519_Generate_Keypair (Ed25519_PK, Ed25519_SK, Success);
      end loop;
      End_Time := Clock;
      Elapsed := (End_Time - Start_Time) / Iterations;
      Put_Line ("Ed25519 Keypair Generation:    " & Format_Ms (Elapsed));
      Classical.Zeroize_Ed25519_Secret (Ed25519_SK);

      -- ML-KEM-1024 keypair generation
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         PQC.ML_KEM_Generate_Keypair (ML_KEM_PK, ML_KEM_SK, Success);
      end loop;
      End_Time := Clock;
      Elapsed := (End_Time - Start_Time) / Iterations;
      Put_Line ("ML-KEM-1024 Keypair Generation: " & Format_Ms (Elapsed));
      PQC.Zeroize_ML_KEM_Secret (ML_KEM_SK);

      -- ML-DSA-87 keypair generation
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         PQC.ML_DSA_Generate_Keypair (ML_DSA_PK, ML_DSA_SK, Success);
      end loop;
      End_Time := Clock;
      Elapsed := (End_Time - Start_Time) / Iterations;
      Put_Line ("ML-DSA-87 Keypair Generation:  " & Format_Ms (Elapsed));
      PQC.Zeroize_ML_DSA_Secret (ML_DSA_SK);

      New_Line;
   end Benchmark_Key_Generation;

   ---------------------------------------------------------------------------
   -- Benchmark 2: Hybrid Operations
   ---------------------------------------------------------------------------

   procedure Benchmark_Hybrid_Operations is
      Iterations : constant := 100;
      Start_Time : Time;
      End_Time   : Time;
      Elapsed    : Duration;

      -- Classical keys
      X25519_PK  : Anubis_Types.X25519_Public_Key;
      X25519_SK  : Anubis_Types.X25519_Secret_Key;
      Ed25519_PK : Anubis_Types.Ed25519_Public_Key;
      Ed25519_SK : Anubis_Types.Ed25519_Secret_Key;

      -- PQC keys
      ML_KEM_PK  : Anubis_Types.ML_KEM_Public_Key;
      ML_KEM_SK  : Anubis_Types.ML_KEM_Secret_Key;
      ML_DSA_PK  : Anubis_Types.ML_DSA_Public_Key;
      ML_DSA_SK  : Anubis_Types.ML_DSA_Secret_Key;

      -- Hybrid operations
      X25519_Ephemeral_PK : Anubis_Types.X25519_Public_Key;
      X25519_Ephemeral_SK : Anubis_Types.X25519_Secret_Key;
      ML_KEM_CT           : Anubis_Types.ML_KEM_Ciphertext;
      Hybrid_Secret       : PQC.Hybrid_Shared_Secret;

      -- Signature
      Message : constant Byte_Array (1 .. 1024) := (others => 0);
      Sig     : PQC.Hybrid_Signature;
      Valid   : Boolean;

      Success : Boolean;
   begin
      Put_Line ("═══════════════════════════════════════════════════════════════");
      Put_Line ("Benchmark 2: Hybrid Operations (averaged over " & Integer'Image (Iterations) & " runs)");
      Put_Line ("═══════════════════════════════════════════════════════════════");
      New_Line;

      -- Generate keys once
      Classical.X25519_Generate_Keypair (X25519_PK, X25519_SK, Success);
      Classical.Ed25519_Generate_Keypair (Ed25519_PK, Ed25519_SK, Success);
      PQC.ML_KEM_Generate_Keypair (ML_KEM_PK, ML_KEM_SK, Success);
      PQC.ML_DSA_Generate_Keypair (ML_DSA_PK, ML_DSA_SK, Success);

      -- Hybrid Encapsulation (X25519 + ML-KEM)
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         PQC.Hybrid_Encapsulate (
            X25519_Public           => X25519_PK,
            ML_KEM_Public           => ML_KEM_PK,
            X25519_Ephemeral_Public => X25519_Ephemeral_PK,
            X25519_Ephemeral_Secret => X25519_Ephemeral_SK,
            Ciphertext              => ML_KEM_CT,
            Hybrid_Secret           => Hybrid_Secret,
            Success                 => Success
         );
      end loop;
      End_Time := Clock;
      Elapsed := (End_Time - Start_Time) / Iterations;
      Put_Line ("Hybrid Encapsulation (X25519+ML-KEM): " & Format_Ms (Elapsed));

      -- Hybrid Decapsulation
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         PQC.Hybrid_Decapsulate (
            X25519_Secret    => X25519_SK,
            ML_KEM_Secret    => ML_KEM_SK,
            X25519_Ephemeral => X25519_Ephemeral_PK,
            ML_KEM_CT        => ML_KEM_CT,
            Hybrid_Secret    => Hybrid_Secret,
            Success          => Success
         );
      end loop;
      End_Time := Clock;
      Elapsed := (End_Time - Start_Time) / Iterations;
      Put_Line ("Hybrid Decapsulation:                  " & Format_Ms (Elapsed));

      -- Hybrid Signature (Ed25519 + ML-DSA)
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         PQC.Hybrid_Sign (Message, Ed25519_SK, ML_DSA_SK, Sig, Success);
      end loop;
      End_Time := Clock;
      Elapsed := (End_Time - Start_Time) / Iterations;
      Put_Line ("Hybrid Sign (Ed25519+ML-DSA):          " & Format_Ms (Elapsed));

      -- Hybrid Verification
      Start_Time := Clock;
      for I in 1 .. Iterations loop
         Valid := PQC.Hybrid_Verify (Message, Sig, Ed25519_PK, ML_DSA_PK);
      end loop;
      End_Time := Clock;
      Elapsed := (End_Time - Start_Time) / Iterations;
      Put_Line ("Hybrid Verify:                         " & Format_Ms (Elapsed));

      New_Line;

      -- Cleanup
      Classical.Zeroize_X25519_Secret (X25519_SK);
      Classical.Zeroize_Ed25519_Secret (Ed25519_SK);
      PQC.Zeroize_ML_KEM_Secret (ML_KEM_SK);
      PQC.Zeroize_ML_DSA_Secret (ML_DSA_SK);
   end Benchmark_Hybrid_Operations;

   ---------------------------------------------------------------------------
   -- Benchmark 3: File Encryption Throughput
   ---------------------------------------------------------------------------

   procedure Benchmark_File_Encryption is
      Test_Sizes : constant array (1 .. 4) of Natural := (
         1_048_576,      -- 1 MB
         10_485_760,     -- 10 MB
         104_857_600,    -- 100 MB
         1_073_741_824   -- 1 GB
      );
      Labels : constant array (1 .. 4) of String (1 .. 6) := (
         "  1 MB", " 10 MB", "100 MB", "  1 GB"
      );

      Identity : Storage.Identity_Keypair;
      Success  : Boolean;
      Result   : Streaming.Result_Code;

      Input_File  : constant String := "/tmp/anubis_bench_input.bin";
      Output_File : constant String := "/tmp/anubis_bench_output.bin";

      Start_Time : Time;
      End_Time   : Time;
      Elapsed    : Duration;

      Signer_Label_Data : constant Signer_Label := (others => 0);
      Signer_FP         : constant Signer_Fingerprint := (others => 0);
      Signer_TS         : constant Unsigned_64 := 0;
   begin
      Put_Line ("═══════════════════════════════════════════════════════════════");
      Put_Line ("Benchmark 3: File Encryption/Decryption Throughput");
      Put_Line ("═══════════════════════════════════════════════════════════════");
      New_Line;

      -- Generate test identity
      Put ("Generating test identity... ");
      Storage.Generate_Identity (Identity, Success);
      if not Success then
         Put_Line ("FAILED");
         return;
      end if;
      Put_Line ("OK");
      New_Line;

      for I in Test_Sizes'Range loop
         Put_Line ("Testing with " & Labels (I) & " file:");

         -- Create test file
         declare
            File : Ada.Streams.Stream_IO.File_Type;
            Buffer : Byte_Array (1 .. 65536) := (others => 0);
            Remaining : Natural := Test_Sizes (I);
         begin
            Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Input_File);
            while Remaining > 0 loop
               if Remaining < Buffer'Length then
                  Byte_Array'Write (Ada.Streams.Stream_IO.Stream (File), Buffer (1 .. Remaining));
                  Remaining := 0;
               else
                  Byte_Array'Write (Ada.Streams.Stream_IO.Stream (File), Buffer);
                  Remaining := Remaining - Buffer'Length;
               end if;
            end loop;
            Ada.Streams.Stream_IO.Close (File);
         end;

         -- Benchmark encryption
         Start_Time := Clock;
         Streaming.Encrypt_File_Streaming (
            Input_Path                => Input_File,
            Output_Path               => Output_File,
            X25519_PK                 => Storage.Get_X25519_Public (Identity),
            ML_KEM_PK                 => Storage.Get_ML_KEM_Public (Identity),
            Ed25519_SK                => Storage.Get_Ed25519_Secret (Identity),
            ML_DSA_SK                 => Storage.Get_ML_DSA_Secret (Identity),
            Signer_Label_Data         => Signer_Label_Data,
            Signer_Timestamp          => Signer_TS,
            Signer_Fingerprint_Data   => Signer_FP,
            Result                    => Result,
            Chunk_Size                => 67_108_864
         );
         End_Time := Clock;
         Elapsed := End_Time - Start_Time;

         if Result = Streaming.Success then
            Put_Line ("  Encryption: " & Format_Ms (Elapsed) & " (" & Format_Throughput (Test_Sizes (I), Elapsed) & ")");
         else
            Put_Line ("  Encryption: FAILED");
         end if;

         -- Benchmark decryption
         Start_Time := Clock;
         declare
            Decrypted_File : constant String := "/tmp/anubis_bench_decrypted.bin";
            Signer_Label_Out : Signer_Label;
            Signer_FP_Out    : Signer_Fingerprint;
            Signer_TS_Out    : Unsigned_64;
         begin
            Streaming.Decrypt_File_Streaming (
               Input_Path                => Output_File,
               Output_Path               => Decrypted_File,
               X25519_SK                 => Storage.Get_X25519_Secret (Identity),
               ML_KEM_SK                 => Storage.Get_ML_KEM_Secret (Identity),
               Ed25519_PK                => Storage.Get_Ed25519_Public (Identity),
               ML_DSA_PK                 => Storage.Get_ML_DSA_Public (Identity),
               Signer_Label_Data         => Signer_Label_Out,
               Signer_Timestamp          => Signer_TS_Out,
               Signer_Fingerprint_Data   => Signer_FP_Out,
               Result                    => Result
            );
         end;
         End_Time := Clock;
         Elapsed := End_Time - Start_Time;

         if Result = Streaming.Success then
            Put_Line ("  Decryption: " & Format_Ms (Elapsed) & " (" & Format_Throughput (Test_Sizes (I), Elapsed) & ")");
         else
            Put_Line ("  Decryption: FAILED");
         end if;

         New_Line;

         -- Cleanup
         if Ada.Directories.Exists (Input_File) then
            Ada.Directories.Delete_File (Input_File);
         end if;
         if Ada.Directories.Exists (Output_File) then
            Ada.Directories.Delete_File (Output_File);
         end if;
         if Ada.Directories.Exists ("/tmp/anubis_bench_decrypted.bin") then
            Ada.Directories.Delete_File ("/tmp/anubis_bench_decrypted.bin");
         end if;
      end loop;

      Storage.Zeroize_Identity (Identity);
   end Benchmark_File_Encryption;

   ---------------------------------------------------------------------------
   -- Main Benchmark Suite
   ---------------------------------------------------------------------------

begin
   Print_Banner;

   Benchmark_Key_Generation;
   Benchmark_Hybrid_Operations;
   Benchmark_File_Encryption;

   Put_Line ("═══════════════════════════════════════════════════════════════");
   Put_Line ("Benchmark Complete!");
   Put_Line ("═══════════════════════════════════════════════════════════════");
end Test_Benchmark;
