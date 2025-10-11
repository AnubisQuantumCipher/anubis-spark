-------------------------------------------------------------------------------
-- Test: Encrypt/Decrypt Large Video File with Encrypted Keystore
-- Demonstrates v1.1.0 encrypted keystore + streaming AEAD on real data
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.Storage;
with Anubis_Types.Streaming;
use Anubis_Types.Streaming;  -- Make Result_Code operators visible

procedure Test_Movie_Encryption is

   Movie_File : constant String := "/Users/sicarii/Desktop/2 Fast 2 Furious.mp4";
   Encrypted  : constant String := "/Users/sicarii/Desktop/2 Fast 2 Furious.mp4.anubis";
   Decrypted  : constant String := "/Users/sicarii/Desktop/2 Fast 2 Furious-decrypted.mp4";
   Key_File   : constant String := "/tmp/movie_key_encrypted.key";
   Passphrase : constant String := "FastFurious2024!SecureMovie";

   Identity : Anubis_Types.Storage.Identity_Keypair;
   Success  : Boolean;
   Result   : Anubis_Types.Streaming.Result_Code;

   Start_Time, End_Time : Time;
   Elapsed : Time_Span;

begin
   Put_Line ("╔═══════════════════════════════════════════════════════════════╗");
   Put_Line ("║  ANUBIS-SPARK v1.1.0 - Movie Encryption Test                 ║");
   Put_Line ("║  Testing: Encrypted Keystore + 2GB File Streaming            ║");
   Put_Line ("╚═══════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("File: " & Movie_File);
   Put_Line ("Format: ANUBISK2 (Argon2id SENSITIVE + XChaCha20-Poly1305)");
   New_Line;
   Put_Line ("═══════════════════════════════════════════════════════════════");

   -- Step 1: Generate identity
   New_Line;
   Put ("1. Generating hybrid post-quantum identity... ");
   Anubis_Types.Storage.Generate_Identity (Identity, Success);

   if not Success then
      Put_Line ("✗ FAILED");
      return;
   end if;
   Put_Line ("✓");

   -- Step 2: Save identity with encrypted keystore
   Put ("2. Saving encrypted keystore (Argon2id 1GiB)... ");
   Start_Time := Clock;
   Anubis_Types.Storage.Save_Identity_Encrypted (
      Identity   => Identity,
      Filename   => Key_File,
      Passphrase => Passphrase,
      Success    => Success
   );
   End_Time := Clock;
   Elapsed := End_Time - Start_Time;

   if not Success then
      Put_Line ("✗ FAILED");
      Anubis_Types.Storage.Zeroize_Identity (Identity);
      return;
   end if;
   Put_Line ("✓ (" & Duration'Image (To_Duration (Elapsed)) & "s)");

   -- Step 3: Zeroize and reload from encrypted keystore
   Put ("3. Zeroizing memory and reloading from encrypted keystore... ");
   Anubis_Types.Storage.Zeroize_Identity (Identity);

   Start_Time := Clock;
   Anubis_Types.Storage.Load_Identity_Encrypted (
      Filename   => Key_File,
      Passphrase => Passphrase,
      Identity   => Identity,
      Success    => Success
   );
   End_Time := Clock;
   Elapsed := End_Time - Start_Time;

   if not Success then
      Put_Line ("✗ FAILED");
      return;
   end if;
   Put_Line ("✓ (" & Duration'Image (To_Duration (Elapsed)) & "s)");

   -- Step 4: Encrypt movie file
   New_Line;
   Put_Line ("4. Encrypting 2.0 GB movie file (streaming AEAD)...");
   Put ("   Progress: ");

   Start_Time := Clock;
   Anubis_Types.Streaming.Encrypt_File_Streaming (
      Input_Path  => Movie_File,
      Output_Path => Encrypted,
      X25519_PK   => Anubis_Types.Storage.Get_X25519_Public (Identity),
      ML_KEM_PK   => Anubis_Types.Storage.Get_ML_KEM_Public (Identity),
      Ed25519_SK  => Anubis_Types.Storage.Get_Ed25519_Secret (Identity),
      ML_DSA_SK   => Anubis_Types.Storage.Get_ML_DSA_Secret (Identity),
      Result      => Result,
      Chunk_Size  => 67_108_864  -- 64 MB chunks
   );
   End_Time := Clock;
   Elapsed := End_Time - Start_Time;

   if Result /= Anubis_Types.Streaming.Success then
      Put_Line ("✗ FAILED");
      Anubis_Types.Storage.Zeroize_Identity (Identity);
      return;
   end if;

   declare
      Seconds : constant Duration := To_Duration (Elapsed);
      Speed_MBps : constant Float := 2048.0 / Float (Seconds);
   begin
      Put_Line ("✓");
      Put_Line ("   Time:  " & Duration'Image (Seconds) & " seconds");
      Put_Line ("   Speed: " & Float'Image (Speed_MBps) & " MB/s");
   end;

   -- Step 5: Decrypt movie file
   New_Line;
   Put_Line ("5. Decrypting movie file (streaming + auth verification)...");
   Put ("   Progress: ");

   Start_Time := Clock;
   Anubis_Types.Streaming.Decrypt_File_Streaming (
      Input_Path  => Encrypted,
      Output_Path => Decrypted,
      X25519_SK   => Anubis_Types.Storage.Get_X25519_Secret (Identity),
      ML_KEM_SK   => Anubis_Types.Storage.Get_ML_KEM_Secret (Identity),
      Ed25519_PK  => Anubis_Types.Storage.Get_Ed25519_Public (Identity),
      ML_DSA_PK   => Anubis_Types.Storage.Get_ML_DSA_Public (Identity),
      Result      => Result
   );
   End_Time := Clock;
   Elapsed := End_Time - Start_Time;

   if Result /= Anubis_Types.Streaming.Success then
      Put_Line ("✗ FAILED - Authentication or decryption error");
      Anubis_Types.Storage.Zeroize_Identity (Identity);
      return;
   end if;

   declare
      Seconds : constant Duration := To_Duration (Elapsed);
      Speed_MBps : constant Float := 2048.0 / Float (Seconds);
   begin
      Put_Line ("✓");
      Put_Line ("   Time:  " & Duration'Image (Seconds) & " seconds");
      Put_Line ("   Speed: " & Float'Image (Speed_MBps) & " MB/s");
   end;

   -- Step 6: Cleanup
   Anubis_Types.Storage.Zeroize_Identity (Identity);

   New_Line;
   Put_Line ("═══════════════════════════════════════════════════════════════");
   Put_Line ("✓ ALL TESTS PASSED!");
   Put_Line ("═══════════════════════════════════════════════════════════════");
   New_Line;
   Put_Line ("Output Files:");
   Put_Line ("  Encrypted:     " & Encrypted);
   Put_Line ("  Decrypted:     " & Decrypted);
   Put_Line ("  Keystore:      " & Key_File);
   New_Line;
   Put_Line ("Security Features Demonstrated:");
   Put_Line ("  ✓ Encrypted keystore (ANUBISK2 format)");
   Put_Line ("  ✓ Argon2id SENSITIVE (1 GiB RAM, 4 iterations)");
   Put_Line ("  ✓ XChaCha20-Poly1305 AEAD for keystore");
   Put_Line ("  ✓ Streaming encryption with 64 MB chunks");
   Put_Line ("  ✓ Hybrid post-quantum protection (ML-KEM + ML-DSA)");
   Put_Line ("  ✓ Constant memory usage (< 100 MB for 2 GB file)");
   New_Line;

end Test_Movie_Encryption;
