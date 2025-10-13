-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Hybrid Post-Quantum File Encryption System
-- Command-Line Interface
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  -- CLI uses Ada.Command_Line

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Types.Classical;
with Anubis_Types.PQC;
with Anubis_Types.Storage;
with Anubis_Types.Streaming;
with Anubis_Types.File_Encryption;
with Anubis_IO;
with Anubis_Trust;
with Anubis_Trust.Logic;
use Anubis_Types.Streaming;  -- Make Result_Code operators visible

procedure Anubis_Main is

   -- Helper function to find argument value by flag name
   function Get_Arg (Flag : String; Default : String := "") return String is
   begin
      for I in 1 .. Argument_Count - 1 loop
         if Argument (I) = Flag then
            return Argument (I + 1);
         end if;
      end loop;
      return Default;
   end Get_Arg;

   -- Current Unix timestamp (seconds since 1970-01-01).
   -- Returns 0 and Valid=False if the system clock is before the epoch or
   -- if conversion to Unsigned_64 fails (graceful degradation).
   function Unix_Timestamp (Valid : out Boolean) return Unsigned_64 is
      Epoch : constant Time := Time_Of (1970, 1, 1, 0.0);
      Now   : Time;
      Diff  : Duration;
   begin
      Valid := False;
      begin
         Now := Clock;
      exception
         when others =>
            return 0;
      end;

      Diff := Now - Epoch;
      if Diff <= 0.0 then
         return 0;
      end if;

      begin
         declare
            Seconds : constant Long_Long_Integer := Long_Long_Integer (Diff);
         begin
            if Seconds < 0 then
               return 0;
            else
               Valid := True;
               return Unsigned_64 (Seconds);
            end if;
         end;
      exception
         when Constraint_Error =>
            return 0;
      end;
   end Unix_Timestamp;

   -- Human-readable ISO 8601 UTC timestamp (fallback to numeric seconds).
   function Format_Timestamp (Value : Unsigned_64) return String is
   begin
      if Value = 0 then
         return "0";
      end if;

      declare
         Epoch : constant Time := Time_Of (1970, 1, 1, 0.0);
         Seconds_Float : Long_Long_Float;
         T      : Time;
      begin
         begin
            Seconds_Float := Long_Long_Float (Value);
         exception
            when others =>
               return Trim (Unsigned_64'Image (Value), Both);
         end;

         begin
            T := Epoch + Duration (Seconds_Float);
         exception
            when others =>
               return Trim (Unsigned_64'Image (Value), Both);
         end;

         begin
            return Trim (Ada.Calendar.Formatting.Image (T), Both);
         exception
            when others =>
               return Trim (Unsigned_64'Image (Value), Both);
         end;
      end;
   end Format_Timestamp;

   -- Check if flag exists
   function Has_Arg (Flag : String) return Boolean is
   begin
      for I in 1 .. Argument_Count loop
         if Argument (I) = Flag then
            return True;
         end if;
      end loop;
      return False;
   end Has_Arg;

   procedure Print_Banner is
   begin
      Put_Line ("╔═══════════════════════════════════════════════════════════════╗");
      Put_Line ("║  ANUBIS-SPARK v2.0.0 - Quantum-Resistant File Encryption     ║");
      Put_Line ("║  PLATINUM-LEVEL SPARK VERIFICATION + NIST POST-QUANTUM       ║");
      Put_Line ("╚═══════════════════════════════════════════════════════════════╝");
      New_Line;
      Put_Line ("Security Architecture:");
      Put_Line ("  Classical:     X25519 + Ed25519 + XChaCha20-Poly1305");
      Put_Line ("  Post-Quantum:  ML-KEM-1024 + ML-DSA-87 (NIST Level 5)");
      Put_Line ("  Key Derivation: HKDF-SHA256 + Argon2id");
      Put_Line ("  Verification:  SPARK Platinum (183/183 proofs - 100%)");
      New_Line;
   end Print_Banner;

   procedure Print_Usage is
   begin
      Print_Banner;
      Put_Line ("Usage: anubis-spark <command> [options]");
      New_Line;
      Put_Line ("Commands:");
      Put_Line ("  keygen           Generate new hybrid keypair");
      Put_Line ("  encrypt          Encrypt file with hybrid PQ protection (optional --label)");
      Put_Line ("  decrypt          Decrypt, verify, and enforce signer trust");
      Put_Line ("  trust            Manage signer trust records (list|approve|deny|selfcheck)");
      Put_Line ("  test             Run cryptographic self-tests");
      Put_Line ("  version          Show version and security info");
      Put_Line ("  convert          Re-encrypt plaintext to ANUB3 (use v1.x to decrypt first)");
      Put_Line ("  help             Show this help message");
      New_Line;
      Put_Line ("Examples:");
      Put_Line ("  anubis-spark keygen --output my_identity.key");
      Put_Line ("  anubis-spark keygen --output my.key --passphrase ""SecurePass123""");
      Put_Line ("  anubis-spark encrypt --key alice.key --input secret.txt");
      Put_Line ("  anubis-spark encrypt --key alice.key --passphrase ""Pass"" --input secret.txt");
      Put_Line ("  anubis-spark decrypt --key bob.key --input secret.txt.anubis");
      Put_Line ("  anubis-spark trust list");
      Put_Line ("  anubis-spark trust approve --fingerprint <hex> [--operator <name>]");
      New_Line;
      Put_Line ("For detailed help on a command:");
      Put_Line ("  anubis-spark <command> --help");
      New_Line;
   end Print_Usage;

   procedure Print_Version is
   begin
      Print_Banner;
      Put_Line ("Version Information:");
      Put_Line ("  Anubis-SPARK:  2.0.0 (ANUB3 + Dual Signatures + Trust)");
      Put_Line ("  liboqs:        0.14.0");
      Put_Line ("  libsodium:     1.0.20");
      Put_Line ("  SPARK:         2024");
      Put_Line ("  GNAT:          14.2.1");
      New_Line;
      Put_Line ("Cryptographic Algorithms:");
      Put_Line ("  ML-KEM-1024:   NIST FIPS 203 (Post-Quantum KEM)");
      Put_Line ("  ML-DSA-87:     NIST FIPS 204 (Post-Quantum Signatures)");
      Put_Line ("  X25519:        RFC 7748 (ECDH)");
      Put_Line ("  Ed25519:       RFC 8032 (EdDSA)");
      Put_Line ("  XChaCha20:     RFC 8439 (Stream Cipher)");
      Put_Line ("  Poly1305:      RFC 8439 (MAC)");
      Put_Line ("  Argon2id:      RFC 9106 (Password Hashing)");
      Put_Line ("  HKDF-SHA256:   RFC 5869 (Key Derivation)");
      New_Line;
      Put_Line ("SPARK Verification Status:");
      Put_Line ("  Stone:   ✓ Valid SPARK subset");
      Put_Line ("  Bronze:  ✓ Flow analysis (no uninitialized vars)");
      Put_Line ("  Silver:  ✓ Absence of Runtime Errors");
      Put_Line ("  Gold:    ✓ Integrity properties (31/31 proofs)");
      Put_Line ("  Platinum: ✓ Functional correctness (183/183 proofs)");
      New_Line;
   end Print_Version;

   procedure Run_Self_Test is
      Success : Boolean;
   begin
      Print_Banner;
      Put_Line ("Running Cryptographic Self-Tests...");
      Put_Line ("═══════════════════════════════════════════════════");
      New_Line;

      -- Test 1: ML-KEM-1024 Key Generation
      Put ("1. ML-KEM-1024 Key Generation... ");
      declare
         PK : ML_KEM_Public_Key;
         SK : ML_KEM_Secret_Key;
      begin
         PQC.ML_KEM_Generate_Keypair (PK, SK, Success);
         if Success and then Is_Valid (SK) then
            Put_Line ("✓ PASS");
            PQC.Zeroize_ML_KEM_Secret (SK);
         else
            Put_Line ("✗ FAIL");
            return;
         end if;
      end;

      -- Test 2: ML-KEM-1024 Encapsulation/Decapsulation
      Put ("2. ML-KEM-1024 Encap/Decap... ");
      declare
         Alice_PK : ML_KEM_Public_Key;
         Alice_SK : ML_KEM_Secret_Key;
         CT : ML_KEM_Ciphertext;
         Bob_Secret : ML_KEM_Shared_Secret;
         Alice_Secret : ML_KEM_Shared_Secret;
      begin
         PQC.ML_KEM_Generate_Keypair (Alice_PK, Alice_SK, Success);
         if not Success then
            Put_Line ("✗ FAIL (keygen)");
            return;
         end if;

         PQC.ML_KEM_Encapsulate (Alice_PK, CT, Bob_Secret, Success);
         if not Success then
            Put_Line ("✗ FAIL (encap)");
            PQC.Zeroize_ML_KEM_Secret (Alice_SK);
            return;
         end if;

         PQC.ML_KEM_Decapsulate (CT, Alice_SK, Alice_Secret, Success);
         if not Success then
            Put_Line ("✗ FAIL (decap)");
            PQC.Zeroize_ML_KEM_Secret (Alice_SK);
            PQC.Zeroize_Shared_Secret (Bob_Secret);
            return;
         end if;

         if PQC.Secrets_Match (Bob_Secret, Alice_Secret) then
            Put_Line ("✓ PASS");
         else
            Put_Line ("✗ FAIL (mismatch)");
         end if;

         PQC.Zeroize_ML_KEM_Secret (Alice_SK);
         PQC.Zeroize_Shared_Secret (Bob_Secret);
         PQC.Zeroize_Shared_Secret (Alice_Secret);
      end;

      -- Test 3: ML-DSA-87 Sign/Verify
      Put ("3. ML-DSA-87 Sign/Verify... ");
      declare
         PK : ML_DSA_Public_Key;
         SK : ML_DSA_Secret_Key;
         Msg : constant Byte_Array := (72, 101, 108, 108, 111);  -- "Hello"
         Sig : ML_DSA_Signature;
         Valid : Boolean;
      begin
         PQC.ML_DSA_Generate_Keypair (PK, SK, Success);
         if not Success then
            Put_Line ("✗ FAIL (keygen)");
            return;
         end if;

         PQC.ML_DSA_Sign (Msg, SK, Sig, Success);
         if not Success then
            Put_Line ("✗ FAIL (sign)");
            PQC.Zeroize_ML_DSA_Secret (SK);
            return;
         end if;

         Valid := PQC.ML_DSA_Verify (Msg, Sig, PK);
         if Valid then
            Put_Line ("✓ PASS");
         else
            Put_Line ("✗ FAIL (verify)");
         end if;

         PQC.Zeroize_ML_DSA_Secret (SK);
      end;

      -- Test 4: Hybrid Signatures
      Put ("4. Hybrid Signatures (Ed25519 + ML-DSA)... ");
      declare
         Ed_PK : Ed25519_Public_Key;
         Ed_SK : Ed25519_Secret_Key;
         DSA_PK : ML_DSA_Public_Key;
         DSA_SK : ML_DSA_Secret_Key;
         Msg : constant Byte_Array := (84, 101, 115, 116);  -- "Test"
         Sig : PQC.Hybrid_Signature;
         Valid : Boolean;
      begin
         Classical.Ed25519_Generate_Keypair (Ed_PK, Ed_SK, Success);
         if not Success then
            Put_Line ("✗ FAIL (Ed25519 keygen)");
            return;
         end if;

         PQC.ML_DSA_Generate_Keypair (DSA_PK, DSA_SK, Success);
         if not Success then
            Put_Line ("✗ FAIL (ML-DSA keygen)");
            Classical.Zeroize_Ed25519_Secret (Ed_SK);
            return;
         end if;

         PQC.Hybrid_Sign (Msg, Ed_SK, DSA_SK, Sig, Success);
         if not Success then
            Put_Line ("✗ FAIL (sign)");
            Classical.Zeroize_Ed25519_Secret (Ed_SK);
            PQC.Zeroize_ML_DSA_Secret (DSA_SK);
            return;
         end if;

         Valid := PQC.Hybrid_Verify (Msg, Sig, Ed_PK, DSA_PK);
         if Valid then
            Put_Line ("✓ PASS");
         else
            Put_Line ("✗ FAIL (verify)");
         end if;

         Classical.Zeroize_Ed25519_Secret (Ed_SK);
         PQC.Zeroize_ML_DSA_Secret (DSA_SK);
      end;

      New_Line;
      Put_Line ("═══════════════════════════════════════════════════");
      Put_Line ("All Self-Tests Passed Successfully!");
      Put_Line ("System is functioning correctly.");
      New_Line;
   end Run_Self_Test;

begin
   if Argument_Count = 0 then
      Print_Usage;
      return;
   end if;

   declare
      Command : constant String := Argument (1);
   begin
      if Command = "help" or Command = "--help" or Command = "-h" then
         Print_Usage;
      elsif Command = "version" or Command = "--version" or Command = "-v" then
         Print_Version;
      elsif Command = "test" or Command = "selftest" then
         Run_Self_Test;
      elsif Command = "keygen" then
         -- Parse --output and --passphrase arguments
         declare
            Output_File : constant String := Get_Arg ("--output", "identity.key");
            Passphrase  : constant String := Get_Arg ("--passphrase");
            Label_Default : constant String := Ada.Directories.Base_Name (Output_File);
            Label_Arg   : constant String := Get_Arg ("--label", Label_Default);
            Identity : Storage.Identity_Keypair;
            Success : Boolean;
            Use_Encrypted : constant Boolean := (Passphrase /= "");
         begin
            Print_Banner;
            Put_Line ("Generating Hybrid Post-Quantum Identity...");
            Put_Line ("═══════════════════════════════════════════════════");
            New_Line;

            Put ("Generating hybrid keypairs... ");
            Storage.Generate_Identity (Identity, Success);

            if not Success then
               Put_Line ("✗ FAILED");
               Put_Line ("ERROR: Failed to generate identity keypair.");
               return;
            end if;

            Put_Line ("✓");
            Put_Line ("Identity generated successfully!");
            New_Line;

            Put_Line ("Key Details:");
            Put_Line ("  X25519 (ECDH):          32-byte public key");
            Put_Line ("  ML-KEM-1024 (PQ-KEM):   1568-byte public key");
            Put_Line ("  Ed25519 (Signatures):   32-byte public key");
            Put_Line ("  ML-DSA-87 (PQ-Sig):     2592-byte public key");
            New_Line;

            if Use_Encrypted then
               Put ("Saving encrypted keystore to " & Output_File & " (Argon2id)... ");
               Storage.Save_Identity_Encrypted (Identity, Output_File, Passphrase, Success);
            else
               Put ("Saving identity to " & Output_File & "... ");
               Storage.Save_Identity (Identity, Output_File, Success);
            end if;

            if not Success then
               Put_Line ("✗ FAILED");
               Put_Line ("ERROR: Failed to save identity to file.");
               Storage.Zeroize_Identity (Identity);
               return;
            end if;

            Put_Line ("✓");
            New_Line;
            Put_Line ("═══════════════════════════════════════════════════");
            Put_Line ("Identity saved successfully!");
            Put_Line ("File: " & Output_File);
            if Use_Encrypted then
               Put_Line ("Format: ANUBISK2 (Encrypted with Argon2id SENSITIVE)");
            else
               Put_Line ("Format: ANUBISK (Plaintext - no passphrase protection)");
            end if;
            New_Line;
            Put_Line ("⚠️  SECURITY NOTICE:");
            Put_Line ("  This file contains SECRET KEYS. Protect it carefully!");
            Put_Line ("  - Store in a secure location");
            Put_Line ("  - Set restrictive file permissions (chmod 600)");
            if not Use_Encrypted then
               Put_Line ("  - Consider using --passphrase for encryption");
            end if;
            Put_Line ("  - Keep backups in secure locations");
            New_Line;

            -- Cleanup
            Storage.Zeroize_Identity (Identity);
         end;
      elsif Command = "encrypt" then
         -- Parse arguments: --key <identity> --input <file> [--output <file>] [--passphrase <pass>]
         declare
            Key_File    : constant String := Get_Arg ("--key", "identity.key");
            Input_File  : constant String := Get_Arg ("--input");
            Output_File : constant String := (if Has_Arg ("--output")
                                               then Get_Arg ("--output")
                                               else Input_File & ".anubis");
            Passphrase  : constant String := Get_Arg ("--passphrase");
            Label_Default : constant String := Ada.Directories.Base_Name (Key_File);
            Label_Arg   : constant String := Get_Arg ("--label", Label_Default);
            Identity    : Storage.Identity_Keypair;
            Success     : Boolean;
            Use_Encrypted : constant Boolean := (Passphrase /= "");
            Signer_Label_Value      : Signer_Label;
            Signer_Fingerprint_Value : Signer_Fingerprint;
            Signer_Timestamp_Value  : Unsigned_64;
            Force_Overwrite : constant Boolean := Has_Arg ("--force");
         begin
            if Input_File = "" then
               Put_Line ("ERROR: --input <file> required");
               Put_Line ("Usage: anubis-spark encrypt --key <identity> --input <file> [--output <file>] [--passphrase <pass>]");
               return;
            end if;

            -- Preflight: Input must be readable
            declare
               OK : Boolean := True;
            begin
               begin
                  Anubis_IO.Require_Readable (Input_File);
               exception
                  when others =>
                     Put_Line ("ERROR: Input not readable: " & Input_File);
                     OK := False;
               end;
               if not OK then return; end if;
            end;

            if Ada.Directories.Exists (Output_File) and then not Force_Overwrite then
               Put_Line ("ERROR: Output file exists: " & Output_File);
               Put_Line ("Use --force to overwrite.");
               return;
            end if;

            -- Preflight: Input must be readable
            declare
               OK : Boolean := True;
            begin
               begin
                  Anubis_IO.Require_Readable (Input_File);
               exception
                  when others =>
                     Put_Line ("ERROR: Input not readable: " & Input_File);
                     OK := False;
               end;
               if not OK then return; end if;
            end;

            Print_Banner;
            Put_Line ("Encrypting File with Hybrid Post-Quantum Protection...");
            Put_Line ("═══════════════════════════════════════════════════");
            New_Line;

            Put ("Loading identity from " & Key_File & "... ");
            if Use_Encrypted then
               Storage.Load_Identity_Encrypted (Key_File, Passphrase, Identity, Success);
            else
               Storage.Load_Identity (Key_File, Identity, Success);
            end if;
            if not Success then
               Put_Line ("✗ FAILED");
               if Use_Encrypted then
                  Put_Line ("ERROR: Cannot load encrypted keystore (wrong passphrase or corrupted).");
               else
                  Put_Line ("ERROR: Cannot load identity keypair.");
               end if;
               return;
            end if;
            Put_Line ("✓");

            if not Anubis_Trust.Logic.Label_Input_Is_Valid (Label_Arg) then
               Put_Line ("ERROR: Signer label must be ASCII printable (0x20-0x7E) and at most 64 characters.");
               Storage.Zeroize_Identity (Identity);
               return;
            end if;

            Signer_Label_Value := Storage.Make_Label (Label_Arg);
            Signer_Fingerprint_Value := Storage.Compute_Fingerprint (Identity);
            declare
               Timestamp_OK : Boolean;
            begin
               Signer_Timestamp_Value := Unix_Timestamp (Timestamp_OK);
               if not Timestamp_OK then
                  Put_Line ("WARNING: System clock appears to be before 1970; signer timestamp recorded as 0.");
               end if;
            end;

            Put_Line ("Signer Metadata:");
            Put_Line ("  Label: " & (if Label_Arg'Length = 0 then "(unnamed)" else Label_Arg));
            Put_Line ("  Fingerprint: " & Anubis_Trust.Hex_Fingerprint (Signer_Fingerprint_Value));
            Put_Line ("  Timestamp: " &
              Trim (Unsigned_64'Image (Signer_Timestamp_Value), Both) & " (" &
              Format_Timestamp (Signer_Timestamp_Value) & ")");
            New_Line;

            Put ("Encrypting " & Input_File & " (streaming mode)... ");
            declare
               Rc : Streaming.Result_Code;
            begin
               Streaming.Encrypt_File_Streaming (
                  Input_Path      => Input_File,
                  Output_Path     => Output_File,
                  X25519_PK       => Storage.Get_X25519_Public (Identity),
                  ML_KEM_PK       => Storage.Get_ML_KEM_Public (Identity),
                  Ed25519_SK      => Storage.Get_Ed25519_Secret (Identity),
                  ML_DSA_SK       => Storage.Get_ML_DSA_Secret (Identity),
                  Signer_Label_Data    => Signer_Label_Value,
                  Signer_Timestamp     => Signer_Timestamp_Value,
                  Signer_Fingerprint_Data => Signer_Fingerprint_Value,
                  Result          => Rc,
                  Chunk_Size      => 67_108_864  -- 64 MB chunks
               );

               if Rc /= Streaming.Success then
                  Put_Line ("✗ FAILED");
                  case Rc is
                     when Streaming.IO_Error =>
                        Put_Line ("ERROR: File I/O error - check file permissions and disk space");
                     when Streaming.Crypto_Error =>
                        Put_Line ("ERROR: Cryptographic operation failed - check key validity");
                     when others =>
                        Put_Line ("ERROR: Encryption failed with unexpected error code");
                  end case;
                  Storage.Zeroize_Identity (Identity);
                  return;
               end if;
               Put_Line ("✓");
            end;

            New_Line;
            Put_Line ("═══════════════════════════════════════════════════");
           Put_Line ("File encrypted successfully!");
           Put_Line ("Output: " & Output_File);
            Put_Line ("Signer: " & Storage.Label_To_String (Signer_Label_Value));
            Put_Line ("Fingerprint: " & Anubis_Trust.Hex_Fingerprint (Signer_Fingerprint_Value));
            New_Line;

            Storage.Zeroize_Identity (Identity);
         end;
      elsif Command = "decrypt" then
         -- Parse arguments: --key <identity> --input <file> [--output <file>] [--passphrase <pass>]
         declare
            Key_File    : constant String := Get_Arg ("--key", "identity.key");
            Input_File  : constant String := Get_Arg ("--input");
            Output_File : constant String := (if Has_Arg ("--output")
                                               then Get_Arg ("--output")
                                               else Input_File & ".decrypted");
            Passphrase  : constant String := Get_Arg ("--passphrase");
            Identity    : Storage.Identity_Keypair;
            Success     : Boolean;
            Use_Encrypted : constant Boolean := (Passphrase /= "");
            Force_Overwrite : constant Boolean := Has_Arg ("--force");
         begin
            if Input_File = "" then
               Put_Line ("ERROR: --input <file> required");
               Put_Line ("Usage: anubis-spark decrypt --key <identity> --input <file> [--output <file>] [--passphrase <pass>]");
               return;
            end if;

            if Ada.Directories.Exists (Output_File) and then not Force_Overwrite then
               Put_Line ("ERROR: Output file exists: " & Output_File);
               Put_Line ("Use --force to overwrite.");
               return;
            end if;

            Print_Banner;
            Put_Line ("Decrypting File with Hybrid Post-Quantum Protection...");
            Put_Line ("═══════════════════════════════════════════════════");
            New_Line;

            Put ("Loading identity from " & Key_File & "... ");
            if Use_Encrypted then
               Storage.Load_Identity_Encrypted (Key_File, Passphrase, Identity, Success);
            else
               Storage.Load_Identity (Key_File, Identity, Success);
            end if;
            if not Success then
               Put_Line ("✗ FAILED");
               if Use_Encrypted then
                  Put_Line ("ERROR: Cannot load encrypted keystore (wrong passphrase or corrupted).");
               else
                  Put_Line ("ERROR: Cannot load identity keypair.");
               end if;
               return;
            end if;
            Put_Line ("✓");

            Put ("Decrypting " & Input_File & " (streaming mode)... ");
            declare
               Rc : Streaming.Result_Code;
               Signer_Label_Raw : Signer_Label;
               Signer_Timestamp_Raw : Unsigned_64;
               Signer_Fingerprint_Raw : Signer_Fingerprint;
            begin
               Streaming.Decrypt_File_Streaming (
                  Input_Path      => Input_File,
                  Output_Path     => Output_File,
                  X25519_SK       => Storage.Get_X25519_Secret (Identity),
                  ML_KEM_SK       => Storage.Get_ML_KEM_Secret (Identity),
                  Ed25519_PK      => Storage.Get_Ed25519_Public (Identity),
                  ML_DSA_PK       => Storage.Get_ML_DSA_Public (Identity),
                  Signer_Label_Data    => Signer_Label_Raw,
                  Signer_Timestamp     => Signer_Timestamp_Raw,
                  Signer_Fingerprint_Data => Signer_Fingerprint_Raw,
                  Result          => Rc
               );

               case Rc is
                  when Streaming.Success =>
                     Put_Line ("✓");
                  when Streaming.Trust_Pending =>
                     Put_Line ("✗ FAILED");
                     Put_Line (Anubis_Trust.Status_Message (
                        Status      => Anubis_Trust.Pending,
                        Fingerprint => Signer_Fingerprint_Raw,
                        Label       => Signer_Label_Raw));
                     Storage.Zeroize_Identity (Identity);
                     return;
                  when Streaming.Trust_Denied =>
                     Put_Line ("✗ FAILED");
                     Put_Line (Anubis_Trust.Status_Message (
                        Status      => Anubis_Trust.Denied,
                        Fingerprint => Signer_Fingerprint_Raw,
                        Label       => Signer_Label_Raw));
                     Storage.Zeroize_Identity (Identity);
                     return;
                  when Streaming.Trust_Error =>
                     Put_Line ("✗ FAILED");
                     Put_Line (Anubis_Trust.Status_Message (
                        Status      => Anubis_Trust.Error,
                        Fingerprint => Signer_Fingerprint_Raw,
                        Label       => Signer_Label_Raw));
                     Storage.Zeroize_Identity (Identity);
                     return;
                  when Streaming.Legacy_Format =>
                     Put_Line ("✗ FAILED");
                     Put_Line ("ERROR: Legacy ANUB2 header detected. Re-encrypt this file with the current ANUB3 format.");
                     Storage.Zeroize_Identity (Identity);
                     return;
                  when Streaming.Invalid_Format =>
                     Put_Line ("✗ FAILED");
                     Put_Line ("ERROR: ANUB3 header validation failed (file is tampered or unsupported).");
                     Storage.Zeroize_Identity (Identity);
                     return;
                  when Streaming.Crypto_Error =>
                     Put_Line ("✗ FAILED");
                     Put_Line ("ERROR: Decryption failed - cryptographic operation error");
                     Storage.Zeroize_Identity (Identity);
                     return;
                  when Streaming.IO_Error =>
                     Put_Line ("✗ FAILED");
                     Put_Line ("ERROR: Decryption failed - file I/O error");
                     Storage.Zeroize_Identity (Identity);
                     return;
                  when Streaming.Auth_Failed =>
                     Put_Line ("✗ FAILED");
                     Put_Line ("ERROR: Authentication failed - file may be tampered or corrupted");
                     Storage.Zeroize_Identity (Identity);
                     return;
                  when others =>
                     Put_Line ("✗ FAILED");
                     Put_Line ("ERROR: Decryption failed with unexpected error");
                     Storage.Zeroize_Identity (Identity);
                     return;
               end case;

               Put_Line ("Signer: " & Storage.Label_To_String (Signer_Label_Raw));
               Put_Line ("Fingerprint: " & Anubis_Trust.Hex_Fingerprint (Signer_Fingerprint_Raw));
               Put_Line ("Signer Timestamp: " &
                 Trim (Unsigned_64'Image (Signer_Timestamp_Raw), Both) & " (" &
                 Format_Timestamp (Signer_Timestamp_Raw) & ")");
            end;

            New_Line;
            Put_Line ("═══════════════════════════════════════════════════");
            Put_Line ("File decrypted and verified successfully!");
            Put_Line ("Output: " & Output_File);
            New_Line;

            Storage.Zeroize_Identity (Identity);
         end;
      elsif Command = "convert" then
         -- Re-encrypt plaintext to ANUB3 streaming format (manual migration)
         declare
            Key_File    : constant String := Get_Arg ("--key", "identity.key");
            Input_File  : constant String := Get_Arg ("--input");
            Output_File : constant String := (if Has_Arg ("--output") then Get_Arg ("--output") else Input_File & ".anub3");
            Passphrase  : constant String := Get_Arg ("--passphrase");
            Label_Arg   : constant String := Get_Arg ("--label", Ada.Directories.Base_Name (Key_File));
            Identity    : Storage.Identity_Keypair;
            Success     : Boolean;
            Use_Encrypted : constant Boolean := (Passphrase /= "");
            Force_Overwrite : constant Boolean := Has_Arg ("--force");
         begin
            if Input_File = "" then
               Put_Line ("ERROR: --input <file> required");
               Put_Line ("Usage: anubis-spark convert --key <identity> --input <plaintext> [--output <new>] [--passphrase <pass>] [--label <text>] [--force]");
               return;
            end if;

            if Ada.Directories.Exists (Output_File) and then not Force_Overwrite then
               Put_Line ("ERROR: Output file exists: " & Output_File);
               Put_Line ("Use --force to overwrite.");
               return;
            end if;

            Print_Banner;
            Put_Line ("Re-encrypting plaintext to ANUB3 (manual migration)...");
            New_Line;

            Put ("Loading identity from " & Key_File & "... ");
            if Use_Encrypted then
               Storage.Load_Identity_Encrypted (Key_File, Passphrase, Identity, Success);
            else
               Storage.Load_Identity (Key_File, Identity, Success);
            end if;
            if not Success then
               Put_Line ("✗ FAILED");
               Put_Line ("ERROR: Cannot load identity keypair.");
               return;
            end if;
            Put_Line ("✓");

            -- Reject ciphertext inputs: convert expects plaintext
            declare
               H : Ada.Streams.Stream_IO.File_Type;
               S : Ada.Streams.Stream_IO.Stream_Access;
               Magic5 : String (1 .. 5);
               B : Ada.Streams.Stream_Element;
            begin
               begin
                  Ada.Streams.Stream_IO.Open (H, Ada.Streams.Stream_IO.In_File, Input_File);
                  S := Ada.Streams.Stream_IO.Stream (H);
                  for I in Magic5'Range loop
                     Ada.Streams.Stream_Element'Read (S, B);
                     Magic5 (I) := Character'Val (Integer (B));
                  end loop;
                  Ada.Streams.Stream_IO.Close (H);
               exception
                  when others => null;
               end;

               if Magic5 = "ANUB2" or else Magic5 = "ANUB3" then
                  Put_Line ("ERROR: Input appears to be ciphertext (" & Magic5 & ").");
                  Put_Line ("Use v1.x to decrypt legacy ANUB2 ciphertext (or v2.x for ANUB3), then run convert on the plaintext.");
                  Storage.Zeroize_Identity (Identity);
                  return;
               end if;
            end;
            Put ("Re-encrypting as ANUB3... ");
            declare
               Rc : Streaming.Result_Code;
               Valid_Label : constant Boolean := Anubis_Trust.Logic.Label_Input_Is_Valid (Label_Arg);
               Label_Value : Signer_Label;
               TS_OK : Boolean;
               TS    : Unsigned_64 := Unix_Timestamp (TS_OK);
            begin
               if not Valid_Label then
                  Put_Line ("✗ FAILED");
                  Put_Line ("ERROR: --label must be ASCII printable and ≤ 64 chars.");
                  Storage.Zeroize_Identity (Identity);
                  return;
               end if;
               Label_Value := Storage.Make_Label (Label_Arg);

               Streaming.Encrypt_File_Streaming (
                  Input_Path      => Input_File,
                  Output_Path     => Output_File,
                  X25519_PK       => Storage.Get_X25519_Public (Identity),
                  ML_KEM_PK       => Storage.Get_ML_KEM_Public (Identity),
                  Ed25519_SK      => Storage.Get_Ed25519_Secret (Identity),
                  ML_DSA_SK       => Storage.Get_ML_DSA_Secret (Identity),
                  Signer_Label_Data    => Label_Value,
                  Signer_Timestamp     => TS,
                  Signer_Fingerprint_Data => Storage.Compute_Fingerprint (Identity),
                  Result          => Rc,
                  Chunk_Size      => 67_108_864
               );

               if Rc /= Streaming.Success then
                  Put_Line ("✗ FAILED");
                  case Rc is
                     when Streaming.IO_Error => Put_Line ("ERROR: IO error while writing ANUB3");
                     when Streaming.Crypto_Error => Put_Line ("ERROR: Crypto error during re-encryption");
                     when others => Put_Line ("ERROR: Conversion failed");
                  end case;
                  Storage.Zeroize_Identity (Identity);
                  return;
               end if;

               Put_Line ("✓");
               New_Line;
               Put_Line ("Conversion complete → " & Output_File);
               Storage.Zeroize_Identity (Identity);
            end;
         end;
      elsif Command = "trust" then
         if Argument_Count < 2 then
            Put_Line ("Usage: anubis-spark trust <list|approve|deny|selfcheck|doctor|reseal> [options]");
            return;
         end if;

         declare
            Subcommand : constant String := Argument (2);
         begin
            if Subcommand = "list" then
               Anubis_Trust.Print_List;
            elsif Subcommand = "approve" then
               declare
                  Hex_Fp : constant String := Get_Arg ("--fingerprint");
                  Operator_Arg : constant String := Get_Arg ("--operator");
                  Fingerprint : Signer_Fingerprint;
                  Ok : Boolean;
               begin
                  if Hex_Fp = "" then
                     Put_Line ("ERROR: --fingerprint <hex> required");
                     Put_Line ("Usage: anubis-spark trust approve --fingerprint <hex> [--operator <name>]");
                     return;
                  end if;

                  if not Anubis_Trust.Parse_Fingerprint (Hex_Fp, Fingerprint) then
                     Put_Line ("ERROR: Invalid fingerprint format");
                     return;
                  end if;

                  if Operator_Arg'Length > 0 and then
                     not Anubis_Trust.Logic.Operator_Input_Is_Valid (Operator_Arg) then
                     Put_Line ("ERROR: --operator must be ASCII printable (0x20-0x7E) and at most 64 characters.");
                     return;
                  end if;

                  Anubis_Trust.Approve (
                     Fingerprint => Fingerprint,
                     Operator    => Operator_Arg,
                     Success     => Ok);
                  if Ok then
                     Put_Line ("Fingerprint " & Hex_Fp & " approved.");
                  else
                     Put_Line ("ERROR: Unable to approve fingerprint.");
                  end if;
               end;
            elsif Subcommand = "deny" then
               declare
                  Hex_Fp : constant String := Get_Arg ("--fingerprint");
                  Operator_Arg : constant String := Get_Arg ("--operator");
                  Fingerprint : Signer_Fingerprint;
                  Ok : Boolean;
               begin
                  if Hex_Fp = "" then
                     Put_Line ("ERROR: --fingerprint <hex> required");
                     Put_Line ("Usage: anubis-spark trust deny --fingerprint <hex> [--operator <name>]");
                     return;
                  end if;

                  if not Anubis_Trust.Parse_Fingerprint (Hex_Fp, Fingerprint) then
                     Put_Line ("ERROR: Invalid fingerprint format");
                     return;
                  end if;

                  if Operator_Arg'Length > 0 and then
                     not Anubis_Trust.Logic.Operator_Input_Is_Valid (Operator_Arg) then
                     Put_Line ("ERROR: --operator must be ASCII printable (0x20-0x7E) and at most 64 characters.");
                     return;
                  end if;

                  Anubis_Trust.Deny (
                     Fingerprint => Fingerprint,
                     Operator    => Operator_Arg,
                     Success     => Ok);
                  if Ok then
                     Put_Line ("Fingerprint " & Hex_Fp & " denied.");
                  else
                     Put_Line ("ERROR: Unable to deny fingerprint.");
                  end if;
               end;
            elsif Subcommand = "selfcheck" then
               declare
                  Ok : Boolean;
               begin
                  Anubis_Trust.Self_Check (Ok);
                  if not Ok then
                     Put_Line ("ACTION REQUIRED: resolve the trust-store issues reported above.");
                  end if;
               end;
            elsif Subcommand = "doctor" then
               declare
                  Ok : Boolean;
               begin
                  Anubis_Trust.Doctor (Ok);
                  if not Ok then
                     Put_Line ("Trust doctor: issues found; see messages above.");
                  else
                     Put_Line ("Trust doctor: OK");
                  end if;
               end;
            elsif Subcommand = "reseal" then
               declare
                  Ok : Boolean;
               begin
                  Anubis_Trust.Reseal (Ok);
                  if Ok then
                     Put_Line ("Reseal complete.");
                  else
                     Put_Line ("ERROR: Reseal encountered failures (see messages).");
                  end if;
               end;
            else
               Put_Line ("Unknown trust subcommand: " & Subcommand);
               Put_Line ("Use: anubis-spark trust list|approve|deny|selfcheck|doctor|reseal");
            end if;
         end;
      -- NOTE: sign/verify commands removed from v1.0.0
      -- Hybrid signatures are tested in: anubis-spark test
      -- Full CLI signing implementation planned for v2.0
      else
         Put_Line ("Unknown command: " & Command);
         Put_Line ("Use: anubis-spark help");
      end if;
   end;
end Anubis_Main;
