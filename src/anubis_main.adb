-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Hybrid Post-Quantum Zero-Knowledge File Encryption System
-- Main entry point
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

procedure Anubis_Main is
begin
   Put_Line ("╔══════════════════════════════════════════════════════════════════╗");
   Put_Line ("║  ANUBIS-SPARK v0.1.0                                             ║");
   Put_Line ("║  Hybrid Post-Quantum Zero-Knowledge File Encryption System       ║");
   Put_Line ("║                                                                  ║");
   Put_Line ("║  Security Level: NIST Level 5 (256-bit equivalent)              ║");
   Put_Line ("║  - Classical: X25519 + XChaCha20-Poly1305 + Ed25519             ║");
   Put_Line ("║  - Post-Quantum: ML-KEM-1024 + ML-DSA-87                        ║");
   Put_Line ("║  - Formal Verification: SPARK 2014                              ║");
   Put_Line ("╚══════════════════════════════════════════════════════════════════╝");
   New_Line;

   Put_Line ("Initializing hybrid post-quantum cryptographic system...");
   Put_Line ("[ ] Loading liboqs (Open Quantum Safe)");
   Put_Line ("[ ] Initializing ML-KEM-1024 (NIST FIPS 203)");
   Put_Line ("[ ] Initializing ML-DSA-87 (NIST FIPS 204)");
   Put_Line ("[ ] Verifying SPARK formal proofs");
   New_Line;

   Put_Line ("Status: Development build - bindings in progress");
   Put_Line ("Next: Creating Ada FFI bindings for liboqs...");
end Anubis_Main;
