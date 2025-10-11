-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Hybrid KDF Contracts
-- Proves: Derived keys are non-zero on success
-- Proves: Keys are zeroed on failure
-- Covers: X25519 + ML-KEM-1024 combination via HKDF
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Contracts; use Anubis_Contracts;

package Anubis_Hybrid_KDF is

   -------------------------------------------------------------------------
   -- Derive Hybrid Secret (X25519 + ML-KEM)
   -------------------------------------------------------------------------

   -- Combine X25519 (32B) and ML-KEM shared secret (32B) via HKDF-SHA256
   -- Proves: Success → output is non-zero
   -- Proves: Failure → output is all zeros
   procedure Derive_Hybrid_Secret
     (X25519_SS  : in  Key_32;
      MLKEM_SS   : in  Key_32;
      Hybrid_SS  : out Key_32;
      Success    : out Boolean) with
     Global  => null,
     Depends => (Hybrid_SS => (X25519_SS, MLKEM_SS),
                 Success   => (X25519_SS, MLKEM_SS)),
     Post    =>
       (if Success then
          -- Success: derived key is non-zero
          (for some I in Hybrid_SS'Range => Hybrid_SS (I) /= 0)
        else
          -- Failure: key is zeroed
          (for all I in Hybrid_SS'Range => Hybrid_SS (I) = 0));

   -------------------------------------------------------------------------
   -- Derive XChaCha20 Encryption Key
   -------------------------------------------------------------------------

   -- Derive XChaCha20 key from hybrid secret with domain separation
   -- Proves: Non-zero input → non-zero output on success
   -- Proves: Failure → output is all zeros
   procedure Derive_XChaCha20_Key
     (Hybrid_SS : in  Key_32;
      Key_Out   : out Key_32;
      Success   : out Boolean) with
     Global  => null,
     Pre     => (for some I in Hybrid_SS'Range => Hybrid_SS (I) /= 0),
     Depends => (Key_Out => Hybrid_SS,
                 Success => Hybrid_SS),
     Post    =>
       (if Success then
          -- Success: encryption key is non-zero
          (for some I in Key_Out'Range => Key_Out (I) /= 0)
        else
          -- Failure: key is zeroed
          (for all I in Key_Out'Range => Key_Out (I) = 0));

end Anubis_Hybrid_KDF;
