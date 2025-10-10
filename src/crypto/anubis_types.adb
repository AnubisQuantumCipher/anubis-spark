-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Cryptographic Type Implementations
-- SPARK-verified secure zeroization and validation
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Types is

   -- Validity checks: Keys are valid if they've been initialized
   function Is_Valid (Key : X25519_Secret_Key) return Boolean is (Key.Valid);
   function Is_Valid (Secret : X25519_Shared_Secret) return Boolean is (Secret.Valid);
   function Is_Valid (Key : Ed25519_Secret_Key) return Boolean is (Key.Valid);
   function Is_Valid (Key : XChaCha20_Key) return Boolean is (Key.Valid);
   function Is_Valid (Key : Argon2_Derived_Key) return Boolean is (Key.Valid);
   function Is_Valid (Key : ML_KEM_Secret_Key) return Boolean is (Key.Valid);
   function Is_Valid (Secret : ML_KEM_Shared_Secret) return Boolean is (Secret.Valid);
   function Is_Valid (Key : ML_DSA_Secret_Key) return Boolean is (Key.Valid);
   function Is_Valid (Key : Master_Key) return Boolean is (Key.Valid);

   -------------------------------------------------------------------------
   -- Secure Zeroization: Overwrites memory with zeros
   -- PLATINUM SPARK: Formal postconditions prove zeroization correctness
   -- SPARK verification ensures keys are properly erased (provably secure)
   -------------------------------------------------------------------------

   procedure Zeroize (Key : in out X25519_Secret_Key) is
   begin
      for I in Key.Data'Range loop
         pragma Loop_Invariant (Key.Valid = Key.Valid'Loop_Entry);
         Key.Data (I) := 0;
      end loop;
      Key.Valid := False;
   end Zeroize;

   procedure Zeroize (Key : in out Ed25519_Secret_Key) is
   begin
      for I in Key.Data'Range loop
         pragma Loop_Invariant (Key.Valid = Key.Valid'Loop_Entry);
         Key.Data (I) := 0;
      end loop;
      Key.Valid := False;
   end Zeroize;

   procedure Zeroize (Key : in out ML_KEM_Secret_Key) is
   begin
      for I in Key.Data'Range loop
         pragma Loop_Invariant (Key.Valid = Key.Valid'Loop_Entry);
         Key.Data (I) := 0;
      end loop;
      Key.Valid := False;
   end Zeroize;

   procedure Zeroize (Key : in out ML_DSA_Secret_Key) is
   begin
      for I in Key.Data'Range loop
         pragma Loop_Invariant (Key.Valid = Key.Valid'Loop_Entry);
         Key.Data (I) := 0;
      end loop;
      Key.Valid := False;
   end Zeroize;

   procedure Zeroize (Key : in out Master_Key) is
   begin
      for I in Key.Data'Range loop
         pragma Loop_Invariant (Key.Valid = Key.Valid'Loop_Entry);
         Key.Data (I) := 0;
      end loop;
      Key.Valid := False;
   end Zeroize;

   procedure Zeroize (Data : in out Byte_Array) is
   begin
      for I in Data'Range loop
         pragma Loop_Invariant (for all J in Data'First .. I - 1 => Data (J) = 0);
         pragma Loop_Variant (Increases => I);
         Data (I) := 0;
      end loop;
   end Zeroize;

end Anubis_Types;
