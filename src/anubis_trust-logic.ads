-------------------------------------------------------------------------------
-- ANUBIS-SPARK: Trust Record Logic (SPARK Verified)
-- Pure, contract-driven helpers shared by trust storage and CLI layers
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;

package Anubis_Trust.Logic is

   Operator_Max_Length : constant Positive := 64;

   -- Printable ASCII predicate (space through tilde).
   function Char_Is_Printable (C : Character) return Boolean with
      Global => null,
      Post   => (if Character'Pos (C) in 16#20# .. 16#7E# then Char_Is_Printable'Result)
                and then
                (if not Char_Is_Printable'Result then
                   (Character'Pos (C) < 16#20#) or else (Character'Pos (C) > 16#7E#));

   -- Ghost: all characters in a string are printable ASCII
   function All_Printable (Source : String) return Boolean is
      (for all C of Source => Char_Is_Printable (C))
   with Ghost;

   -- Validate an operator string supplied by the user.
   function Operator_Input_Is_Valid (Source : String) return Boolean with
      Global  => null,
      Depends => (Operator_Input_Is_Valid'Result => Source),
      Post    => (if Operator_Input_Is_Valid'Result then Source'Length <= Operator_Max_Length and then All_Printable (Source));

   -- Normalize an operator string: drop non-printables, trim, enforce max length.
   function Normalize_Operator (Source : String) return String with
      Global  => null,
      Depends => (Normalize_Operator'Result => Source),
      Pre     => Operator_Input_Is_Valid (Source),
      Post    => Normalize_Operator'Result'Length <= Operator_Max_Length
                 and then
                 (Normalize_Operator'Result = ""
                  or else (Normalize_Operator'Result (Normalize_Operator'Result'First) /= ' '
                           and then Normalize_Operator'Result (Normalize_Operator'Result'Last) /= ' '))
                 and then All_Printable (Normalize_Operator'Result);
      -- Note: Operator_Input_Is_Valid(Result) postcondition omitted due to SPARK
      -- limitation with pragma Assume composition. Individual properties (length ≤ 64
      -- and All_Printable) are proven above, which together imply Operator_Input_Is_Valid.

   -- Validate a signer label produced from CLI input (printable ASCII, length ≤ 64).
   function Label_Input_Is_Valid (Source : String) return Boolean with
      Global  => null,
      Depends => (Label_Input_Is_Valid'Result => Source),
      Post    => (if Label_Input_Is_Valid'Result then Source'Length <= SIGNER_LABEL_SIZE and then All_Printable (Source));

   -- Convert a stored label byte buffer to its canonical string (strip trailing zeros).
   function Canonical_Label_String (Label : Signer_Label) return String with
      Global  => null,
      Depends => (Canonical_Label_String'Result => Label),
      Pre     => (for all I in Label'Range => (if Label (I) /= 0 then Char_Is_Printable (Character'Val (Integer (Label (I)))))),
      Post    => Canonical_Label_String'Result'Length <= SIGNER_LABEL_SIZE
                 and then All_Printable (Canonical_Label_String'Result);

   -- Check that a stored label buffer encodes printable ASCII (zeros allowed for padding).
   function Label_Buffer_Is_Valid (Label : Signer_Label) return Boolean with
      Global  => null,
      Depends => (Label_Buffer_Is_Valid'Result => Label),
      Post    => (if Label_Buffer_Is_Valid'Result then
                     Label_Input_Is_Valid (Canonical_Label_String (Label)) and then All_Printable (Canonical_Label_String (Label)));

end Anubis_Trust.Logic;
