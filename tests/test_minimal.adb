-------------------------------------------------------------------------------
-- Minimal test - just initialize liboqs
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with OQS_Common; use OQS_Common;

procedure Test_Minimal is
begin
   Put_Line ("Starting minimal test...");

   Put_Line ("Calling OQS_init...");
   OQS_init;

   Put_Line ("SUCCESS: liboqs initialized!");

   Put_Line ("Calling OQS_destroy...");
   OQS_destroy;

   Put_Line ("SUCCESS: Test complete!");
end Test_Minimal;
