pragma Ada_2012;

package body Test_Packet_Parser is

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Packet_Parser_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, Test_Simple_Add'Access, "Example");
      Register_Routine(T, Test_2'Access, "Example2");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   pragma Warnings (Off, "formal parameter ""T"" is not referenced");
   function Name (T: Packet_Parser_Test) return Message_String is
   begin
      return Format("Packet Parser Test");
   end Name;
   pragma Warnings (On, "formal parameter ""T"" is not referenced");

   ---------------------
   -- Test_Simple_Add --
   ---------------------

   procedure Test_Simple_Add (T : in out Test_Cases.Test_Case'Class) is
   begin
      null;
   end Test_Simple_Add;

   procedure Test_2  (T : in out Test_Cases.Test_Case'Class) is
   begin
      null;
   end Test_2;

end Test_Packet_Parser;
