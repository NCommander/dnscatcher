with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Test_Packet_Parser is
   type Packet_Parser_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Packet_Parser_Test);
   -- Register routines to be run

   function Name (T: Packet_Parser_Test) return Message_String;
   -- Provide name identifying the test case

   -- Test Routines:
   procedure Test_Simple_Add (T : in out Test_Cases.Test_Case'Class);
   procedure Test_2 (T : in out Test_Cases.Test_Case'Class);

end Test_Packet_Parser;
