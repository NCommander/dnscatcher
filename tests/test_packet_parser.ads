with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with DNS_Common.Config;
with DNS_Common.Logger;       use DNS_Common.Logger;

package Test_Packet_Parser is
   type Packet_Parser_Test is new Test_Cases.Test_Case with record
      Capture_Config          : DNS_Common.Config.Configuration_Ptr;
      Logger_Task             : DNS_Common.Logger.Logger;
   end record;

   -- Setup the DNS Catcher library
   procedure Set_Up_Case(T: in out Packet_Parser_Test);

   -- Teardown here
   procedure Tear_Down_Case(T: in out Packet_Parser_Test);

   procedure Register_Tests (T: in out Packet_Parser_Test);
   -- Register routines to be run

   function Name (T: Packet_Parser_Test) return Message_String;
   -- Provide name identifying the test case

   -- Test Routines:
   procedure Test_Parse_A_Record (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Parse_SOA_Record (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Parse_CNAME_Record (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Parse_NS_Record (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Parse_PTR_Record (T : in out Test_Cases.Test_Case'Class);

end Test_Packet_Parser;
