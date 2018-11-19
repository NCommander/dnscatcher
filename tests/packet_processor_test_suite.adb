pragma Ada_2012;

with Test_Packet_Parser;

package body Packet_Processor_Test_Suite is
   use AUnit.Test_Suites;

   -- Statically allocate test suite;
   Result : aliased Test_Suite;

   -- And the test cases
   Test_Packet_Parser_Ptr : aliased Test_Packet_Parser.Packet_Parser_Test;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Packet_Parser_Ptr'Access);
      return (Result'Access);
   end Suite;

end Packet_Processor_Test_Suite;
