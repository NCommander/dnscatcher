-- Copyright 2019 Michael Casadevall <michael@casadevall.pro>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with DNSCatcher.Config;
with DNSCatcher.Utils.Logger; use DNSCatcher.Utils.Logger;

package Test_Packet_Parser is
   type Packet_Parser_Test is new Test_Cases.Test_Case with record
      Capture_Config : DNSCatcher.Config.Configuration_Ptr;
      Logger_Task    : DNSCatcher.Utils.Logger.Logger;
   end record;

   -- Setup the DNS Catcher library
   procedure Set_Up_Case (T : in out Packet_Parser_Test);

   -- Teardown here
   procedure Tear_Down_Case (T : in out Packet_Parser_Test);

   procedure Register_Tests (T : in out Packet_Parser_Test);
   -- Register routines to be run

   function Name
     (T : Packet_Parser_Test)
     return Message_String;
   -- Provide name identifying the test case

   -- Test Routines:
   procedure Test_Parse_A_Record (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Parse_SOA_Record (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Parse_CNAME_Record (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Parse_NS_Record (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Parse_PTR_Record (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Parse_OPT_Record (T : in out Test_Cases.Test_Case'Class);

end Test_Packet_Parser;
