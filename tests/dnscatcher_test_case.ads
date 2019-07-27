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

-- @summary
-- A base class used for testing functionality of DNSCatcher with common
-- initialize and teardown code shared between modules
--
-- @details
-- All DNSCatcher tests descend from this test case as a method of simplifying
-- code handling. Some lesser versions may be needed for testing more basic
-- functionality
package DNSCatcher_Test_Case is
   -- DNSCatcher Test Type
   -- It handles the global state of the test such as config variables and the
   -- Logger
   --
   -- @value Capture_Config
   -- Configuration of the DNSCatcher Library
   --
   -- @value Logger_Task
   -- Local instance of the loger
   type DNSCatcher_Test_Type is abstract new Test_Cases.Test_Case with record
      Capture_Config : DNSCatcher.Config.Configuration;
      Logger_Task    : DNSCatcher.Utils.Logger.Logger;
   end record;

   -- Register tests for this individual test case
   --
   -- @value T
   -- Global config for test case
   procedure Register_Tests (T : in out DNSCatcher_Test_Type) is abstract;

   -- Setup the DNS Catcher library for each test case
   --
   -- @value T
   -- Global config for test case
   procedure Set_Up_Case (T : in out DNSCatcher_Test_Type);

   -- Teardown each test case
   --
   -- @value T
   -- Global config for test case
   procedure Tear_Down_Case (T : in out DNSCatcher_Test_Type);

   -- Provide name identifying the test case
   --
   -- @value T
   -- Global config for test case
   function Name
     (T : DNSCatcher_Test_Type)
      return Message_String is abstract;

end DNSCatcher_Test_Case;
