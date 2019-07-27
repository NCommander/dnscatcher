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

pragma Ada_2012;
package body Test_Network_ASync_IO is

   procedure Register_Tests (T : in out Network_ASync_IO_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Setup_And_Teardown'Access, "Test Async IO Setup/Teardown");
   end Register_Tests;

   pragma Warnings (Off, "formal parameter ""T"" is not referenced");
   function Name
     (T : Network_ASync_IO_Test)
      return Message_String
   is
   begin
      return Format ("Network ASync_IO");
   end Name;

   procedure Test_Setup_And_Teardown (T : in out Test_Cases.Test_Case'Class) is
   begin
      null;
   end Test_Setup_And_Teardown;

end Test_Network_ASync_IO;
