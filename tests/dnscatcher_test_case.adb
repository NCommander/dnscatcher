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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body DNSCatcher_Test_Case is
   procedure Set_Up_Case (T : in out DNSCatcher_Test_Type) is
   begin
      T.Capture_Config.Local_Listen_Port        := 53;
      T.Capture_Config.Upstream_DNS_Server := To_Unbounded_String ("4.2.2.2");
      T.Capture_Config.Upstream_DNS_Server_Port := 53;

      -- Configure the logger
      T.Capture_Config.Logger_Config.Log_Level := DEBUG;
      T.Capture_Config.Logger_Config.Use_Color := True;

      T.Logger_Task.Initialize (T.Capture_Config.Logger_Config);
      T.Logger_Task.Start;
   end Set_Up_Case;

   procedure Tear_Down_Case (T : in out DNSCatcher_Test_Type) is
   begin
      T.Logger_Task.Stop;
   end Tear_Down_Case;

   ----------
   -- Name --
   ----------

   pragma Warnings (Off, "formal parameter ""T"" is not referenced");
   function Name
     (T : DNSCatcher_Test_Type)
      return Message_String
   is
   begin
      return Format ("Packet Parser Test");
   end Name;

end DNSCatcher_Test_Case;
