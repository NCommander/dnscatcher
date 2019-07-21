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

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;

with GNAT.Ctrl_C; use GNAT.Ctrl_C;

with Packet_Catcher;
with Signal_Handlers;

with DNSCatcher.Config;

procedure DNSCatcherD is
begin
   Install_Handler (Handler => Signal_Handlers.SIGINT_Handler'Access);
   Packet_Catcher.Run_Catcher;
exception
   when Error : Ada.IO_Exceptions.Name_Error =>
      begin
         Put (Standard_Error, "FATAL: Unable to open config file: ");
         Put_Line (Standard_Error, Exception_Message (Error));
         Packet_Catcher.Stop_Catcher;
      end;
   when Error : DNSCatcher.Config.Missing_Mandatory_Config_Option =>
      begin
         Put (Standard_Error, "FATAL: Missing mandatory config line. ");
         Put_Line (Standard_Error, Exception_Message (Error));
         Packet_Catcher.Stop_Catcher;
      end;
   when Error : DNSCatcher.Config.Malformed_Line =>
      begin
         Put (Standard_Error, "FATAL: Unable to parse config file. ");
         Put_Line (Standard_Error, Exception_Message (Error));
         Packet_Catcher.Stop_Catcher;
      end;
   when Error : others =>
      begin
         Put (Standard_Error, "FATAL: Unknown error: ");
         Put_Line (Standard_Error, Exception_Information (Error));
         Packet_Catcher.Stop_Catcher;
      end;
end DNSCatcherD;
