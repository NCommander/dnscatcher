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

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;        use Ada.Exceptions;

with DNSCatcher.DNS;          use DNSCatcher.DNS;
with DNSCatcher.DNS.Client;
with DNSCatcher.Config;
with DNSCatcher.Utils.Logger; use DNSCatcher.Utils.Logger;

with DNSCatcher.DNS.Transaction_Manager;
use DNSCatcher.DNS.Transaction_Manager;


with DNSCatcher.Types; use DNSCatcher.Types;

procedure DNSC is
   DClient                 : DNSCatcher.DNS.Client.Client;
   Capture_Config          : DNSCatcher.Config.Configuration;
   Logger_Task             : DNSCatcher.Utils.Logger.Logger;
   Transaction_Manager_Ptr : DNS_Transaction_Manager_Task_Ptr;
   Outbound_Packet         : Raw_Packet_Record_Ptr;
   Logger_Packet           : DNSCatcher.Utils.Logger.Logger_Message_Packet_Ptr;

begin
   Capture_Config.Local_Listen_Port        := 41231;
   Capture_Config.Upstream_DNS_Server      := To_Unbounded_String ("4.2.2.2");
   Capture_Config.Upstream_DNS_Server_Port := 53;

   -- Configure the logger
   Capture_Config.Logger_Config.Log_Level := DEBUG;
   Capture_Config.Logger_Config.Use_Color := True;

   Logger_Task.Initialize (Capture_Config.Logger_Config);
   Logger_Task.Start;
   Logger_Packet := new Logger_Message_Packet;

   Transaction_Manager_Ptr := new DNS_Transaction_Manager_Task;

   -- Start up all tasks

   -- Generate our packet
   DClient.Create_Header;
   DClient.Add_Query (To_Unbounded_String ("casadevall.pro"), A, INternet);

   Outbound_Packet := DClient.Create_Packet (Capture_Config);
   Transaction_Manager_Ptr.From_Client_Resolver_Packet
     (Packet => Outbound_Packet,
      Local  => True);

   Logger_Queue.Add_Packet (Logger_Packet);

   delay 2.0;
   -- And tear it all down
   Transaction_Manager_Ptr.Stop;

   Logger_Task.Stop;
exception
   when Error : others =>
      begin
         Put (Standard_Error, "Unknown error: ");
         Put_Line (Standard_Error, Exception_Information (Error));
         Logger_Task.Stop;
      end;

end DNSC;
