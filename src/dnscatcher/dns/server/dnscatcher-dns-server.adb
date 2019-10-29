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

with Ada.Unchecked_Conversion;

with DNSCatcher.Config;
with DNSCatcher.Utils.Logger; use DNSCatcher.Utils.Logger;

with DNSCatcher.Network.ASync_IO;

package body DNSCatcher.DNS.Server is
   Shutting_Down : Boolean := False;

   procedure Start_Server is
      -- Input and Output Sockets
      Capture_Config : DNSCatcher.Config.Configuration;

      Logger_Task   : DNSCatcher.Utils.Logger.Logger;
      Logger_Packet : DNSCatcher.Utils.Logger.Logger_Message_Packet_Ptr;
   begin
      -- Load the config file from disk
      DNSCatcher.Config.Initialize (Capture_Config);
      DNSCatcher.Config.Read_Cfg_File
        (Capture_Config, "conf/dnscatcherd.conf");

      -- Configure the logger
      Capture_Config.Logger_Config.Log_Level := DEBUG;
      Capture_Config.Logger_Config.Use_Color := True;

      Logger_Task.Initialize (Capture_Config.Logger_Config);

      -- Connect the packet queue and start it all up
      Logger_Task.Start;
      Logger_Packet := new DNSCatcher.Utils.Logger.Logger_Message_Packet;
      Logger_Packet.Log_Message (NOTICE, "DNSCatcher starting up ...");
      DNSCatcher.Utils.Logger.Logger_Queue.Add_Packet (Logger_Packet);

--      Transaction_Manager_Ptr.Set_Packet_Queue
--        (Sender_Interface.Get_Packet_Queue_Ptr);
--      Transaction_Manager_Ptr.Start;

      loop
         if Shutting_Down
         then
            goto Shutdown_Procedure;
         else
            delay 1.0;
         end if;
      end loop;

      <<Shutdown_Procedure>>
      Logger_Task.Stop;

   end Start_Server;

   -- And shutdown
   procedure Stop_Server is
   begin
      Shutting_Down := True;
   end Stop_Server;

end DNSCatcher.DNS.Server;
