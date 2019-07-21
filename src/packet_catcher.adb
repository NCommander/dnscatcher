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

with Ada.Unchecked_Deallocation;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Sockets;          use GNAT.Sockets;

with DNSCatcher.Config;
with DNSCatcher.Utils.Logger; use DNSCatcher.Utils.Logger;
with DNSCatcher.Network.UDP.Receiver;
with DNSCatcher.Network.UDP.Sender;

with DNSCatcher.DNS.Transaction_Manager;
use DNSCatcher.DNS.Transaction_Manager;

package body Packet_Catcher is
   Shutting_Down : Boolean := False;

   procedure Run_Catcher is
      -- Input and Output Sockets
      DNS_Transactions : DNSCatcher.DNS.Transaction_Manager.DNS_Transaction_Manager_Task;
      Capture_Config     : DNSCatcher.Config.Configuration_Ptr;
      Receiver_Interface : DNSCatcher.Network.UDP.Receiver.UDP_Receiver_Interface;
      Sender_Interface : DNSCatcher.Network.UDP.Sender.UDP_Sender_Interface;
      Logger_Task             : DNSCatcher.Utils.Logger.Logger;
      Transaction_Manager_Ptr : DNS_Transaction_Manager_Task_Ptr;
      Socket                  : Socket_Type;
      Logger_Packet           : DNSCatcher.Utils.Logger.Logger_Message_Packet_Ptr;

      procedure Free_Transaction_Manager is new Ada.Unchecked_Deallocation
        (Object => DNS_Transaction_Manager_Task,
         Name   => DNS_Transaction_Manager_Task_Ptr);
      procedure Free_DNSCatacher_Config is new Ada.Unchecked_Deallocation
        (Object => DNSCatcher.Config.Configuration,
         Name   => DNSCatcher.Config.Configuration_Ptr);
   begin
      -- Load the config file from disk
      Capture_Config :=
        DNSCatcher.Config.Parse_Config_File("conf/dnscatcherd.conf");

      -- Configure the logger
      Capture_Config.Logger_Config.Log_Level := DEBUG;
      Capture_Config.Logger_Config.Use_Color := True;

      Logger_Task.Initialize (Capture_Config.Logger_Config);
      Transaction_Manager_Ptr := new DNS_Transaction_Manager_Task;

      -- Connect the packet queue and start it all up
      Logger_Task.Start;
      Logger_Packet := new DNSCatcher.Utils.Logger.Logger_Message_Packet;
      Logger_Packet.Log_Message (NOTICE, "DNSCatcher starting up ...");
      DNSCatcher.Utils.Logger.Logger_Queue.Add_Packet (Logger_Packet);

      -- Socket has to be shared between receiver and sender. This likely needs
      -- to move to to a higher level class
      begin
         Create_Socket
           (Socket => Socket,
            Mode   => Socket_Datagram);
         Set_Socket_Option
           (Socket => Socket,
            Level  => IP_Protocol_For_UDP_Level,
            Option => (GNAT.Sockets.Receive_Timeout, Timeout => 1.0));
         Bind_Socket
           (Socket  => Socket,
            Address =>
              (Family => Family_Inet, Addr => Any_Inet_Addr,
               Port   => Capture_Config.Local_Listen_Port));
      exception
         when Exp_Error : GNAT.Sockets.Socket_Error =>
            begin
               Logger_Packet := new DNSCatcher.Utils.Logger.Logger_Message_Packet;
               Logger_Packet.Log_Message
                 (ERROR, "Socket error: " & Exception_Information (Exp_Error));
               Logger_Packet.Log_Message (ERROR, "Refusing to start!");
               Logger_Queue.Add_Packet (Logger_Packet);
               goto Shutdown_Procedure;
            end;
      end;

      Receiver_Interface.Initialize
        (Capture_Config, Transaction_Manager_Ptr, Socket);
      Sender_Interface.Initialize (Capture_Config, Socket);

      Transaction_Manager_Ptr.Set_Packet_Queue
        (Sender_Interface.Get_Packet_Queue_Ptr);
      Transaction_Manager_Ptr.Start;
      Receiver_Interface.Start;
      Sender_Interface.Start;

      loop
         if Shutting_Down
         then
            goto Shutdown_Procedure;
         else
            delay 1.0;
         end if;
      end loop;

      <<Shutdown_Procedure>>
      Sender_Interface.Shutdown;
      Receiver_Interface.Shutdown;
      Transaction_Manager_Ptr.Stop;
      Logger_Task.Stop;
      Free_Transaction_Manager (Transaction_Manager_Ptr);
      Free_DNSCatacher_Config (Capture_Config);

   end Run_Catcher;

   -- And shutdown
   procedure Stop_Catcher is
   begin
      Shutting_Down := True;
   end Stop_Catcher;

end Packet_Catcher;
