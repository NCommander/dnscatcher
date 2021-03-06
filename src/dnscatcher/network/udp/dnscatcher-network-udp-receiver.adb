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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with DNSCatcher.Types;        use DNSCatcher.Types;
with DNSCatcher.Utils.Logger; use DNSCatcher.Utils.Logger;

package body DNSCatcher.Network.UDP.Receiver is
   task body Receive_Packet_Task is
      --DNS_Config              : Configuration;
      -- Local config variables
      Upstream_DNS_Server      : Unbounded_String;
      Upstream_DNS_Server_Port : Port_Type;

      DNS_Socket              : Socket_Type;
      DNS_Transaction_Manager : DNS_Transaction_Manager_Task_Ptr;
      Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (1500));
      Offset                  : Stream_Element_Offset;
      Incoming_Address        : Sock_Addr_Type;
      DNS_Packet              : Raw_Packet_Record_Ptr;
      Process_Packets         : Boolean := False;
      Logger_Packet           : Logger_Message_Packet_Ptr;
   begin
      loop
         Logger_Packet := new Logger_Message_Packet;
         Logger_Packet.Push_Component ("UDP Receiver");

         -- Either just started or stopping, we're terminatable in this state
         while Process_Packets = False
         loop
            select
               accept Initialize
                 (Config              : Configuration;
                  Socket              : Socket_Type;
                  Transaction_Manager : DNS_Transaction_Manager_Task_Ptr) do
                  Upstream_DNS_Server      := Config.Upstream_DNS_Server;
                  Upstream_DNS_Server_Port := Config.Upstream_DNS_Server_Port;

                  DNS_Socket              := Socket;
                  DNS_Transaction_Manager := Transaction_Manager;
               end Initialize;
            or
               accept Start do
                  Process_Packets := True;
                  Logger_Packet.Log_Message (NOTICE, "Receiver Startup");
                  Logger_Queue.Add_Packet
                    (Logger_Packet); -- We do this because state change
               end Start;
            or
               accept Stop do
                  Logger_Packet.Log_Message (NOTICE, "Receiver Stop");
                  Logger_Queue.Add_Packet
                    (Logger_Packet); -- We do this because state change
                  null;
               end Stop;
            or
               terminate;
            end select;
         end loop;

         while Process_Packets
         loop -- Main receiving loop
            Logger_Packet := new Logger_Message_Packet;
            Logger_Packet.Push_Component ("UDP Receiver");

            select
               accept Start do
                  null;
               end Start;
            or
               accept Stop do
                  Process_Packets := False;
               end Stop;
            else
               begin
                  Receive_Socket
                    (Socket => DNS_Socket,
                     Item   => Buffer,
                     Last   => Offset,
                     From   => Incoming_Address);

                  Logger_Packet.Log_Message
                    (INFO,
                     ("Received UDP Packet From " &
                      Image (Incoming_Address.Addr) & ":" &
                      Port_Type'Image (Incoming_Address.Port)));

                  Logger_Packet.Log_Message
                    (DEBUG,
                     ("Read " & Stream_Element_Offset'Image (Offset) &
                      " bytes"));

                  -- Copy the packet for further processing
                  DNS_Packet              := new Raw_Packet_Record;
                  DNS_Packet.From_Address :=
                    To_Unbounded_String (Image (Incoming_Address.Addr));
                  DNS_Packet.From_Port  := Incoming_Address.Port;
                  DNS_Packet.To_Address := Upstream_DNS_Server;
                  DNS_Packet.To_Port    := Upstream_DNS_Server_Port;

                  -- Create a new buffer of the right length and copy
                  -- the data in See comment on the Header Length BS
                  -- in raw_dns_packets.ads ...
                  DNS_Packet.Raw_Data.Header :=
                    SEA_To_DNS_Packet_Header (Buffer
                         (Buffer'First .. DNS_PACKET_HEADER_SIZE));
                  DNS_Packet.Raw_Data.Data :=
                    new Stream_Element_Array
                      (1 .. Offset - DNS_PACKET_HEADER_SIZE);
                  DNS_Packet.Raw_Data.Data.all := Buffer
                      (DNS_PACKET_HEADER_SIZE + 1 .. Offset);
                  DNS_Packet.Raw_Data_Length := Offset;

                  -- Was this a server response, or client response?
                  if Upstream_DNS_Server /= (Image (Incoming_Address.Addr))
                  then
                     DNS_Transaction_Manager.From_Client_Resolver_Packet
                       (Packet => DNS_Packet,
                        Local  => False);
                  else
                     DNS_Transaction_Manager.From_Upstream_Resolver_Packet
                       (Packet => DNS_Packet);
                  end if;

               exception
                  -- Socket Errors will happen due to time out all the time, we
                  -- just need to restart and recover
                  when GNAT.Sockets.Socket_Error =>
                     begin
                        -- This isn't the RIGHT thing to do, but its the only
                        -- way to handle Recv Timeouts because it doesn't use
                        -- a unique exception. We'll hope the right exception
                        -- pops up on write cause otherwise we're screwed
                        null;
                     end;
               end;
            end select;

            Logger_Queue.Add_Packet (Logger_Packet);
         end loop;
      end loop;
   exception
      when Exp_Error : others =>
         begin
            Logger_Packet.Log_Message
              (ERROR, "Unknown error: " & Exception_Information (Exp_Error));
            --Packet_Catcher.Stop_Catcher;
         end;
   end Receive_Packet_Task;

   procedure Initialize
     (This                : in out UDP_Receiver_Interface;
      Config              :        Configuration;
      Transaction_Manager :        DNS_Transaction_Manager_Task_Ptr;
      Socket              :        Socket_Type)
   is
   begin
      -- Save our config for good measure
      This.Receiver_Socket     := Socket;
      This.Transaction_Manager := Transaction_Manager;
      This.Receiver_Task       := new Receive_Packet_Task;
      This.Receiver_Task.Initialize
        (Config, This.Receiver_Socket, This.Transaction_Manager);
   end Initialize;

   procedure Start (This : in out UDP_Receiver_Interface) is
   begin
      This.Receiver_Task.Start;
   end Start;

   procedure Shutdown (This : in out UDP_Receiver_Interface) is
   begin
      -- Cleanly shuts down the interface
      if This.Receiver_Task /= null
      then
         This.Receiver_Task.Stop;
      end if;
   end Shutdown;
end DNSCatcher.Network.UDP.Receiver;
