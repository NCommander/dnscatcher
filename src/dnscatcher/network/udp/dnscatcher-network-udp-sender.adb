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

with Ada.Exceptions; use Ada.Exceptions;

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DNS_Core_Constructs;   use DNS_Core_Constructs;
with DNSCatcher.Utils.Logger;     use DNSCatcher.Utils.Logger;

package body DNSCatcher.Network.UDP.Sender is

   task body Send_Packet_Task is
      DNS_Socket            : Socket_Type;
      DNS_Packet            : Raw_Packet_Record;
      Outbound_Packet_Queue : DNS_Raw_Packet_Queue_Ptr := null;
      Outgoing_Address      : Inet_Addr_Type;
      Outgoing_Port         : Port_Type;
      Length                : Stream_Element_Offset;
      Process_Packets       : Boolean                  := False;
      Packet_Count          : Integer                  := 0;
      Logger_Packet         : Logger_Message_Packet_Ptr;
   begin
      loop
         -- Either just started or stopping, we're terminatable in this state
         if Outbound_Packet_Queue /= null
         then
            Outbound_Packet_Queue.Count (Packet_Count);
         end if;

         while Process_Packets = False and Packet_Count = 0
         loop
            select
               accept Initialize
                 (Socket       : Socket_Type;
                  Packet_Queue : DNS_Raw_Packet_Queue_Ptr) do
                  DNS_Socket            := Socket;
                  Outbound_Packet_Queue := Packet_Queue;
               end Initialize;
            or
               accept Start do
                  Logger_Packet := new Logger_Message_Packet;
                  Logger_Packet.Push_Component ("UDP Sender");

                  Process_Packets := True;
                  Logger_Packet.Log_Message (INFO, "Sender startup");
                  Logger_Queue.Add_Packet (Logger_Packet);
               end Start;
            or
               accept Stop do
                  null;
               end Stop;
            or
               terminate;
            end select;
         end loop;

         -- We're actively processing packets
         while Process_Packets or Packet_Count > 0
         loop
            Logger_Packet := new Logger_Message_Packet;
            Logger_Packet.Push_Component ("UDP Sender");

            select
               accept Start do
                  null;
               end Start;
            or
               accept Stop do
                  Process_Packets := False;
               end Stop;
            else
               Outbound_Packet_Queue.Count (Packet_Count);
               if Packet_Count > 0
               then
                  Outbound_Packet_Queue.Get (DNS_Packet);
                  declare
                     Buffer : Stream_Element_Array
                       (1 .. DNS_Packet.Raw_Data_Length);
                     Header : SEA_DNS_Packet_Header;
                  begin
                     Outgoing_Address :=
                       Inet_Addr (To_String (DNS_Packet.To_Address));
                     Outgoing_Port := DNS_Packet.To_Port;

                     -- And send the damn thing
                     Logger_Packet.Log_Message
                       (DEBUG, "Sent packet to " & Image (Outgoing_Address));

                     -- Create the outbound message
                     Header :=
                       DNS_Packet_Header_To_SEA (DNS_Packet.Raw_Data.Header);
                     Buffer := Header & DNS_Packet.Raw_Data.Data.all;
                     Send_Socket
                       (Socket => DNS_Socket,
                        Item   => Buffer,
                        Last   => Length,
                        To     =>
                          (Family => Family_Inet, Addr => Outgoing_Address,
                           Port   => Outgoing_Port));

                  exception
                     when Exp_Error : others =>
                        begin
                           Logger_Packet.Log_Message
                             (ERROR,
                              "Unknown error: " &
                              Exception_Information (Exp_Error));
                        end;
                  end;
               else
                  delay 0.1;
               end if;
            end select;

            -- Send the logs on their way
            Logger_Queue.Add_Packet (Logger_Packet);
         end loop;
      end loop;
   end Send_Packet_Task;

   procedure Initialize
     (This   : in out UDP_Sender_Interface;
      Config :        Configuration_Ptr;
      Socket :        Socket_Type)
   is
   begin
      -- Save our config for good measure
      This.Config := Config;

      This.Sender_Socket := Socket;
      This.Sender_Task   := new Send_Packet_Task;
      This.Packet_Queue  := new Raw_Packet_Record_Queue;

      This.Sender_Task.Initialize (This.Sender_Socket, This.Packet_Queue);
   end Initialize;

   procedure Start (This : in out UDP_Sender_Interface) is
   begin
      This.Sender_Task.Start;
   end Start;

   procedure Shutdown (This : in out UDP_Sender_Interface) is
   begin
      -- Cleanly shuts down the interface
      if This.Sender_Task /= null
      then
         This.Sender_Task.Stop;
      end if;
   end Shutdown;

   function Get_Packet_Queue_Ptr
     (This : in out UDP_Sender_Interface)
      return DNS_Raw_Packet_Queue_Ptr
   is
   begin
      return This.Packet_Queue;
   end Get_Packet_Queue_Ptr;

end DNSCatcher.Network.UDP.Sender;
