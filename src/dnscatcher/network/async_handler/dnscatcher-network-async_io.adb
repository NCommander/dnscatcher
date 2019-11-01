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

with Ada.Text_IO; use ada.Text_IO;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;

with DNSCatcher.DNS.Transaction_Manager;
use DNSCatcher.DNS.Transaction_Manager;

with DNSCatcher.Datasets;     use DNSCatcher.Datasets;
with DNSCatcher.Utils.Logger; use DNSCatcher.Utils.Logger;

package body DNSCatcher.Network.ASync_IO is
   DNS_Transaction_Manager : DNS_Transaction_Manager_Task;
   Outbound_Packet_Queue   : DNS_Raw_Packet_Queue_Ptr;

   -- Notification procedure to tell libuv there's outbound data to be
   -- processed
   procedure Async_Notify is
      procedure UV_Async_Notify;
      pragma Import (C, UV_Async_Notify, "raise_pending_data");
   begin
      UV_Async_Notify;
   end Async_Notify;

   -- Async_IO task body, starts the libuv event loop
   task body Async_IO_Task is
   begin
      Outbound_Packet_Queue := new DNSCatcher.Datasets.Raw_Packet_Record_Queue;
      Outbound_Packet_Queue.Set_Callback_Function (Async_Notify'Access);

      DNS_Transaction_Manager.Set_Packet_Queue (Outbound_Packet_Queue);
      DNS_Transaction_Manager.Start;
      Start_UV_Event_Loop;
      DNS_Transaction_Manager.Stop;

   exception
      when Error : others =>
         begin
            Put (Standard_Error, "FATAL: Unknown error: ");
            Put_Line (Standard_Error, Exception_Information (Error));
         end;

   end Async_IO_Task;

   -- C handler to start libuv main loop
   procedure Start_UV_Event_Loop is
      procedure Internal;
      pragma Import (C, Internal, "dnscatcher_async_event_loop");
   begin
      Internal;
   end Start_UV_Event_Loop;

   procedure Handle_Inbound_Packet
     (Packet         : System.Address;
      C_Length       : Interfaces.C.size_t;
      Origin_Address : Interfaces.C.char_array;
      Origin_Port    : Interfaces.C.int)
   is
      Upstream_DNS_Server      : Unbounded_String;
      Upstream_DNS_Server_Port : Port_Type;

      Logger_Packet    : Logger_Message_Packet_Ptr;
      Incoming_Address : constant Unbounded_String :=
        To_Unbounded_String (Interfaces.C.To_Ada
             (Origin_Address,
              Trim_Nul => True));
      Incoming_Port : constant Port_Type := Port_Type (Origin_Port);
      Buffer        : Stream_Element_Array (1 .. Stream_Element_Offset (1500));
      Length        : constant Stream_Element_Offset :=
        Stream_Element_Offset (C_Length);
      DNS_Packet : Raw_Packet_Record_Ptr;

      type SEA_Pointer is access all Stream_Element_Array (1 .. Length);

      function As_SEA_Pointer is new Ada.Unchecked_Conversion (System.Address,
         SEA_Pointer);

   begin
      Upstream_DNS_Server      := To_Unbounded_String ("8.8.8.8");
      Upstream_DNS_Server_Port := 53;

      Logger_Packet := new Logger_Message_Packet;
      Logger_Packet.Push_Component ("UDP Receiver");
      Buffer (1 .. Length) := As_SEA_Pointer (Packet).all (1 .. Length);
      Logger_Packet.Log_Message
        (INFO,
         ("Received UDP Packet From " & To_String (Incoming_Address) & ":" &
          Incoming_Port'Image));

      Logger_Packet.Log_Message
        (DEBUG, ("Read " & Stream_Element_Offset'Image (Length) & " bytes"));

      -- Copy the packet for further processing
      DNS_Packet              := new Raw_Packet_Record;
      DNS_Packet.From_Address := Incoming_Address;
      DNS_Packet.From_Port    := Incoming_Port;
      DNS_Packet.To_Address   := Upstream_DNS_Server;
      DNS_Packet.To_Port      := Upstream_DNS_Server_Port;

      -- Create a new buffer of the right length and copy the data in See
      -- comment on the Header Length BS in raw_dns_packets.ads ...
      DNS_Packet.Raw_Data.Header := SEA_To_DNS_Packet_Header (Buffer
             (Buffer'First .. DNS_PACKET_HEADER_SIZE));
      DNS_Packet.Raw_Data.Data :=
        new Stream_Element_Array (1 .. Length - DNS_PACKET_HEADER_SIZE);
      DNS_Packet.Raw_Data.Data.all := Buffer
          (DNS_PACKET_HEADER_SIZE + 1 .. Length);
      DNS_Packet.Raw_Data_Length := Length;

      -- Was this a server response, or client response?
      if Upstream_DNS_Server /= Incoming_Address
      then
         DNS_Transaction_Manager.From_Client_Resolver_Packet
           (Packet => DNS_Packet,
            Local  => False);
      else
         DNS_Transaction_Manager.From_Upstream_Resolver_Packet
           (Packet => DNS_Packet);
      end if;

      Logger_Queue.Add_Packet (Logger_Packet);
   end Handle_Inbound_Packet;

   function Spool_Packets_To_UV return Raw_Packet_Record_C_Ptr is
      Packet_Record : Raw_Packet_Record_Ptr;
   begin
      Outbound_Packet_Queue.Get(Packet_Record);

      -- If the packet queue is empty, return a NULL pointer
      if Packet_Record = null then
         return null;
      end if;

      -- Otherwise, convert the packet to something C can handle.
      Put_Line("Spooled packet");
      return To_C(Packet_Record);
   end Spool_Packets_To_UV;
end DNSCatcher.Network.ASync_IO;
