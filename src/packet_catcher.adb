with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Sockets; use GNAT.Sockets;

with Ada.Task_Identification; use Ada.Task_Identification;

with Ada.Streams; use Ada.Streams;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Unchecked_Conversion;

with Raw_Dns_Packets;

package body Packet_Catcher is
   -- Input and Output Sockets
   DNS_Socket           : Socket_Type;
   To_Resolver          : Inet_Addr_Type := Inet_Addr ("4.2.2.2");
   Inbound_Packet_Queue : Raw_DNS_Packet_Queue;

   -- Tasks Definition
   task Receive_Packet is
      entry Start;
   end Receive_Packet;

   task Send_Packet is
      entry Start;
      entry Process_Queue;
   end Send_Packet;

   -- Handles DNS Packets in a FIFO queue; built around Vectors, this may
   -- need to be changed for performance reasons at some point
   protected body Raw_DNS_Packet_Queue is
      entry Put (Packet : in Raw_DNS_Packet) when True is
      begin
         Stored_Packets.Append (Packet);
      end Put;

      entry Get (Packet : out Raw_DNS_Packet) when True is
      begin
         Packet := Stored_Packets.First_Element;
         Stored_Packets.Delete_First;
      end Get;

      function Count return Integer is
      begin
         return Integer (Stored_Packets.Length);
      end Count;

   end Raw_DNS_Packet_Queue;

   function SEA_To_Dns_Packet_Header is new
     Ada.Unchecked_Conversion(Source => Stream_Element_Array,
                              Target => Raw_Dns_Packets.Dns_Packet_Header);

   task body Receive_Packet is

      Buffer           : access Stream_Element_Array;
      Offset           : Stream_Element_Offset;
      Incoming_Address : Sock_Addr_Type;
      DNS_Packet       : Raw_DNS_Packet;
      Packet_Header    : Raw_DNS_Packets.DNS_Packet_Header;
   begin
      accept Start;

      Buffer := new Stream_Element_Array (1 .. Stream_Element_Offset (1500));
      Ada.Text_IO.Put_Line ("Test Receive Packet");
      loop -- Main receiving loop
         Receive_Packet_Loop :
         loop -- Receive the packet
            Receive_Socket
              (Socket => DNS_Socket, Item => Buffer.all, Last => Offset,
               From   => Incoming_Address);
            exit when Buffer'Last = -1;

            Put ("Received UDP Packet From ");
            Put_Line (Image (Incoming_Address.Addr));
            Put_Line
              ("Read " & Stream_Element_Offset'Image (Offset) & " bytes");

            -- See if we can successfully decode some information from
            -- the packet
            Packet_Header := SEA_To_Dns_Packet_Header(Buffer.all);
            Put("  DNS Transaction ID: ");
            Put(Standard_Output, Integer(Packet_Header.Identifier), Base => 16);
            New_Line;


            -- Copy the packet for further processing
            DNS_Packet.From_Address :=
              To_Unbounded_String (Image (Incoming_Address.Addr));
            DNS_Packet.To_Address      := To_Unbounded_String ("4.4.2.2");
            DNS_Packet.Raw_Data        := Buffer;
            DNS_Packet.Raw_Data_Length := Offset;
            Inbound_Packet_Queue.Put (Packet => DNS_Packet);

            -- Tell Send Packet to go do stuff
            Send_Packet.Process_Queue;
         end loop Receive_Packet_Loop;
      end loop;
   exception
      when Error : others =>
         begin
            Put (Standard_Error, "Unknown error: ");
            Put_Line (Standard_Error, Exception_Information (Error));
            Packet_Catcher.Stop_Catcher;
         end;

         -- Test
   end Receive_Packet;

   task body Send_Packet is
      DNS_Packet       : Raw_DNS_Packet;
      Outgoing_Address : Inet_Addr_Type;
      Length           : Stream_Element_Offset;

   begin
      accept Start;
      Put_Line ("Send packet task started");
      -- Safely convert buffer to string element

      loop
         select
            accept Process_Queue do
               Inbound_Packet_Queue.Get (DNS_Packet);
               Outgoing_Address :=
                 Inet_Addr (To_String (DNS_Packet.To_Address));

               -- And send the damn thing
               Send_Socket
                 (Socket => DNS_Socket,
                  Item   =>
                    DNS_Packet.Raw_Data.all (1 .. DNS_Packet.Raw_Data_Length),
                  Last => Length,
                  To   =>
                    (Family => Family_Inet, Addr => Outgoing_Address,
                     Port   => 53));
               Put ("Sent packet to ");
               Put_Line (Image (Outgoing_Address));
            end Process_Queue;
         or
            delay 0.1;
         end select;
      end loop;
   exception
      when Error : others =>
         begin
            Put (Standard_Error, "Unknown error: ");
            Put_Line (Standard_Error, Exception_Information (Error));
            Packet_Catcher.Stop_Catcher;
         end;
   end Send_Packet;

   procedure Run_Catcher is
   begin
      -- Setup the inbound port 53 socket
      GNAT.Sockets.Initialize;
      Create_Socket (Socket => DNS_Socket, Mode => Socket_Datagram);
      Bind_Socket
        (Socket  => DNS_Socket,
         Address =>
           (Family => Family_Inet, Addr => Any_Inet_Addr, Port => 53));

      -- Start our processing loops
      Receive_Packet.Start;
      Send_Packet.Start;
   end Run_Catcher;

   -- And shutdown
   procedure Stop_Catcher is
   begin
      Abort_Task (Receive_Packet'Identity);
      Abort_Task (Send_Packet'Identity);
   end Stop_Catcher;

end Packet_Catcher;
