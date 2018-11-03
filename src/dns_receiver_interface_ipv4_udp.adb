with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNAT.Sockets; use GNAT.Sockets;

with Packet_Catcher;
with Utils;

with DNSCatcher_Config; use DNSCatcher_Config;
with DNS_Network_Receiver_Interface;
with DNS_Transaction_Manager;
with DNS_Raw_Packet_Records; use DNS_Raw_Packet_Records;
with DNS_Core_Constructs; use DNS_Core_Constructs;

package body DNS_Receiver_Interface_IPv4_UDP is
   task body Receive_Packet_Task is
      DNS_Config              : Configuration_Ptr;
      DNS_Socket              : Socket_Type;
      DNS_Transaction_Manager : DNS_Transaction_Manager_Task_Ptr;
      Buffer                  : Stream_Element_Array (1 .. Stream_Element_Offset (1500));
      Offset                  : Stream_Element_Offset;
      Incoming_Address        : Sock_Addr_Type;
      DNS_Packet              : DNS_Raw_Packet_Record_Ptr;
      Process_Packets         : Boolean := False;
   begin
      accept Initialize (Config : Configuration_Ptr; Socket : Socket_Type;
         Transaction_Manager    : DNS_Transaction_Manager_Task_Ptr) do
         DNS_Config              := Config;
         DNS_Socket              := Socket;
         DNS_Transaction_Manager := Transaction_Manager;
      end Initialize;

      loop
         -- Either just started or stopping, we're terminatable in this state
         Put_Line ("Received State STOPPED");
         while Process_Packets = False loop
            select
               accept Start do
                  Process_Packets := True;
               end Start;
            or
               accept Stop do
                  null;
               end Stop;
            or
               terminate;
            end select;
         end loop;

         Put_Line ("Received State STARTED");

         while Process_Packets loop -- Main receiving loop
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
                    (Socket => DNS_Socket, Item => Buffer, Last => Offset,
                     From   => Incoming_Address);

                  Put_Line
                    ("Received UDP Packet From " & Image (Incoming_Address.Addr) & ":" &
                     Port_Type'Image (Incoming_Address.Port));

                  Put_Line ("Read " & Stream_Element_Offset'Image (Offset) & " bytes");

                  -- Copy the packet for further processing
                  DNS_Packet              := new DNS_Raw_Packet_Record;
                  DNS_Packet.From_Address := To_Unbounded_String (Image (Incoming_Address.Addr));
                  DNS_Packet.From_Port    := Incoming_Address.Port;
                  DNS_Packet.To_Address   := DNS_Config.Upstream_DNS_Server;
                  DNS_Packet.To_Port      := DNS_Config.Upstream_DNS_Server_Port;

                  -- Create a new buffer of the right length and copy the data in
                  -- See comment on the Header Length BS in raw_dns_packets.ads ...
                  DNS_Packet.Raw_Data.Header := SEA_To_DNS_Packet_Header(Buffer(Buffer'First..DNS_PACKET_HEADER_SIZE));
                  DNS_Packet.Raw_Data.Data   := new Stream_Element_Array(1..Offset-DNS_PACKET_HEADER_SIZE);
                  DNS_Packet.Raw_Data.Data.all := Buffer(DNS_PACKET_HEADER_SIZE+1..Offset);
                  DNS_Packet.Raw_Data_Length := Offset;

                  -- Was this a server response, or client response?
                  if DNS_Config.Upstream_DNS_Server /= (Image (Incoming_Address.Addr)) then
                     DNS_Transaction_Manager.From_Client_Resolver_Packet (Packet => DNS_Packet);
                  else
                     DNS_Transaction_Manager.From_Upstream_Resolver_Packet (Packet => DNS_Packet);
                  end if;

               exception

                  -- Socket Errors will happen due to time out all the time, we just need to
                  -- restart and recover
                  when Error : GNAT.Sockets.Socket_Error =>
                     begin
                        -- This isn't the RIGHT thing to do, but its the only way to handle Recv
                        -- Timeouts because it doesn't use a unique exception. We'll hope the right
                        -- exception pops up on write cause otherwise we're screwed
                        null;
                     end;
               end;

            end select;
         end loop;
      end loop;
   exception
      when Error : others =>
         begin
            Put (Standard_Error, "Unknown error: ");
            Put_Line (Standard_Error, Exception_Information (Error));
            Packet_Catcher.Stop_Catcher;
         end;
   end Receive_Packet_Task;

   procedure Initialize (This : in out IPv4_UDP_Receiver_Interface; Config : Configuration_Ptr;
      Transaction_Manager     :        DNS_Transaction_Manager_Task_Ptr; Socket : Socket_Type) is
   begin
      GNAT.Sockets.Initialize;

      -- Save our config for good measure
      This.Config              := Config;
      This.Receiver_Socket     := Socket;
      This.Transaction_Manager := Transaction_Manager;
      This.Receiver_Task       := new Receive_Packet_Task;
      This.Receiver_Task.Initialize (Config, This.Receiver_Socket, This.Transaction_Manager);
   end Initialize;

   procedure Start (This : in out IPv4_UDP_Receiver_Interface) is
   begin
      This.Receiver_Task.Start;
   end Start;

   procedure Shutdown (This : in out IPv4_UDP_Receiver_Interface) is
   begin
      -- Cleanly shuts down the interface
      This.Receiver_Task.Stop;
   end Shutdown;
end DNS_Receiver_Interface_IPv4_UDP;
