with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DNS_Core_Constructs;   use DNS_Core_Constructs;

package body DNS_Sender_Interface_IPv4_UDP is

   task body Send_Packet_Task is
      DNS_Socket            : Socket_Type;
      DNS_Packet            : Raw_Packet_Record;
      Outbound_Packet_Queue : DNS_Raw_Packet_Queue_Ptr;
      Outgoing_Address      : Inet_Addr_Type;
      Outgoing_Port         : Port_Type;
      Length                : Stream_Element_Offset;
      Process_Packets       : Boolean := False;
      Packet_Count          : Integer := 0;
   begin
      accept Initialize (Socket : Socket_Type; Packet_Queue : DNS_Raw_Packet_Queue_Ptr) do
         DNS_Socket            := Socket;
         Outbound_Packet_Queue := Packet_Queue;
      end Initialize;

      Put_Line ("Send packet task started");
      loop
         -- Either just started or stopping, we're terminatable in this state
         Put_Line ("Send State STOPPED");
         while Process_Packets = False
         loop
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

         Put_Line ("Send State STARTED");
         -- We're actively processing packets
         while Process_Packets
         loop
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
                  Outgoing_Address := Inet_Addr (To_String (DNS_Packet.To_Address));
                  Outgoing_Port    := DNS_Packet.To_Port;

                  -- And send the damn thing
                  Put ("Sent packet to ");
                  Put_Line (Image (Outgoing_Address));
                  Put_Line (Port_Type'Image (Outgoing_Port));

                  -- Create the outbound message
                  declare
                     Buffer : Stream_Element_Array (1 .. DNS_Packet.Raw_Data_Length);
                     Header : SEA_DNS_Packet_Header;
                  begin
                     Header := DNS_Packet_Header_To_SEA (DNS_Packet.Raw_Data.Header);
                     Buffer := Header & DNS_Packet.Raw_Data.Data.all;
                     Send_Socket
                       (Socket => DNS_Socket, Item => Buffer, Last => Length,
                        To     =>
                          (Family => Family_Inet, Addr => Outgoing_Address,
                           Port   => Outgoing_Port));

                     Put ("Sent packet to ");
                     Put_Line (Image (Outgoing_Address));
                  end;
               end if;
            end select;
         end loop;
      end loop;

   exception
      when Error : others =>
         begin
            Put (Standard_Error, "Unknown error: ");
            Put_Line (Standard_Error, Exception_Information (Error));
         end;
   end Send_Packet_Task;

   procedure Initialize (This : in out IPv4_UDP_Sender_Interface; Config : Configuration_Ptr;
      Socket                  :        Socket_Type)
   is
   begin
      -- Save our config for good measure
      This.Config := Config;

      This.Sender_Socket := Socket;
      This.Sender_Task   := new Send_Packet_Task;
      This.Packet_Queue  := new Raw_Packet_Record_Queue;

      This.Sender_Task.Initialize (This.Sender_Socket, This.Packet_Queue);
   end Initialize;

   procedure Start (This : in out IPv4_UDP_Sender_Interface) is
   begin
      This.Sender_Task.Start;
   end Start;

   procedure Shutdown (This : in out IPv4_UDP_Sender_Interface) is
   begin
      -- Cleanly shuts down the interface
      This.Sender_Task.Stop;
   end Shutdown;

   function Get_Packet_Queue_Ptr
     (This : in out IPv4_UDP_Sender_Interface) return DNS_Raw_Packet_Queue_Ptr
   is
   begin
      return This.Packet_Queue;
   end Get_Packet_Queue_Ptr;

end DNS_Sender_Interface_IPv4_UDP;
