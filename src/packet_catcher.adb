with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Sockets; use GNAT.Sockets;

with Ada.Task_Identification; use Ada.Task_Identification;

with Ada.Streams; use Ada.Streams;

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;

with Ada.Unchecked_Conversion;

with Ada.Containers; use Ada.Containers;

with Ada.Unchecked_Deallocation;

with DNSCatcher_Config;
with DNS_Network_Receiver_Interface;
with DNS_Network_Sender_Interface;
with DNS_Receiver_Interface_IPv4_UDP;
with DNS_Sender_Interface_IPv4_UDP;
with DNS_Transaction_Manager; use DNS_Transaction_Manager;

package body Packet_Catcher is
   Shutting_Down : Boolean := False;

   procedure Run_Catcher is
      -- Input and Output Sockets
      DNS_Transactions        : DNS_Transaction_Manager.DNS_Transaction_Manager_Task;
      Capture_Config          : DNSCatcher_Config.Configuration_Ptr;
      Receiver_Interface      : DNS_Receiver_Interface_IPv4_UDP.IPv4_UDP_Receiver_Interface;
      Sender_Interface        : DNS_Sender_Interface_IPv4_UDP.IPv4_UDP_Sender_Interface;
      Transaction_Manager_Ptr : DNS_Transaction_Manager_Task_Ptr;
      Socket                  : Socket_Type;

      procedure Free_Transaction_Manager is new Ada.Unchecked_Deallocation
        (Object => DNS_Transaction_Manager_Task,
         Name   => DNS_Transaction_Manager_Task_Ptr);
      procedure Free_DNSCatacher_Config is new Ada.Unchecked_Deallocation
        (Object => DNSCatcher_Config.Configuration,
         Name => DNSCatcher_Config.Configuration_Ptr);
   begin
      Capture_Config                          := new DNSCatcher_Config.Configuration;
      Capture_Config.Local_Listen_Port        := 53;
      Capture_Config.Upstream_DNS_Server      := To_Unbounded_String ("4.2.2.2");
      Capture_Config.Upstream_DNS_Server_Port := 53;

      Transaction_Manager_Ptr := new DNS_Transaction_Manager_Task;

      -- Socket has to be shared between receiver and sender. This likely needs to move to
      -- to a higher level class
      Create_Socket (Socket => Socket, Mode => Socket_Datagram);
      Set_Socket_Option
        (Socket => Socket, Option => (GNAT.Sockets.Receive_Timeout, Timeout => 1.0));
      Bind_Socket
        (Socket  => Socket,
         Address =>
           (Family => Family_Inet, Addr => Any_Inet_Addr,
            Port   => Capture_Config.Local_Listen_Port));

      Receiver_Interface.Initialize (Capture_Config, Transaction_Manager_Ptr, Socket);
      Sender_Interface.Initialize (Capture_Config, Transaction_Manager_Ptr, Socket);

      -- Connect the packet queue and start it all up
      Transaction_Manager_Ptr.Set_Packet_Queue (Sender_Interface.Get_Packet_Queue_Ptr);
      Transaction_Manager_Ptr.Start;
      Receiver_Interface.Start;
      Sender_Interface.Start;

      loop
         if Shutting_Down = True then
            Put_Line ("Starting shutdown");
            Receiver_Interface.Shutdown;
            Sender_Interface.Shutdown;
            Transaction_Manager_Ptr.Stop;
            Free_Transaction_Manager(Transaction_Manager_Ptr);
            Free_DNSCatacher_Config(Capture_Config);
            return;
         else
            delay 1.0;
         end if;
      end loop;

   end Run_Catcher;

   -- And shutdown
   procedure Stop_Catcher is
   begin
      Shutting_Down := True;
   end Stop_Catcher;

end Packet_Catcher;
