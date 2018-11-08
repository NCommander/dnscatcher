with Ada.Text_IO; use Ada.Text_IO;

with Ada.Unchecked_Conversion;

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets;          use GNAT.Sockets;

with DNS_Common.Config;
with DNS_Common.Logger;       use DNS_Common.Logger;
with DNS_Receiver_Interface_IPv4_UDP;
with DNS_Sender_Interface_IPv4_UDP;
with DNS_Transaction_Manager; use DNS_Transaction_Manager;

package body Packet_Catcher is
   Shutting_Down : Boolean := False;

   procedure Run_Catcher is
      -- Input and Output Sockets
      DNS_Transactions        : DNS_Transaction_Manager.DNS_Transaction_Manager_Task;
      Capture_Config          : DNS_Common.Config.Configuration_Ptr;
      Receiver_Interface      : DNS_Receiver_Interface_IPv4_UDP.IPv4_UDP_Receiver_Interface;
      Sender_Interface        : DNS_Sender_Interface_IPv4_UDP.IPv4_UDP_Sender_Interface;
      Transaction_Manager_Ptr : DNS_Transaction_Manager_Task_Ptr;
      Socket                  : Socket_Type;

      procedure Free_Transaction_Manager is new Ada.Unchecked_Deallocation
        (Object => DNS_Transaction_Manager_Task, Name => DNS_Transaction_Manager_Task_Ptr);
      procedure Free_DNSCatacher_Config is new Ada.Unchecked_Deallocation
        (Object => DNS_Common.Config.Configuration, Name => DNS_Common.Config.Configuration_Ptr);
   begin
      Capture_Config                          := new DNS_Common.Config.Configuration;
      Capture_Config.Local_Listen_Port        := 5553;
      Capture_Config.Upstream_DNS_Server      := To_Unbounded_String ("4.2.2.2");
      Capture_Config.Upstream_DNS_Server_Port := 53;

      -- Configure the logger
      Capture_Config.Logger_Config.Log_Level := DEBUG;
      Capture_Config.Logger_Config.Use_Color := True;

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
      Sender_Interface.Initialize (Capture_Config, Socket);

      -- Connect the packet queue and start it all up
      Transaction_Manager_Ptr.Set_Packet_Queue (Sender_Interface.Get_Packet_Queue_Ptr);
      Transaction_Manager_Ptr.Start;
      Receiver_Interface.Start;
      Sender_Interface.Start;

      loop
         if Shutting_Down
         then
            Put_Line ("Starting shutdown");
            Sender_Interface.Shutdown;
            Receiver_Interface.Shutdown;
            Transaction_Manager_Ptr.Stop;
            Free_Transaction_Manager (Transaction_Manager_Ptr);
            Free_DNSCatacher_Config (Capture_Config);
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
