with GNAT.Sockets;                           use GNAT.Sockets;
with DNS_Common.Config;                      use DNS_Common.Config;
with DNS_Network_Sender_Interface;
with DNS_Core_Constructs.Raw_Packet_Records; use DNS_Core_Constructs.Raw_Packet_Records;

package DNS_Sender_Interface_IPv4_UDP is
   -- Tasks Definition
   task type Send_Packet_Task is
      entry Initialize (Socket : Socket_Type; Packet_Queue : DNS_Raw_Packet_Queue_Ptr);
      entry Start;
      entry Stop;
   end Send_Packet_Task;
   type Send_Packet_Task_Ptr is access Send_Packet_Task;

   type IPv4_UDP_Sender_Interface is new DNS_Network_Sender_Interface.Sender_Interface with record
      Config        : Configuration_Ptr;
      Sender_Socket : Socket_Type;
      Sender_Task   : Send_Packet_Task_Ptr;
      Packet_Queue  : DNS_Raw_Packet_Queue_Ptr;
   end record;
   type IPv4_UDP_Receiver_Interface_Ptr is access IPv4_UDP_Sender_Interface;

   procedure Initialize (This : in out IPv4_UDP_Sender_Interface; Config : Configuration_Ptr;
      Socket                  :        Socket_Type);
   -- Initializes a network interface and does any necessary prep work. It MUST be called before
   -- calling any other method

   procedure Start (This : in out IPv4_UDP_Sender_Interface);
   -- Starts the interface

   procedure Shutdown (This : in out IPv4_UDP_Sender_Interface);
   -- Cleanly shuts down the interface

   function Get_Packet_Queue_Ptr
     (This : in out IPv4_UDP_Sender_Interface) return DNS_Raw_Packet_Queue_Ptr;
   -- Returns a pointer to the packet queue

end DNS_Sender_Interface_IPv4_UDP;
