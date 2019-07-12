with GNAT.Sockets;            use GNAT.Sockets;
with DNS_Common.Config;       use DNS_Common.Config;
with DNS_Network_Receiver_Interface;
with DNS_Transaction_Manager; use DNS_Transaction_Manager;

package DNS_Receiver_Interface_IPv4_UDP is
   -- Tasks Definition
   task type Receive_Packet_Task is
      entry Initialize
        (Config              : Configuration_Ptr;
         Socket              : Socket_Type;
         Transaction_Manager : DNS_Transaction_Manager_Task_Ptr);
      entry Start;
      entry Stop;
   end Receive_Packet_Task;
   type Receive_Packet_Task_Ptr is access Receive_Packet_Task;

   type IPv4_UDP_Receiver_Interface is new DNS_Network_Receiver_Interface
     .Receiver_Interface with
   record
      Config              : Configuration_Ptr;
      Receiver_Socket     : Socket_Type;
      Transaction_Manager : DNS_Transaction_Manager_Task_Ptr;
      Receiver_Task       : Receive_Packet_Task_Ptr;
   end record;
   type IPv4_UDP_Receiver_Interface_Ptr is access IPv4_UDP_Receiver_Interface;

   procedure Initialize
     (This                : in out IPv4_UDP_Receiver_Interface;
      Config              :        Configuration_Ptr;
      Transaction_Manager :        DNS_Transaction_Manager_Task_Ptr;
      Socket              :        Socket_Type);
      -- Initializes a network interface and does any necessary prep work. It
      -- MUST be called before calling any other method

   procedure Start (This : in out IPv4_UDP_Receiver_Interface);
   -- Starts the interface

   procedure Shutdown (This : in out IPv4_UDP_Receiver_Interface);
   -- Cleanly shuts down the interface
end DNS_Receiver_Interface_IPv4_UDP;
