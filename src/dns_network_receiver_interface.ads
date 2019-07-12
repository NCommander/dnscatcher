with GNAT.Sockets; use GNAT.Sockets;

with DNS_Common.Config;       use DNS_Common.Config;
with DNS_Transaction_Manager; use DNS_Transaction_Manager;

package DNS_Network_Receiver_Interface is
   type Receiver_Interface is abstract tagged null record;

   procedure Initialize
     (This                : in out Receiver_Interface;
      Config              :        Configuration_Ptr;
      Transaction_Manager :        DNS_Transaction_Manager_Task_Ptr;
      Socket              :        Socket_Type) is abstract;
      -- Initializes a network interface and does any necessary prep work. It
      -- MUST be called before calling any other method

   procedure Start (This : in out Receiver_Interface) is abstract;
   -- Starts the interface

   procedure Shutdown (This : in out Receiver_Interface) is abstract;
   -- Cleanly shuts down the interface

end DNS_Network_Receiver_Interface;
