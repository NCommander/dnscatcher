with GNAT.Sockets; use GNAT.Sockets;

with DNSCatcher_Config; use DNSCatcher_Config;

package DNS_Network_Sender_Interface is
   type Sender_Interface is abstract tagged null record;

   procedure Initialize (This : in out Sender_Interface; Config : Configuration_Ptr;
      Socket                  :        Socket_Type) is abstract;
   -- Initializes a network interface and does any necessary prep work. It MUST be called before
   -- calling any other method

   procedure Start (This : in out Sender_Interface) is abstract;
   -- Starts the interface

   procedure Shutdown (This : in out Sender_Interface) is abstract;
   -- Cleanly shuts down the interface

end DNS_Network_Sender_Interface;
