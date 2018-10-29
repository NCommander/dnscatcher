with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets;          use GNAT.Sockets;

package DNSCatcher_Config is
   type Configuration is record
      Local_Listen_Port : Port_Type;
      -- Port we'll listen on

      Upstream_DNS_Server : Unbounded_String;
      -- Upstream DNS server queries will be forwarded to
      Upstream_DNS_Server_Port : Port_Type;
      -- Port used by the upstream server
   end record;
   type Configuration_Ptr is access Configuration;
end DNSCatcher_Config;
