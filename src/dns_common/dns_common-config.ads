with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets;          use GNAT.Sockets;
with DNS_Common.Logger;     use DNS_Common.Logger;

package DNS_Common.Config is
   type Configuration is record
      Local_Listen_Port : Port_Type;
      -- Port we'll listen on

      Upstream_DNS_Server : Unbounded_String;
      -- Upstream DNS server queries will be forwarded to
      Upstream_DNS_Server_Port : Port_Type;
      -- Port used by the upstream server

      Logger_Config : Logger_Configuration;
      -- Configuration used by the logger
   end record;
   type Configuration_Ptr is access Configuration;
end DNS_Common.Config;
