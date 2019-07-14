with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;

with GNAT.Sockets;      use GNAT.Sockets;
with DNS_Common.Logger; use DNS_Common.Logger;

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

   -- Functions
   procedure Initialize_Config_Parse;
   function Parse_Config_File
     (Config_File_Path : Unbounded_String)
      return Configuration_Ptr;

   type Parse_Procedure is access procedure
     (Config    : Configuration_Ptr;
      Value_Str : String);
   procedure Parse_Local_Listen_Port
     (Config    : Configuration_Ptr;
      Value_Str : String);
   procedure Parse_Upstream_DNS_Server
     (Config    : Configuration_Ptr;
      Value_Str : String);
   procedure Parse_Upstream_DNS_Server_Port
     (Config    : Configuration_Ptr;
      Value_Str : String);

      -- Type for handling configuration variables GCP = Global Config
      -- Paramters
   package GCP_Management is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Parse_Procedure);

   GCP_Map : GCP_Management.Map;

   -- Defined Exceptions
   Malformed_Line : exception;
   Missing_Mandatory_Config_Option : exception;
end DNS_Common.Config;
