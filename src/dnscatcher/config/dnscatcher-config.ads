-- Copyright 2019 Michael Casadevall <michael@casadevall.pro>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;

with GNAT.Sockets;      use GNAT.Sockets;
with DNS_Common.Logger; use DNS_Common.Logger;

package DNSCatcher.Config is
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
     (Config_File_Path : String)
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
end DNSCatcher.Config;
