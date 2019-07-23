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

with GNAT.Sockets;            use GNAT.Sockets;
with DNSCatcher.Utils.Logger; use DNSCatcher.Utils.Logger;

-- @summary
-- Configuration information used by the DNSCatcher library
--
-- @description
-- The DNSCatcher library (and daemons) require configuration to know external
-- resources such as validation resolvers, database storage information, and
-- logger information.
--
-- This information can be stored as a configuration file, loaded from the
-- database (to be implemented), or programmatically stored. It is intended
-- for configuration information to be dynamically updatable (either through
-- runtime commands or sending SIGHUP to the Catcher daemon). As such
-- configuration information will be migrated to being a protected object
-- with standard setters and getters to prevent race conditions.
--
package DNSCatcher.Config is
   -- Configuration record information
   --
   -- @field Local_Listen_Port
   -- Port currently used to determine where DNS requests are listened for when
   -- accepting legacy DNS traffic. Defaults to 53.
   --
   -- @field Upstream_DNS_Server
   -- Currently used to denote servers requests are forwarded to. At the moment
   -- only one server is supported at a time
   --
   -- @field Upstream_DNS_Server_Port Port to communicate with the server with.
   -- Defaults to 53.
   --
   -- @field Logger_Config
   -- Global configuration of the Logger object
   type Configuration is record
      Local_Listen_Port        : Port_Type;
      Upstream_DNS_Server      : Unbounded_String;
      Upstream_DNS_Server_Port : Port_Type;
      Logger_Config            : Logger_Configuration;
   end record;
   type Configuration_Ptr is access Configuration;

   -- Functions

   -- Initializes the Configuration parser by loading methods to handle data
   -- values and storage. Must be called before calling Parse_Config_File
   procedure Initialize_Config_Parse;

   -- Handles parsing the configuration file
   --
   -- It is the caller's responsibility to deallocate the pointer after all
   -- DNSCatcher services have shutdown.
   --
   -- @value Config_File_Path
   -- Path to the configuration file
   --
   -- @exception Malformed_Line
   -- Raised when a line is malformed and can't be properly parsed
   --
   -- @exception Missing_Mandatory_Config_Option Raised when a mandatory option
   -- in the config file is not present
   --
   function Parse_Config_File
     (Config_File_Path : String)
      return Configuration_Ptr;

   -- Defined Exceptions
   Malformed_Line                  : exception;
   Missing_Mandatory_Config_Option : exception;
end DNSCatcher.Config;
