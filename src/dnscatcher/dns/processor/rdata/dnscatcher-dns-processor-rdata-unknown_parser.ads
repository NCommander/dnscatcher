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

with DNSCatcher.DNS.Processor.Packet; use DNSCatcher.DNS.Processor.Packet;

-- @summary
-- RData Unknown_Parser is a catch-all for any RRTypes that do not have
-- specific handling in this package. It exists to prevent crashes and to
-- store these rrtypes in a generic form as a failsafe.
--
-- @description
-- The unknown packet parser is the last resort used by the RData package.
-- Aside from the generic Type and TTL values, the unknown handler simply
-- contains a binary representation of a given class
--
package DNSCatcher.DNS.Processor.RData.Unknown_Parser is
   -- Unknown RData Type
   type Parsed_Unknown_RData is
     new DNSCatcher.DNS.Processor.RData.Parsed_RData with private;
   type Parsed_Unknown_RData_Access is access all Parsed_Unknown_RData;

   -- Converts a RR record to logicial representation
   --
   -- @value This
   -- Class object
   --
   -- @value DNS_Header
   -- DNS Packet Header
   --
   -- @value Parsed_RR
   -- Any parsed Resource Record from Processor.Packet
   --
   procedure From_Parsed_RR
     (This       : in out Parsed_Unknown_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record);

      -- Represents RData as a String for debug logging
      --
      -- @value This
      -- Class object
      --
      -- @returns
      -- String saying "UNKNOWN"
      --
   function RData_To_String
     (This : in Parsed_Unknown_RData)
      return String;

   -- Represents the resource record packet as a whole as a string
   --
   -- @value This
   -- Class object
   --
   -- @returns
   --  String that states "UNKNOWN"
   --
   function Print_Packet
     (This : in Parsed_Unknown_RData)
      return String;

   -- Frees and deallocates the class object
   --
   -- @value This
   -- Class object to deallocate
   --
   procedure Delete (This : in out Parsed_Unknown_RData);

private
   type Parsed_Unknown_RData is new DNSCatcher.DNS.Processor.RData
     .Parsed_RData with
   record
      RData : Unbounded_String;
   end record;

end DNSCatcher.DNS.Processor.RData.Unknown_Parser;
