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

-- @description
--
-- RData processor for A records
--
-- @summary
--
-- Processes an A record into a IPv4 string
--
package DNSCatcher.DNS.Processor.RData.A_Parser is
   -- Parsed A RData Representation
   type Parsed_A_RData is
     new DNSCatcher.DNS.Processor.RData.Parsed_RData with private;
   type Parsed_A_RData_Access is access all Parsed_A_RData;

   -- Converts a RR record to logicial representation
   --
   -- @value This
   -- Class object
   --
   -- @value DNS_Header
   -- DNS Packet Header
   --
   -- @value Parsed_RR
   -- A parsed Resource Record from Processor.Packet
   --
   procedure From_Parsed_RR
     (This       : in out Parsed_A_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record);

      -- Represents RData as a String for debug logging
      --
      -- @value This
      -- Class object
      --
      -- @returns
      -- An IPv4 String
      --
   function RData_To_String
     (This : in Parsed_A_RData)
      return String;

   -- Represents the resource record packet as a whole as a string
   --
   -- @value This
   -- Class object
   --
   -- @returns
   -- String in the format of "A *IPv4 String*
   --
   function Print_Packet
     (This : in Parsed_A_RData)
      return String;

   -- Frees and deallocates the class object
   --
   -- @value This
   -- Class object to deallocate
   --
   procedure Delete (This : in out Parsed_A_RData);

private
   type Parsed_A_RData is new DNSCatcher.DNS.Processor.RData.Parsed_RData with
   record
      A_Record : Unbounded_String;
   end record;
end DNSCatcher.DNS.Processor.RData.A_Parser;
