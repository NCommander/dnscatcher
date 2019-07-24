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
-- RData processor for SOA records
--
-- @summary
--
-- SOA records are Start of Authority records; they specify the authoritive
-- information for a given zone. They contain the primary nameserver for a
-- zone, a responsible contact, and caching information related to a zone,
-- and most importantly, the zone serial number.
--
-- An SOA record will exist for every given zone, and is a fundamental part of
-- the DNS record system.
--
package DNSCatcher.DNS.Processor.RData.SOA_Parser is
   -- Parsed SOA Data
   --
   -- @value Primary_Nameserver
   -- Contains the primary nameserver for the zone
   --
   -- @value Responsible_Contact
   -- The contact information (usually an email) for the zone
   --
   -- @value Serial
   -- The serial number of a given zone. Must be incremented every time a zone
   -- is updated, and used with NOTIFY operations as well. Serial numbers are
   -- *NOT* simple integer additions, but instead use serial number arithmetic
   -- to handle cases such as wraparound.
   --
   -- @value Refresh
   -- A caching nameserver should refresh the SOA for a given zone every X
   -- seconds. If the SOA serial is unchanged, data can remained cached.
   --
   -- @value Retry
   -- If the authoritive server(s) for a given zone are not responding, this
   -- is the interval that should be used to retry operations. It must be less
   -- than the Refresh value
   --
   -- @value Expire
   -- This is the period of time a zone can be cached before it is deleted if
   -- there is no response from the authoritive nameservers
   --
   -- @value Minimum
   -- The minimum time/TTL for negative caching.
   --
   type Parsed_SOA_RData is new DNSCatcher.DNS.Processor.RData
     .Parsed_RData with
   record
      Primary_Nameserver  : Unbounded_String;
      Responsible_Contact : Unbounded_String;
      Serial              : Unsigned_32;
      Refresh             : Unsigned_32;
      Retry               : Unsigned_32;
      Expire              : Unsigned_32;
      Minimum             : Unsigned_32;
   end record;
   type Parsed_SOA_RData_Access is access all Parsed_SOA_RData;

   -- Converts a RR record to logicial representation
   --
   -- @value This
   -- Class object
   --
   -- @value DNS_Header
   -- DNS Packet Header
   --
   -- @value Parsed_RR
   -- SOA parsed Resource Record from Processor.Packet
   --
   procedure From_Parsed_RR
     (This       : in out Parsed_SOA_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record);

      -- Represents RData as a String for debug logging
      --
      -- @value This
      -- Class object
      --
      -- @returns
      -- String representing the status of EDNS information
      --
   function RData_To_String
     (This : in Parsed_SOA_RData)
      return String;

   -- Represents the resource record packet as a whole as a string
   --
   -- @value This
   -- Class object
   --
   -- @returns
   --  String in the format of "SOA *soa info*
   --
   function Print_Packet
     (This : in Parsed_SOA_RData)
      return String;

   -- Frees and deallocates the class object
   --
   -- @value This
   -- Class object to deallocate
   --
   procedure Delete (This : in out Parsed_SOA_RData);

end DNSCatcher.DNS.Processor.RData.SOA_Parser;
