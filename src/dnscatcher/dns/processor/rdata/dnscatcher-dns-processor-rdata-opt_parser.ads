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
-- RData processor for OPT/EDNS0 records
--
-- @summary
--
-- Processes EDNS record data into a usable form. Unlike other RRtypes, OPT is
-- a special "meta" type that is used to handle EDNS data. There can only be a
-- single OPT record per reply, and various options are set in it that affect
-- the rules of how the DNS client/server operate.
--
package DNSCatcher.DNS.Processor.RData.OPT_Parser is
   -- Parsed OPT Data
   --
   -- @value Requester_UDP_Size
   -- Represents the MTU of a packet the client can receive or send over UDP.
   -- This value is always present for EDNS0 requests. Defined as CLASS in
   -- RFC6891
   --
   -- @value Extended_RCode
   -- The actual RCode value of a DNS message if EDNS0 is in use. EDNS has the
   -- top 8 bits which prefix the legacy RCode in the DNS packet header.
   --
   -- @value EDNS_Version
   -- Defines the version of EDNS in use. Currently only version 0 is specified
   --
   -- @value DNSSEC_OK
   -- Part of the flags area of EDNS, determines if a client wants DNSSEC info
   -- from the resolver
   --
   -- @value Z_Section
   -- Unused flags representing the last 15 bits of the EDNS message header
   --
   -- @value OPT_Values_Len
   -- Length of the EDNS parameters area
   --
   -- @value OPT_Values_Data
   -- Raw data of OPT value pairs, to be processed further
   --
   type Parsed_OPT_RData is new DNSCatcher.DNS.Processor.RData
     .Parsed_RData with
   record
      Requester_UDP_Size : Unsigned_16;
      Extended_RCode     : RCodes;
      EDNS_Version       : Unsigned_8;
      DNSSEC_OK          : Boolean;
      Z_Section          : Unsigned_15;
      OPT_Values_Len     : Unsigned_16;
      OPT_Values_Data    : Unbounded_String;
   end record;
   type Parsed_OPT_RData_Access is access all Parsed_OPT_RData;

   -- Converts a RR record to logicial representation
   --
   -- @value This
   -- Class object
   --
   -- @value DNS_Header
   -- DNS Packet Header
   --
   -- @value Parsed_RR
   -- OPT parsed Resource Record from Processor.Packet
   --
   procedure From_Parsed_RR
     (This       : in out Parsed_OPT_RData;
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
     (This : in Parsed_OPT_RData)
      return String;

   -- Represents the resource record packet as a whole as a string
   --
   -- @value This
   -- Class object
   --
   -- @returns
   --  String in the format of "OPT *edns info*
   --
   function Print_Packet
     (This : in Parsed_OPT_RData)
      return String;

   -- Frees and deallocates the class object
   --
   -- @value This
   -- Class object to deallocate
   --
   procedure Delete (This : in out Parsed_OPT_RData);

   Unknown_EDNS_Version : exception;
end DNSCatcher.DNS.Processor.RData.OPT_Parser;
