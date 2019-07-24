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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

limited with DNSCatcher.DNS.Processor.Packet;

with DNSCatcher.DNS;   use DNSCatcher.DNS;
with DNSCatcher.Types; use DNSCatcher.Types;

-- @summary
-- The RData Processor abstract class, used to implement all RData processors.
-- This package is used to convert rdata to/from wire form.
--
-- @description
-- DNS RRtypes have differing binary encoding depending on the resource record
-- type so this package acts as an abstraction layer for varying RData types
-- which are implemented as subclasses of this. In certain cases however, it
-- is necessary to get the derieved class and cast back to that. For example,
-- retrieving the OPT record like that is essentially required.
--
package DNSCatcher.DNS.Processor.RData is

   -- Abstract Parsed_Data object
   --
   -- These values are part of the common header of all RRtypes
   --
   -- @value RName
   -- Resource recode name
   --
   -- @value RType
   -- Resource record type
   --
   -- @value TTL
   -- Time to Live of a given record
   type Parsed_RData is abstract tagged record
      RName : Unbounded_String;
      RType : RR_Types;
      TTL   : Unsigned_32;
   end record;

   type Parsed_RData_Access is access all Parsed_RData'Class;

   -- Constructor for RData subclasses
   --
   -- To_Parsed_RData creates a Parsed_RData object from the correct derieved
   -- class and returns the common Parsed_RData class in response.
   --
   -- @value DNS_Header
   -- DNS Packet Header used for some RType information
   --
   -- @value Parsed_RR
   -- The initial logicial representation created by Packet Parser
   --
   function To_Parsed_RData
     (DNS_Header : DNS_Packet_Header;
      Parsed_RR  : DNSCatcher.DNS.Processor.Packet.Parsed_DNS_Resource_Record)
      return Parsed_RData_Access;

   -- Abstract class constructor
   --
   -- @value This
   -- RData object being constructed
   --
   -- @value DNS_Header
   -- DNS Packet Header
   --
   -- @value Parsed_RR
   -- Parsed resource record from the packet parsing step
   --
   procedure From_Parsed_RR
     (This       : in out Parsed_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        DNSCatcher.DNS.Processor.Packet
        .Parsed_DNS_Resource_Record) is abstract;

      -- Abstract conversion of RData to String
      --
      -- @value This
      -- RData object
      --
      -- @returns
      -- A string representation of RData
      --
   function RData_To_String
     (This : in Parsed_RData)
      return String is abstract;

   -- Abstract pretty printer
   --
   -- @value This
   -- RData object
   --
   -- @returns
   -- String representation of nicely formatted RData information
   --
   function Print_Packet
     (This : in Parsed_RData)
      return String is abstract;

   -- Deconstructor abstract method
   --
   -- Releases all resources allocated by the RData object
   --
   -- @value This
   -- Object to be deconstructed
   --
   procedure Delete (This : in out Parsed_RData) is abstract;
end DNSCatcher.DNS.Processor.RData;
