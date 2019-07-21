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

limited with DNS_Packet_Processor;

with DNSCatcher.DNS; use DNSCatcher.DNS;
with DNSCatcher.Types; use DNSCatcher.Types;

package DNS_RData_Processor is

   type Parsed_RData is abstract tagged record
      RName : Unbounded_String;
      RType : RR_Types;
      TTL   : Unsigned_32;
   end record;

   type Parsed_RData_Access is access all Parsed_RData'Class;

   function To_Parsed_RData
     (DNS_Header : DNS_Packet_Header;
      Parsed_RR : DNS_Packet_Processor.Parsed_DNS_Resource_Record)
      return Parsed_RData_Access;

   -- Represents RData in a string like fashion
   procedure From_Parsed_RR
     (This      : in out Parsed_RData;
      DNS_Header : DNS_Packet_Header;
      Parsed_RR : DNS_Packet_Processor.Parsed_DNS_Resource_Record) is abstract;
   function RClass_To_String
     (This : in Parsed_RData)
      return String is abstract;
   function RData_To_String
     (This : in Parsed_RData)
      return String is abstract;
   function Print_Packet
     (This : in Parsed_RData)
      return String is abstract;
   procedure Delete (This : in out Parsed_RData) is abstract;
end DNS_RData_Processor;
