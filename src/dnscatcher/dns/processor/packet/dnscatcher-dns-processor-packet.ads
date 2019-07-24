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

with Ada.Containers.Vectors;  use Ada.Containers;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Streams;             use Ada.Streams;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with DNSCatcher.DNS;   use DNSCatcher.DNS;
with DNSCatcher.Types; use DNSCatcher.Types;

with DNSCatcher.Utils.Logger;        use DNSCatcher.Utils.Logger;
with DNSCatcher.DNS.Processor.RData; use DNSCatcher.DNS.Processor.RData;

-- @summary
--
-- Handles processing of raw DNS packets and their representation from wire
-- form to useful data structures.
--
-- @description
--
-- The Packet Processor is the top level interface for parsing a packet, and
-- getting useful information out of it. It also parses RData throught the
-- RData processor to create a single set of record(s) that describe a given
-- DNS packet
--
package DNSCatcher.DNS.Processor.Packet is

   -- Logicial representation of a DNS Question packet
   type Parsed_DNS_Question is record
      QName  : Unbounded_String; -- Domain the question is asked for
      QType  : RR_Types; -- QType RRType desired by client
      QClass : Classes; -- DNS Class queried
   end record;

   -- Logicial representation of a parsed DNS Resource Record
   type Parsed_DNS_Resource_Record is record
      RName        : Unbounded_String; -- Resource Name
      RType        : RR_Types; -- Resource Record Type
      RClass       : Unsigned_16; -- DNS Class
      RCode        : Unsigned_4; -- Response code
      TTL          : Unsigned_32; -- Time to Live
      RData        : Unbounded_String; -- Raw RData response
      Raw_Packet : Stream_Element_Array_Ptr; -- Raw packet data, used by RData parser
      RData_Offset : Stream_Element_Offset; -- Offset within raw packet to start of RData
   end record;

   -- Vector class for questions
   package Question_Vector is new Vectors (Positive, Parsed_DNS_Question);

   -- Vector class for Resource Records
   package Resource_Record_Vector is new Vectors (Positive,
      Parsed_RData_Access);

   -- Parsed DNS Packet
   type Parsed_DNS_Packet is record
      Header     : DNS_Packet_Header; -- DNS Packet Header
      Questions  : Question_Vector.Vector; -- Questions section
      Answer     : Resource_Record_Vector.Vector; -- Answers section
      Authority  : Resource_Record_Vector.Vector; -- Authoritive section
      Additional : Resource_Record_Vector.Vector; -- Additional Section
   end record;
   type Parsed_DNS_Packet_Ptr is access Parsed_DNS_Packet;

   -- Converts raw DNS packets into parsed DNS packets
   --
   -- @value Logger
   -- Pointer to the a logger message packet from the caller
   --
   -- @value Packet
   -- Raw DNS data gathered from DNSCatcher.Network.*
   --
   -- @returns
   -- Pointer to parsed DNS data; must be freed by caller
   --
   function Packet_Parser
     (Logger : Logger_Message_Packet_Ptr;
      Packet : Raw_Packet_Record_Ptr)
      return Parsed_DNS_Packet_Ptr;
   pragma Test_Case (Name => "Packet_Parser", Mode => Robustness);

   Unknown_RCode              : exception;
   Unknown_RR_Type            : exception;
   Unknown_Class              : exception;
   Unknown_Compression_Method : exception;

   -- Converts DNS names in packets to string
   --
   -- The full packet is required due to DNS compression
   --
   -- @value Raw_Data
   -- Pointer to the full packet
   --
   -- @value Offset
   -- Offet to the string to decode
   --
   -- @returns
   -- Unbounded_String with the DNS name decoded or exception
   --
   function Parse_DNS_Packet_Name_Records
     (Raw_Data :        Raw_DNS_Packet_Data_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Unbounded_String;

   -- Converts DNS RR Types in packets to RRTypes Enum
   --
   -- @value Raw_Data
   -- Pointer to the full packet
   --
   -- @value Offset
   -- Offet to the string to decode
   --
   -- @returns
   -- DNS RR_Type
   --
   function Parse_DNS_RR_Type
     (Raw_Data :        Raw_DNS_Packet_Data_Ptr;
      Offset   : in out Stream_Element_Offset)
      return RR_Types;

   -- Converts DNS Class in packets to DNS Class Enum
   --
   -- @value Raw_Data
   -- Pointer to the full packet
   --
   -- @value Offset
   -- Offet to the string to decode
   --
   -- @returns
   -- DNS Class
   --
   function Parse_DNS_Class
     (Raw_Data :        Raw_DNS_Packet_Data_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Classes;

   -- Converts DNS Questions in packets to logicial representation
   --
   -- @value Logger
   -- Pointer to logger message packet
   --
   -- @value Raw_Data
   -- The full packet data section to decode
   --
   -- @value Offset
   -- Offet to the string to decode
   --
   -- @returns
   -- Parsed DNS Question
   --
   function Parse_Question_Record
     (Logger   :        Logger_Message_Packet_Ptr;
      Raw_Data :        Raw_DNS_Packet_Data_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Parsed_DNS_Question;

   -- Converts DNS resource record to logicial representation
   --
   -- @value Logger
   -- Pointer to logger message packet
   --
   -- @value Packet
   -- The full packet to decode
   --
   -- @value Offset
   -- Offet to the resource record to decode
   --
   -- @returns
   -- Parsed Resource Record
   --
   function Parse_Resource_Record_Response
     (Logger :        Logger_Message_Packet_Ptr;
      Packet :        Raw_DNS_Packet;
      Offset : in out Stream_Element_Offset)
      return Parsed_RData_Access;

   -- Deallocators

   -- Deallocation helper for Parsed_DNS_Packet_Ptrs
   --
   -- @value Packet
   -- The packet to be deallocated
   --
   procedure Free_Parsed_DNS_Packet (Packet : in out Parsed_DNS_Packet_Ptr);

end DNSCatcher.DNS.Processor.Packet;
