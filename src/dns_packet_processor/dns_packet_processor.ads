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

with DNSCatcher.DNS; use DNSCatcher.DNS;
with DNSCatcher.Types; use DNSCatcher.Types;

with DNSCatcher.Utils.Logger; use DNSCatcher.Utils.Logger;
with DNS_RData_Processor; use DNS_RData_Processor;

package DNS_Packet_Processor is
   -- Create vector types for each type of section DNS question converted from
   -- wire format to human parsable format
   type Parsed_DNS_Question is record
      QName  : Unbounded_String;
      QType  : RR_Types;
      QClass : Classes;
   end record;

   type Parsed_DNS_Resource_Record is record
      RName        : Unbounded_String;
      RType        : RR_Types;
      RClass       : Unsigned_16;
      RCode        : Unsigned_4;
      TTL          : Unsigned_32;
      RData        : Unbounded_String;
      Raw_Packet : Stream_Element_Array_Ptr; -- Because sometimes we need the whole packet to decode
      RData_Offset : Stream_Element_Offset;
   end record;

   package Question_Vector is new Vectors (Positive, Parsed_DNS_Question);
   package Resource_Record_Vector is new Vectors (Positive,
      Parsed_RData_Access);

   type Parsed_DNS_Packet is record
      Header     : DNS_Packet_Header;
      Questions  : Question_Vector.Vector;
      Answer     : Resource_Record_Vector.Vector;
      Authority  : Resource_Record_Vector.Vector;
      Additional : Resource_Record_Vector.Vector;
   end record;
   type Parsed_DNS_Packet_Ptr is access Parsed_DNS_Packet;

   function Packet_Parser
     (Logger : Logger_Message_Packet_Ptr;
      Packet : Raw_Packet_Record_Ptr)
      return Parsed_DNS_Packet_Ptr;
   pragma Test_Case (Name => "Packet_Parser", Mode => Robustness);

   function Parse_DNS_Packet_Name_Records
     (Raw_Data :        Raw_DNS_Packet_Data;
      Offset   : in out Stream_Element_Offset)
      return Unbounded_String;

   function Parse_DNS_RR_Type
     (Raw_Data :        Raw_DNS_Packet_Data;
      Offset   : in out Stream_Element_Offset)
      return RR_Types;

   function Parse_DNS_Class
     (Raw_Data :        Raw_DNS_Packet_Data;
      Offset   : in out Stream_Element_Offset)
      return Classes;

   function Parse_Question_Record
     (Logger   :        Logger_Message_Packet_Ptr;
      Raw_Data :        Raw_DNS_Packet_Data;
      Offset   : in out Stream_Element_Offset)
      return Parsed_DNS_Question;

   function Parse_Resource_Record_Response
     (Logger   :        Logger_Message_Packet_Ptr;
      Packet   :        Raw_DNS_Packet;
      Offset   : in out Stream_Element_Offset)
      return Parsed_RData_Access;

   Unknown_RCode              : exception;
   Unknown_RR_Type            : exception;
   Unknown_Class              : exception;
   Unknown_Compression_Method : exception;

end DNS_Packet_Processor;
