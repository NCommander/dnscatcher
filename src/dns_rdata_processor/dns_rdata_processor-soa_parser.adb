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

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with DNSCatcher.Utils; use DNSCatcher.Utils;

package body DNS_RData_Processor.SOA_Parser is

   -- SOA records are messy. There are two compressed domain names we need to
   -- decode, then some 32 bit ints that follow afterwards, so let's try and
   -- get that data first and out of the way
   pragma Warnings (Off, "formal parameter ""DNS_Header"" is not referenced");
   procedure From_Parsed_RR
     (This       : in out Parsed_SOA_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record)
   is
      Offset : Stream_Element_Offset := Parsed_RR.RData_Offset;
   begin
      -- Convert things back to SEA Types Try to decode MNAME first
      This.Primary_Nameserver :=
        Parse_DNS_Packet_Name_Records (Parsed_RR.Raw_Packet, Offset);
      This.Responsible_Contact :=
        Parse_DNS_Packet_Name_Records (Parsed_RR.Raw_Packet, Offset);
      This.Serial  := Read_Unsigned_32 (Parsed_RR.Raw_Packet, Offset);
      This.Refresh := Read_Unsigned_32 (Parsed_RR.Raw_Packet, Offset);
      This.Retry   := Read_Unsigned_32 (Parsed_RR.Raw_Packet, Offset);
      This.Expire  := Read_Unsigned_32 (Parsed_RR.Raw_Packet, Offset);
      This.Minimum := Read_Unsigned_32 (Parsed_RR.Raw_Packet, Offset);
   end From_Parsed_RR;
   pragma Warnings (On, "formal parameter ""DNS_Header"" is not referenced");

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function RClass_To_String
     (This : in Parsed_SOA_RData)
      return String
   is
   begin
      return "";
   end RClass_To_String;
   pragma Warnings (On, "formal parameter ""This"" is not referenced");

   function RData_To_String
     (This : in Parsed_SOA_RData)
      return String
   is
   begin
      return To_String (This.Primary_Nameserver) & " " &
        To_String (This.Responsible_Contact) & This.Serial'Image &
        This.Refresh'Image & This.Retry'Image & This.Expire'Image &
        This.Minimum'Image;
   end RData_To_String;

   function Print_Packet
     (This : in Parsed_SOA_RData)
      return String
   is
   begin
      return "SOA " & This.RData_To_String;
   end Print_Packet;

   -- Obliberate ourselves
   procedure Delete (This : in out Parsed_SOA_RData) is
      procedure Free_Parsed_SOA_Record is new Ada.Unchecked_Deallocation
        (Object => Parsed_SOA_RData, Name => Parsed_SOA_RData_Access);
      Ptr : aliased Parsed_SOA_RData_Access := This'Unchecked_Access;
   begin
      Free_Parsed_SOA_Record (Ptr);
   end Delete;
end DNS_RData_Processor.SOA_Parser;
