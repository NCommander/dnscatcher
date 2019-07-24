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

with Ada.Unchecked_Deallocation;

with DNSCatcher.Utils; use DNSCatcher.Utils;

package body DNSCatcher.DNS.Processor.RData.A_Parser is

   -- A records are simply four octlets which we need to turn into integers
   -- then decode back into an ASCII string
   pragma Warnings (Off, "formal parameter ""DNS_Header"" is not referenced");
   procedure From_Parsed_RR
     (This       : in out Parsed_A_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record)
   is
   begin
      --This.A_Record := Decode_DNS_IPv4_Address (Parsed_RR);
      This.A_Record := Inet_Ntop (IPv4, Parsed_RR.RData);
   end From_Parsed_RR;
   pragma Warnings (On, "formal parameter ""DNS_Header"" is not referenced");

   function RData_To_String
     (This : in Parsed_A_RData)
      return String
   is
   begin
      return To_String (This.A_Record);
   end RData_To_String;

   function Print_Packet
     (This : in Parsed_A_RData)
      return String
   is
   begin
      return "A " & RData_To_String (This);
   end Print_Packet;

   -- Obliberate ourselves
   procedure Delete (This : in out Parsed_A_RData) is
      procedure Free_Parsed_A_Record is new Ada.Unchecked_Deallocation
        (Object => Parsed_A_RData, Name => Parsed_A_RData_Access);
      Ptr : aliased Parsed_A_RData_Access := This'Unchecked_Access;
   begin
      Free_Parsed_A_Record (Ptr);
   end Delete;
end DNSCatcher.DNS.Processor.RData.A_Parser;
