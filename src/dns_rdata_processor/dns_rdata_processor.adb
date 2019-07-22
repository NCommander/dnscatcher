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
with DNS_RData_Processor.A_Parser;   use DNS_RData_Processor.A_Parser;
with DNS_RData_Processor.SOA_Parser; use DNS_RData_Processor.SOA_Parser;
with DNS_RData_Processor.OPT_Parser; use DNS_RData_Processor.OPT_Parser;
with DNS_RData_Processor.Domain_Name_Response_Parser;
use DNS_RData_Processor.Domain_Name_Response_Parser;
with DNS_RData_Processor.Unknown_Parser;
use DNS_RData_Processor.Unknown_Parser;
package body DNS_RData_Processor is

   function To_Parsed_RData
     (DNS_Header : DNS_Packet_Header;
      Parsed_RR : Parsed_DNS_Resource_Record)
      return Parsed_RData_Access
   is
      Working_Record : Parsed_RData_Access;
   begin
      case Parsed_RR.RType is
         when A =>
            Working_Record := new Parsed_A_RData;
         when SOA =>
            Working_Record := new Parsed_SOA_RData;
         when OPT =>
            Working_Record := new Parsed_OPT_RData;
            -- Handle all DNS responses which are just a name
         when CNAME =>
            Working_Record := new Parsed_DNR_RData;
         when PTR =>
            Working_Record := new Parsed_DNR_RData;
         when NS =>
            Working_Record := new Parsed_DNR_RData;
         when others =>
            Working_Record := new Parsed_Unknown_RData;
      end case;

      Working_Record.RName := Parsed_RR.RName;
      Working_Record.RType := Parsed_RR.RType;
      Working_Record.TTL   := Parsed_RR.TTL;
      Working_Record.From_Parsed_RR (DNS_Header, Parsed_RR);
      return Working_Record;
   end To_Parsed_RData;
end DNS_RData_Processor;
