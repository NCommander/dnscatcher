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

-- Some DNS records such as NS and CNAME return a common rtype format which is
-- just a compressed domain name; this package handles both those cases.

with DNSCatcher.DNS.Processor.Packet; use DNSCatcher.DNS.Processor.Packet;

package DNSCatcher.DNS.Processor.RData.Domain_Name_Response_Parser is
   type Parsed_DNR_RData is
     new DNSCatcher.DNS.Processor.RData.Parsed_RData with private;
   type Parsed_DNR_RData_Access is access all Parsed_DNR_RData;

   procedure From_Parsed_RR
     (This       : in out Parsed_DNR_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record);
   function RData_To_String
     (This : in Parsed_DNR_RData)
     return String;
   function RClass_To_String
     (This : in Parsed_DNR_RData)
     return String;
   function Print_Packet
     (This : in Parsed_DNR_RData)
     return String;
   procedure Delete (This : in out Parsed_DNR_RData);

private
   type Parsed_DNR_RData is new DNSCatcher.DNS.Processor.RData
     .Parsed_RData with
   record
      Domain_Name : Unbounded_String;
   end record;
end DNSCatcher.DNS.Processor.RData.Domain_Name_Response_Parser;
