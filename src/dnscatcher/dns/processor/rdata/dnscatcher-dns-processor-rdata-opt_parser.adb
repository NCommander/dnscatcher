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

with Interfaces; use Interfaces;

package body DNSCatcher.DNS.Processor.RData.OPT_Parser is

   -- EDNS is a pretty harry beast and incorporates a lot of stuff in ways
   -- different from all other packets. As such parsing is quite a bit more
   -- complicated than it is for other packet types.

   procedure From_Parsed_RR
     (This       : in out Parsed_OPT_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record)
   is
      Top_Half_TTL    : Interfaces.Unsigned_16;
      Bottom_Half_TTL : Interfaces.Unsigned_16;
      Full_RCode      : Interfaces.Unsigned_16 := 0;
   begin
      -- RClass is the UDP requester size
      This.Requester_UDP_Size := Parsed_RR.RClass;

      --!pp off
      -- TTL is subdivided into the following parts
      -- 0..7 - Extended RCode
      -- 8..16 - EDNS Version (must be zero)
      --
      -- The remainder is a 16-bit flags register set by
      -- IANA. At the time of writing, only a single flag,
      -- DO, is specified. The registry is available here:
      -- https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-13
      --
      -- EDNS Flags:
      -- 17 - DO
      -- 18..32 - Must be zero
      --
      -- The horseshit however continues. Despite being a 32-bit int, it's
      -- essentially two 16-bit ones, so we need to split this into a high
      -- and low half and then compare it. My brain is already hurting ...
      --!pp on

      -- To get the full RCode, we need to take the 4-bit "normal" RCode, then
      -- tack on the EDNS ones at the top for 12 bits total

      -- Since we're network byte order, load the high half first from the
      -- bottom bits
      Top_Half_TTL :=
        Interfaces.Unsigned_16
          (Shift_Right (Interfaces.Unsigned_32 (Parsed_RR.TTL), 16));
      Bottom_Half_TTL := Interfaces.Unsigned_16 (Parsed_RR.TTL and 16#ffff#);

      -- 0..7 MSB is our RCode
      Full_RCode := Top_Half_TTL and 16#ff00#;
      Full_RCode := Shift_Right (Full_RCode, 4);
      Full_RCode :=
        Full_RCode or Interfaces.Unsigned_16 (DNS_Header.Response_Code);
      This.Extended_RCode := RCodes'Enum_Val (Full_RCode);

      -- Grab the EDNS version. It should be zero
      This.EDNS_Version :=
        Interfaces.C.Extensions.Unsigned_8 (Top_Half_TTL and 16#ff#);

      -- Easiest way to fish out these bits is doing ANDs
      This.DNSSEC_OK := (Bottom_Half_TTL and 16#8000#) /= 0;
      This.Z_Section := Unsigned_15 (Bottom_Half_TTL and 16#7FF#);

      -- Mask off top bits
   end From_Parsed_RR;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function RClass_To_String
     (This : in Parsed_OPT_RData)
     return String
   is
   begin
      return "";
   end RClass_To_String;
   pragma Warnings (On, "formal parameter ""This"" is not referenced");

   function RData_To_String
     (This : in Parsed_OPT_RData)
     return String
   is
   begin
      return "RCode: " & This.Extended_RCode'Image & " Version: " &
        This.EDNS_Version'Image & " DO: " & This.DNSSEC_OK'Image & " Z: " &
        This.Z_Section'Image;
   end RData_To_String;

   function Print_Packet
     (This : in Parsed_OPT_RData)
     return String
   is
   begin
      return "OPT " & RData_To_String (This);
   end Print_Packet;

   -- Obliberate ourselves
   procedure Delete (This : in out Parsed_OPT_RData) is
   begin
      null;
   end Delete;

end DNSCatcher.DNS.Processor.RData.OPT_Parser;
