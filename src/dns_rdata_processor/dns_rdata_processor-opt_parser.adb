with Interfaces; use Interfaces;

package body DNS_RData_Processor.OPT_Parser is

   -- EDNS is a pretty harry beast and incorporates a lot of stuff in ways
   -- different from all other packets. As such parsing is quite a bit more
   -- complicated than it is for other packet types.

   procedure From_Parsed_RR
     (This       : in out Parsed_OPT_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record)
   is
      Full_RCode     : Interfaces.Unsigned_16 := 0;
   begin
      -- RClass is the UDP requester size
      This.Requester_UDP_Size := Parsed_RR.RClass;

      -- pp! off
      -- TTL is subdivided into the following parts 
      -- 0..7 - Extended RCode 
      -- 8..16 EDNS Version (must be zero) 
      --
      -- The remainder is a 16-bit flags register set by IANA. At the time of
      -- writing, only a single flag, DO, is specified. The registry is
      -- available here: 
      -- https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-13
      --
      -- EDNS Flags:
      -- 17 - DO
      -- 18..32 - Must be zero
      --!pp on
      
      -- To get the full RCode, we need to take the 4-bit "normal" RCode, then
      -- tack on the EDNS ones at the top for 12 bits total
      Full_RCode := Interfaces.Unsigned_16 (Parsed_RR.TTL and 16#ff#);
      Full_RCode := Shift_Left (Full_RCode, 4);
      Full_RCode := Full_RCode and Interfaces.Unsigned_16(DNS_Header.Response_Code);
      This.Extended_RCode := RCodes'Enum_Val(Full_RCode);

      -- Grab the EDNS version. It should be zero
      This.EDNS_Version := Interfaces.C.Extensions.Unsigned_8(Parsed_RR.TTL and 16#ff00#);
      if (This.EDNS_Version /= 0) then
         raise Unknown_EDNS_Version with "EDNS0 is only supported version!";
      end if;

      -- Easiest way to fish out these bits is doing ANDs and bitshifts

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
      return "RCode: " & This.Extended_RCode'Image & " Version: " & This.EDNS_Version'Image;
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

end DNS_RData_Processor.OPT_Parser;
