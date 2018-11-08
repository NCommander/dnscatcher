with DNS_Common; use DNS_Common;
with DNS_Common.Utils; use DNS_Common.Utils;
package body DNS_RData_Processor.A_Parser is

   -- A records are simply four octlets which we need to turn into integers then
   -- decode back into an ASCII string
   procedure From_Parsed_RR (This : in out Parsed_A_RData; Parsed_RR : Parsed_DNS_Resource_Record)
   is
   begin
      --This.A_Record := Decode_DNS_IPv4_Address (Parsed_RR);
      This.A_Record := Inet_Ntop(IPv4, Parsed_RR.RData);
   end From_Parsed_RR;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function RClass_To_String (This : in Parsed_A_RData) return String is
   begin
      return "";
   end RClass_To_String;
   pragma Warnings (On, "formal parameter ""This"" is not referenced");

   function RData_To_String (This : in Parsed_A_RData) return String is
   begin
      return To_String (This.A_Record);
   end RData_To_String;

end DNS_RData_Processor.A_Parser;