with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;

with DNS_RData_Processor.Utils; use DNS_RData_Processor;

package body DNS_RData_Processor.A_Parser is
   
   -- A records are simply four octlets which we need to turn into integers then
   -- decode back into an ASCII string
   procedure From_Parsed_RR(This: in out Parsed_A_RData; Parsed_RR: Parsed_DNS_Resource_Record) is
   begin

      This.A_Record := Decode_DNS_IPv4_Address(Parsed_RR);
   end From_Parsed_RR;

   function RClass_To_String(This: in Parsed_A_RData) return String is
   begin
      return "";
   end RClass_To_String;

   function RData_To_String(This: in Parsed_A_RData) return String is
   begin
      return To_String(This.A_Record);
   end RData_To_String;

end DNS_RData_Processor.A_Parser;
