with DNS_Common;       use DNS_Common;
with DNS_Common.Utils; use DNS_Common.Utils;
with Ada.Unchecked_Deallocation;

package body DNS_RData_Processor.A_Parser is

   -- A records are simply four octlets which we need to turn into integers
   -- then decode back into an ASCII string
   procedure From_Parsed_RR
     (This      : in out Parsed_A_RData;
      Parsed_RR :        Parsed_DNS_Resource_Record)
   is
   begin
      --This.A_Record := Decode_DNS_IPv4_Address (Parsed_RR);
      This.A_Record := Inet_Ntop (IPv4, Parsed_RR.RData);
   end From_Parsed_RR;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function RClass_To_String
     (This : in Parsed_A_RData)
      return String
   is
   begin
      return "";
   end RClass_To_String;
   pragma Warnings (On, "formal parameter ""This"" is not referenced");

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
end DNS_RData_Processor.A_Parser;
