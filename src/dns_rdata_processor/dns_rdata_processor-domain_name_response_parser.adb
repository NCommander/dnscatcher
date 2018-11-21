with Ada.Unchecked_Deallocation;
with Ada.Streams; use Ada.Streams;

package body DNS_RData_Processor.Domain_Name_Response_Parser is

   -- A records are simply four octlets which we need to turn into integers then
   -- decode back into an ASCII string
   procedure From_Parsed_RR (This : in out Parsed_DNR_RData;
      Parsed_RR                   :        Parsed_DNS_Resource_Record)
   is
      Offset : Stream_Element_Offset := Parsed_RR.RData_Offset;
   begin
      This.Domain_Name := Parse_DNS_Packet_Name_Records (Parsed_RR.Raw_Packet, Offset);
   end From_Parsed_RR;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function RClass_To_String (This : in Parsed_DNR_RData) return String is
   begin
      return "";
   end RClass_To_String;
   pragma Warnings (On, "formal parameter ""This"" is not referenced");

   function RData_To_String (This : in Parsed_DNR_RData) return String is
   begin
      return To_String (This.Domain_Name);
   end RData_To_String;

   function Print_Packet (This : in Parsed_DNR_RData) return String is
   begin
      return This.RType'Image & " " & RData_To_String (This);
   end Print_Packet;

   -- Obliberate ourselves
   procedure Delete (This : in out Parsed_DNR_RData) is
      procedure Free_Parsed_DNR_Record is new Ada.Unchecked_Deallocation
        (Object => Parsed_DNR_RData, Name => Parsed_DNR_RData_Access);
      Ptr : aliased Parsed_DNR_RData_Access := This'Unchecked_Access;
   begin
      Free_Parsed_DNR_Record (Ptr);
   end Delete;
end DNS_RData_Processor.Domain_Name_Response_Parser;
