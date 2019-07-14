with Ada.Unchecked_Deallocation;

package body DNS_RData_Processor.Unknown_Parser is

   -- A records are simply four octlets which we need to turn into integers
   -- then decode back into an ASCII string
   pragma Warnings (Off, "formal parameter ""DNS_Header"" is not referenced");
   procedure From_Parsed_RR
     (This       : in out Parsed_Unknown_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record)
   is
   begin
      This.RData := Parsed_RR.RData;
   end From_Parsed_RR;
   pragma Warnings (On, "formal parameter ""DNS_Header"" is not referenced");

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function RClass_To_String
     (This : in Parsed_Unknown_RData)
      return String
   is
   begin
      return "UNKNOWN";
   end RClass_To_String;

   function RData_To_String
     (This : in Parsed_Unknown_RData)
      return String
   is
   begin
      return "UNKNOWN";
   end RData_To_String;

   function Print_Packet
     (This : in Parsed_Unknown_RData)
      return String
   is
   begin
      return "UNKNOWN";
   end Print_Packet;
   pragma Warnings (On, "formal parameter ""This"" is not referenced");

   -- Obliberate ourselves
   procedure Delete (This : in out Parsed_Unknown_RData) is
      procedure Free_Parsed_Unknown_Record is new Ada.Unchecked_Deallocation
        (Object => Parsed_Unknown_RData, Name => Parsed_Unknown_RData_Access);
      Ptr : aliased Parsed_Unknown_RData_Access := This'Unchecked_Access;
   begin
      Free_Parsed_Unknown_Record (Ptr);
   end Delete;
end DNS_RData_Processor.Unknown_Parser;
