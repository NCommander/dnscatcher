package body DNS_RData_Processor.Unknown_Parser is

   -- A records are simply four octlets which we need to turn into integers then
   -- decode back into an ASCII string
   procedure From_Parsed_RR (This : in out Parsed_Unknown_RData; Parsed_RR : Parsed_DNS_Resource_Record)
   is
   begin
      This.RData := Parsed_RR.RData;
   end From_Parsed_RR;

   pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function RClass_To_String (This : in Parsed_Unknown_RData) return String is
   begin
      return "UNKNOWN";
   end RClass_To_String;

   function RData_To_String (This : in Parsed_Unknown_RData) return String is
   begin
      return "UNKNOWN";
   end RData_To_String;
   pragma Warnings (On, "formal parameter ""This"" is not referenced");
   
end DNS_RData_Processor.Unknown_Parser;
