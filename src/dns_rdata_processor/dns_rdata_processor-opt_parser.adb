package body DNS_RData_Processor.OPT_Parser is
   -- A records are simply four octlets which we need to turn into integers
   -- then decode back into an ASCII string
   procedure From_Parsed_RR
     (This      : in out Parsed_OPT_RData;
      Parsed_RR :        Parsed_DNS_Resource_Record)
   is
   begin
      null;
   end From_Parsed_RR;

   --pragma Warnings (Off, "formal parameter ""This"" is not referenced");
   function RClass_To_String
     (This : in Parsed_OPT_RData)
      return String
   is
   begin
      return "";
   end RClass_To_String;
   --pragma Warnings (On, "formal parameter ""This"" is not referenced");

   function RData_To_String
     (This : in Parsed_OPT_RData)
      return String
   is
   begin
      return "";
   end RData_To_String;

   function Print_Packet
     (This : in Parsed_OPT_RData)
      return String
   is
   begin
      return "A " & RData_To_String (This);
   end Print_Packet;

   -- Obliberate ourselves
   procedure Delete (This : in out Parsed_OPT_RData) is
   begin
      null;
   end Delete;
   

end DNS_RData_Processor.OPT_Parser;
