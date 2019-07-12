-- Some DNS records such as NS and CNAME return a common rtype format which is
-- just a compressed domain name; this package handles both those cases.

with DNS_Packet_Processor; use DNS_Packet_Processor;

package DNS_RData_Processor.Domain_Name_Response_Parser is
   type Parsed_DNR_RData is new DNS_RData_Processor.Parsed_RData with private;
   type Parsed_DNR_RData_Access is access all Parsed_DNR_RData;

   procedure From_Parsed_RR
     (This      : in out Parsed_DNR_RData;
      Parsed_RR :        Parsed_DNS_Resource_Record);
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
   type Parsed_DNR_RData is new DNS_RData_Processor.Parsed_RData with record
      Domain_Name : Unbounded_String;
   end record;
end DNS_RData_Processor.Domain_Name_Response_Parser;
