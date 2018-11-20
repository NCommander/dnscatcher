with DNS_Packet_Processor; use DNS_Packet_Processor;

package DNS_RData_Processor.SOA_Parser is
   type Parsed_SOA_RData is new DNS_RData_Processor.Parsed_RData with record
      Primary_Nameserver : Unbounded_String;
      Responsible_Contact: Unbounded_String;
      Serial: Unsigned_32;
      Refresh: Unsigned_32;
      Retry: Unsigned_32;
      Expire: Unsigned_32;
      Minimum: Unsigned_32;
   end record;
   type Parsed_SOA_RData_Access is access all Parsed_SOA_RData;

   procedure From_Parsed_RR (This : in out Parsed_SOA_RData; Parsed_RR : Parsed_DNS_Resource_Record);
   function RData_To_String (This : in Parsed_SOA_RData) return String;
   function RClass_To_String (This : in Parsed_SOA_RData) return String;
   function Print_Packet (This : in Parsed_SOA_RData) return String;
   procedure Delete(This: in out Parsed_SOA_RData);

end DNS_RData_Processor.SOA_Parser;
