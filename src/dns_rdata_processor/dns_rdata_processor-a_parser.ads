with DNS_Packet_Processor; use DNS_Packet_Processor;

package DNS_RData_Processor.A_Parser is
   type Parsed_A_RData is new DNS_RData_Processor.Parsed_RData with private;
   type Parsed_A_RData_Access is access all Parsed_A_RData;

   procedure From_Parsed_RR
     (This       : in out Parsed_A_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record);
   function RData_To_String
     (This : in Parsed_A_RData)
      return String;
   function RClass_To_String
     (This : in Parsed_A_RData)
      return String;
   function Print_Packet
     (This : in Parsed_A_RData)
      return String;
   procedure Delete (This : in out Parsed_A_RData);

private
   type Parsed_A_RData is new DNS_RData_Processor.Parsed_RData with record
      A_Record : Unbounded_String;
   end record;
end DNS_RData_Processor.A_Parser;
