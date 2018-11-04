with DNS_Packet_Processor; use DNS_Packet_Processor;

package DNS_RData_Processor.Utils is
   function Decode_DNS_IPv4_Address(Parsed_RR: Parsed_DNS_Resource_Record) return Unbounded_String;

end DNS_RData_Processor.Utils;
