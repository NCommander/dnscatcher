with DNS_Packet_Processor;         use DNS_Packet_Processor;
with DNS_RData_Processor.A_Parser; use DNS_RData_Processor.A_Parser;

package body DNS_RData_Processor is

   function To_Parsed_RData (Parsed_RR : Parsed_DNS_Resource_Record) return Parsed_RData_Access is
      Working_Record : Parsed_A_RData_Access;
   begin
      Working_Record       := new Parsed_A_RData;
      Working_Record.RName := Parsed_RR.RName;
      Working_Record.RType := Parsed_RR.RType;
      Working_Record.TTL   := Parsed_RR.TTL;
      Working_Record.From_Parsed_RR (Parsed_RR);
      return Parsed_RData_Access (Working_Record);
   end To_Parsed_RData;
end DNS_RData_Processor;
