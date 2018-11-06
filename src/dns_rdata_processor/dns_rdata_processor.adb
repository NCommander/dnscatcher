with DNS_Packet_Processor;               use DNS_Packet_Processor;
with DNS_RData_Processor.A_Parser;       use DNS_RData_Processor.A_Parser;
with DNS_RData_Processor.Unknown_Parser; use DNS_RData_Processor.Unknown_Parser;
package body DNS_RData_Processor is

   function To_Parsed_RData (Parsed_RR : Parsed_DNS_Resource_Record) return Parsed_RData_Access is
      Working_Record : Parsed_RData_Access;
   begin
      case Parsed_RR.RType is
         when A =>
            Working_Record := new Parsed_A_RData;
         when others =>
            Working_Record := new Parsed_Unknown_RData;
      end case;

      Working_Record.RName := Parsed_RR.RName;
      Working_Record.RType := Parsed_RR.RType;
      Working_Record.TTL   := Parsed_RR.TTL;
      Working_Record.From_Parsed_RR (Parsed_RR);
      return Working_Record;
   end To_Parsed_RData;
end DNS_RData_Processor;
