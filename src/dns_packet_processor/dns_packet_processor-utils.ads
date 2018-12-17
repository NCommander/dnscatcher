package DNS_Packet_Processor.Utils is
   function Ntohs (Network_Short : Unsigned_16) return Unsigned_16;
   function Ntohl (Network_Long : Unsigned_32) return Unsigned_32;
   function Htons (Host_Short : Unsigned_16) return Unsigned_16;
   function Htonl (Host_Long : Unsigned_32) return Unsigned_32;

   function Read_Unsigned_16 (Raw_Data :        Stream_Element_Array_Ptr;
      Offset                           : in out Stream_Element_Offset) return Unsigned_16;
   function Read_Unsigned_32 (Raw_Data :        Stream_Element_Array_Ptr;
                              Offset                           : in out Stream_Element_Offset) return Unsigned_32;
   procedure Free_Parsed_DNS_Packet(Packet: in out Parsed_DNS_Packet_Ptr);
end DNS_Packet_Processor.Utils;
