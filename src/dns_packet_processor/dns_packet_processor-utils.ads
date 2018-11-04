with Ada.Streams;

package DNS_Packet_Processor.Utils is
   function Read_Unsigned_16(Raw_Data: Stream_Element_Array_Ptr; Offset: in out Stream_Element_Offset) return Unsigned_16;
   function Read_Unsigned_32(Raw_Data: Stream_Element_Array_Ptr; Offset: in out Stream_Element_Offset) return Unsigned_32;
end DNS_Packet_Processor.Utils;
