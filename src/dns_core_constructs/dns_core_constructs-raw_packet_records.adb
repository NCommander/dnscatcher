with DNS_Core_Constructs.Utils; use DNS_Core_Constructs.Utils;

package body DNS_Core_Constructs.Raw_Packet_Records is
   -- Handles DNS Packets in a FIFO queue; built around Vectors, this may need to be changed for
   -- performance reasons at some point
   protected body Raw_Packet_Record_Queue is
      entry Put (Packet : in Raw_Packet_Record) when True is
      begin
         Stored_Packets.Append (Packet);
         Packet_Count := Packet_Count + 1;
      end Put;
      entry Get (Packet : out Raw_Packet_Record) when Packet_Count > 0 is
      begin
         Packet := Stored_Packets.First_Element;
         Stored_Packets.Delete_First;
         Packet_Count := Packet_Count - 1;
      end Get;
      entry Count (Count : out Integer) when True is
      begin
         Count := Packet_Count;
      end Count;
      entry Empty when True is
      begin
         -- Release all memory
         declare
            procedure Dump_Vector_Data (c : Stored_Packets_Vector.Cursor) is
            begin
               Free_Stream_Element_Array_Ptr (Stored_Packets (c).Raw_Data.Data);
            end Dump_Vector_Data;
         begin
            Stored_Packets.Iterate (Dump_Vector_Data'access);
         end;
      end Empty;
   end Raw_Packet_Record_Queue;
end DNS_Core_Constructs.Raw_Packet_Records;
