package body DNS_Core_Constructs.Utils is
   procedure Free_Raw_Packet_Record_Ptr (Ptr : in out Raw_Packet_Record_Ptr) is
      procedure Free_Ptr_Record is new Ada.Unchecked_Deallocation
        (Object => Raw_Packet_Record, Name => Raw_Packet_Record_Ptr);
   begin
      Free_Stream_Element_Array_Ptr (Ptr.Raw_Data.Data);
      Free_Ptr_Record (Ptr);
   end Free_Raw_Packet_Record_Ptr;

end DNS_Core_Constructs.Utils;
