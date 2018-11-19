-- Misc code and other bits that are used in multiple modules end up here

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with DNS_Core_Constructs.Raw_Packet_Records; use DNS_Core_Constructs.Raw_Packet_Records;

package DNS_Core_Constructs.Utils is
   -- Deallocations
   procedure Free_Stream_Element_Array_Ptr is new Ada.Unchecked_Deallocation
     (Object => Stream_Element_Array, Name => Stream_Element_Array_Ptr);

   procedure Free_Raw_Packet_Record_Ptr(Ptr: in out Raw_Packet_Record_Ptr);

end DNS_Core_Constructs.Utils;
