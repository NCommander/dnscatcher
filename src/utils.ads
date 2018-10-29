-- Misc code and other bits that are used in multiple modules end up here

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

limited with Raw_DNS_Packets;

package Utils is
   type Stream_Element_Array_Ptr is access Stream_Element_Array;

   -- Deallocations
   procedure Free_Stream_Element_Array_Ptr is new Ada.Unchecked_Deallocation
     (Object => Stream_Element_Array, Name => Utils.Stream_Element_Array_Ptr);

end Utils;
