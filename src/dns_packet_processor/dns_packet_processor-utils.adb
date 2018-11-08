with Ada.Unchecked_Conversion;

package body DNS_Packet_Processor.Utils is
   function Ntohs (Network_Short : Unsigned_16) return Unsigned_16 is
      function Internal (Network_Short : Unsigned_16) return Unsigned_16;
      pragma Import (C, Internal, "ntohs");
   begin
      return Internal (Network_Short);
   end Ntohs;

   function Ntohl (Network_Long : Unsigned_32) return Unsigned_32 is
      function Internal (Network_Long : Unsigned_32) return Unsigned_32;
      pragma Import (C, Internal, "ntohl");
   begin
      return Internal (Network_Long);
   end Ntohl;

   -- Bullshit functions to handle Endianess, wish Ada handled this better
   function Read_Unsigned_16 (Raw_Data :        Stream_Element_Array_Ptr;
      Offset                           : in out Stream_Element_Offset) return Unsigned_16
   is
      Network_Short : Unsigned_16;
      function From_Network_Value is new Ada.Unchecked_Conversion (Source => Stream_Element_Array,
         Target                                                           => Unsigned_16);

   begin
      Network_Short := Ntohs (From_Network_Value (Raw_Data (Offset .. Offset + 1)));
      Offset        := Offset + 2;
      return Network_Short;
   end Read_Unsigned_16;

   -- 32-bit bullshit
   function Read_Unsigned_32 (Raw_Data :        Stream_Element_Array_Ptr;
      Offset                           : in out Stream_Element_Offset) return Unsigned_32
   is
      Network_Long : Unsigned_32;
      function From_Network_Value is new Ada.Unchecked_Conversion (Source => Stream_Element_Array,
         Target                                                           => Unsigned_32);

   begin
      Network_Long := Ntohl (From_Network_Value (Raw_Data (Offset .. Offset + 3)));
      Offset       := Offset + 4;
      return Network_Long;
   end Read_Unsigned_32;

end DNS_Packet_Processor.Utils;