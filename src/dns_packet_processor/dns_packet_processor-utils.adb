-- Copyright 2019 Michael Casadevall <michael@casadevall.pro>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body DNS_Packet_Processor.Utils is
   function Ntohs
     (Network_Short : Unsigned_16)
      return Unsigned_16
   is
      function Internal
        (Network_Short : Unsigned_16)
         return Unsigned_16;
      pragma Import (C, Internal, "ntohs");
   begin
      return Internal (Network_Short);
   end Ntohs;

   function Ntohl
     (Network_Long : Unsigned_32)
      return Unsigned_32
   is
      function Internal
        (Network_Long : Unsigned_32)
         return Unsigned_32;
      pragma Import (C, Internal, "ntohl");
   begin
      return Internal (Network_Long);
   end Ntohl;

   function Htons
     (Host_Short : Unsigned_16)
      return Unsigned_16
   is
      function Internal
        (Host_Short : Unsigned_16)
         return Unsigned_16;
      pragma Import (C, Internal, "htons");
   begin
      return Internal (Host_Short);
   end Htons;

   function Htonl
     (Host_Long : Unsigned_32)
      return Unsigned_32
   is
      function Internal
        (Host_Long : Unsigned_32)
         return Unsigned_32;
      pragma Import (C, Internal, "htonl");
   begin
      return Internal (Host_Long);
   end Htonl;

   -- Bullshit functions to handle Endianess, wish Ada handled this better
   function Read_Unsigned_16
     (Raw_Data :        Stream_Element_Array_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Unsigned_16
   is
      Network_Short : Unsigned_16;
      function From_Network_Value is new Ada.Unchecked_Conversion
        (Source => Stream_Element_Array, Target => Unsigned_16);

   begin
      Network_Short := Ntohs (From_Network_Value (Raw_Data
                (Offset .. Offset + 1)));
      Offset := Offset + 2;
      return Network_Short;
   end Read_Unsigned_16;

   -- 32-bit bullshit
   function Read_Unsigned_32
     (Raw_Data :        Stream_Element_Array_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Unsigned_32
   is
      Network_Long : Unsigned_32;
      function From_Network_Value is new Ada.Unchecked_Conversion
        (Source => Stream_Element_Array, Target => Unsigned_32);

   begin
      Network_Long := Ntohl (From_Network_Value (Raw_Data
                (Offset .. Offset + 3)));
      Offset := Offset + 4;
      return Network_Long;
   end Read_Unsigned_32;

   procedure Free_Parsed_DNS_Packet (Packet : in out Parsed_DNS_Packet_Ptr) is
      procedure Free_Ptr is new Ada.Unchecked_Deallocation
        (Object => Parsed_DNS_Packet, Name => Parsed_DNS_Packet_Ptr);
   begin
      Packet.Questions.Clear;

      for I of Packet.Answer
      loop
         I.Delete;
      end loop;

      for I of Packet.Authority
      loop
         I.Delete;
      end loop;

      for I of Packet.Additional
      loop
         I.Delete;
      end loop;

      Packet.Answer.Clear;
      Packet.Authority.Clear;
      Packet.Additional.Clear;

      Free_Ptr (Packet);
   end Free_Parsed_DNS_Packet;

end DNS_Packet_Processor.Utils;
