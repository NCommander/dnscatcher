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

package DNS_Packet_Processor.Utils is
   function Ntohs
     (Network_Short : Unsigned_16)
      return Unsigned_16;
   function Ntohl
     (Network_Long : Unsigned_32)
      return Unsigned_32;
   function Htons
     (Host_Short : Unsigned_16)
      return Unsigned_16;
   function Htonl
     (Host_Long : Unsigned_32)
      return Unsigned_32;

   function Read_Unsigned_16
     (Raw_Data :        Stream_Element_Array_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Unsigned_16;
   function Read_Unsigned_32
     (Raw_Data :        Stream_Element_Array_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Unsigned_32;
   procedure Free_Parsed_DNS_Packet (Packet : in out Parsed_DNS_Packet_Ptr);
end DNS_Packet_Processor.Utils;
