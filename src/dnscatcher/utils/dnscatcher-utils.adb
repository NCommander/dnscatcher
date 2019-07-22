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

with System;

with Ada.Unchecked_Conversion;

with Interfaces.C;

package body DNSCatcher.Utils is
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


   function Inet_Ntop
     (Family   : IP_Addr_Family;
      Raw_Data : Unbounded_String)
      return Unbounded_String
   is
      procedure Internal
        (Family : Interfaces.C.int;
         Src    : System.Address;
         Dst    : System.Address;
         Len    : Interfaces.C.int);
      pragma Import (C, Internal, "ada_inet_ntop");

      -- 16 is the max length of a v4 string + null
      C_IP_String : Interfaces.C.char_array (1 .. 16);
   begin
      -- Call the ada helper function
      Internal
        (Interfaces.C.int (Family'Enum_Rep), To_String (Raw_Data)'Address,
         C_IP_String'Address, 15);
      return To_Unbounded_String (Interfaces.C.To_Ada (C_IP_String));
   end Inet_Ntop;
end DNSCatcher.Utils;
