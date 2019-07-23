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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;

with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with DNSCatcher.Types; use DNSCatcher.Types;

package DNSCatcher.Utils is
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

   type IP_Addr_Family is
     (IPv4,
      IPv6);
   for IP_Addr_Family use (IPv4 => 1, IPv6 => 2);

   function Inet_Ntop
     (Family   : IP_Addr_Family;
      Raw_Data : Unbounded_String)
     return Unbounded_String;

   -- Deallocators
   procedure Free_Stream_Element_Array_Ptr is new Ada.Unchecked_Deallocation
     (Object => Stream_Element_Array, Name => Stream_Element_Array_Ptr);
end DNSCatcher.Utils;
