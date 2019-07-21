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

with Interfaces; use Interfaces;
with Interfaces.C;

with System;

package body DNSCatcher.Utils is
   function Inet_Ntop
     (Family   : IP_Addr_Family;
      Raw_Data : Unbounded_String)
      return Unbounded_String
   is
      procedure Internal
        (Family : C.int;
         Src    : System.Address;
         Dst    : System.Address;
         Len    : C.int);
      pragma Import (C, Internal, "ada_inet_ntop");

      -- 16 is the max length of a v4 string + null
      C_IP_String : C.char_array (1 .. 16);
   begin
      -- Call the ada helper function
      Internal
        (C.int (Family'Enum_Rep), To_String (Raw_Data)'Address,
         C_IP_String'Address, 15);
      return To_Unbounded_String (C.To_Ada (C_IP_String));
   end Inet_Ntop;
end DNSCatcher.Utils;
