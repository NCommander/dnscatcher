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

with System;

package body DNSCatcher.DNS.Processor.RData.Utils is
   function Decode_DNS_IPv4_Address
     (Parsed_RR : Parsed_DNS_Resource_Record)
      return Unbounded_String
   is
      type Raw_IPv4_Components is record
         A : Unsigned_8;
         B : Unsigned_8;
         C : Unsigned_8;
         D : Unsigned_8;
      end record;
      pragma Pack (Raw_IPv4_Components);
      for Raw_IPv4_Components'Bit_Order use System.High_Order_First;
      for Raw_IPv4_Components'Scalar_Storage_Order use System.High_Order_First;

      IPv4_Components : Raw_IPv4_Components;
      ASCII_IPv4      : Unbounded_String;

      function To_IPv4_Components is new Ada.Unchecked_Conversion
        (Source => String, Target => Raw_IPv4_Components);
   begin
      IPv4_Components :=
        To_IPv4_Components (To_String (Parsed_RR.RData) (1 .. 4));
      ASCII_IPv4 :=
        ASCII_IPv4 & IPv4_Components.A'Image & IPv4_Components.B'Image &
        IPv4_Components.C'Image & IPv4_Components.D'Image;

      return ASCII_IPv4;
   end Decode_DNS_IPv4_Address;
end DNSCatcher.DNS.Processor.RData.Utils;
