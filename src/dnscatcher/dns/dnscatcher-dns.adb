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

package body DNSCatcher.DNS is
   function To_String
     (RR_Type : RR_Types)
     return String
   is
   begin
      -- This **** is required because 'in' is a keyword and Ada is case
      -- insensitive
      case RR_Type is
         when WILDCARD =>
            return "*";
         when others =>
            return RR_Types'Image (RR_Type);
      end case;

   end To_String;

   function To_String
     (DNS_Class : Classes)
     return String
   is
   begin
      -- This **** is required because 'in' is a keyword and Ada is case
      -- insensitive
      case DNS_Class is
         when INternet =>
            return "IN";
         when others =>
            return Classes'Image (DNS_Class);
      end case;

   end To_String;
end DNSCatcher.DNS;
