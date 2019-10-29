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

with DNSCatcher.Utils; use DNSCatcher.Utils;

package body DNSCatcher.Types is

   procedure Free_Raw_DNS_Packet (Packet : in out Raw_DNS_Packet) is
   begin
      Free_Stream_Element_Array_Ptr (Packet.Data);
   end Free_Raw_DNS_Packet;

   procedure Free_Raw_Packet_Record_Ptr (Ptr : in out Raw_Packet_Record_Ptr) is
      procedure Free_Ptr_Record is new Ada.Unchecked_Deallocation
        (Object => Raw_Packet_Record, Name => Raw_Packet_Record_Ptr);
   begin
      Free_Stream_Element_Array_Ptr (Ptr.Raw_Data.Data);
      Free_Ptr_Record (Ptr);
   end Free_Raw_Packet_Record_Ptr;

   function To_C(RPP : Raw_Packet_Record) return Raw_Packet_Record_C is
      RPPC : Raw_Packet_Record_C;
   begin
      RPPC.From_Address := Interfaces.C.To_C(To_String(RPP.From_Address));
      RPPC.From_Port := Interfaces.C.int(RPP.From_Port);
      RPPC.To_Address := Interfaces.C.To_C(To_String(RPP.To_Address));
      RPPC.To_Port := Interfaces.C.int(RPP.To_Port);

      return RPPC;
   end To_C;

end DNSCatcher.Types;
