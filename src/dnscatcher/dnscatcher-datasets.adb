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

with DNSCatcher.Utils; use DNSCatcher.Utils;

package body DNSCatcher.Datasets is
   -- Handles DNS Packets in a FIFO queue; built around Vectors, this may need
   -- to be changed for performance reasons at some point
   protected body Raw_Packet_Record_Queue is
      entry Put (Packet : in Raw_Packet_Record) when True is
      begin
         Stored_Packets.Append (Packet);
         Packet_Count := Packet_Count + 1;
         if Callback_Func /= null then
            Callback_Func.all;
         end if;
      end Put;
      entry Get (Packet : out Raw_Packet_Record_Ptr) when True is
         Alloced_Packet : constant Raw_Packet_Record_Ptr := new Raw_Packet_Record;
      begin
         if Packet_Count > 0 then
            Alloced_Packet.all := Stored_Packets.First_Element;
            Stored_Packets.Delete_First;
            Packet_Count := Packet_Count - 1;
            Packet := Alloced_Packet;
         else
            Packet := null;
         end if;
      end Get;
      entry Count (Count : out Integer) when True is
      begin
         Count := Packet_Count;
      end Count;
      entry Empty when True is
      begin
         -- Release all memory
         declare
            procedure Dump_Vector_Data (c : Stored_Packets_Vector.Cursor) is
            begin
               Free_Stream_Element_Array_Ptr
                 (Stored_Packets (c).Raw_Data.Data);
            end Dump_Vector_Data;
         begin
            Stored_Packets.Iterate (Dump_Vector_Data'access);
         end;
      end Empty;
      entry Set_Callback_Function ( Callback_Function : in Raw_Packet_Record_Queue_Callback ) when True is
      begin
         Callback_Func := Callback_Function;
      end Set_Callback_Function;
   end Raw_Packet_Record_Queue;
end DNSCatcher.Datasets;
