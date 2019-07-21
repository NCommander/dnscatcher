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

with Ada.Containers.Vectors; use Ada.Containers;

with DNSCatcher.Types; use DNSCatcher.Types;

package DNSCatcher.Datasets is
   package Stored_Packets_Vector is new Vectors (Natural, Raw_Packet_Record);
   use Stored_Packets_Vector;

   protected type Raw_Packet_Record_Queue is
      entry Put (Packet : in Raw_Packet_Record);
      entry Get (Packet : out Raw_Packet_Record);
      entry Count (Count : out Integer);
      entry Empty;
   private
      Stored_Packets : Vector;
      Packet_Count   : Integer := 0;
   end Raw_Packet_Record_Queue;
   type DNS_Raw_Packet_Queue_Ptr is access Raw_Packet_Record_Queue;
end DNSCatcher.Datasets;
