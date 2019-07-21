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

with DNSCatcher.Network;

with DNSCatcher.Datasets; use DNSCatcher.Datasets;

package DNSCatcher.Network.UDP.Sender is
   -- Tasks Definition
   task type Send_Packet_Task is
      entry Initialize
        (Socket       : Socket_Type;
         Packet_Queue : DNS_Raw_Packet_Queue_Ptr);
      entry Start;
      entry Stop;
   end Send_Packet_Task;
   type Send_Packet_Task_Ptr is access Send_Packet_Task;

   type UDP_Sender_Interface is
     new DNSCatcher.Network.Sender_Interface with
   record
      Config        : Configuration_Ptr;
      Sender_Socket : Socket_Type;
      Sender_Task   : Send_Packet_Task_Ptr;
      Packet_Queue  : DNS_Raw_Packet_Queue_Ptr;
   end record;
   type IPv4_UDP_Receiver_Interface_Ptr is access UDP_Sender_Interface;

   procedure Initialize
     (This   : in out UDP_Sender_Interface;
      Config :        Configuration_Ptr;
      Socket :        Socket_Type);
      -- Initializes a network interface and does any necessary prep work. It
      -- MUST be called before calling any other method

   procedure Start (This : in out UDP_Sender_Interface);
   -- Starts the interface

   procedure Shutdown (This : in out UDP_Sender_Interface);
   -- Cleanly shuts down the interface

   function Get_Packet_Queue_Ptr
     (This : in out UDP_Sender_Interface)
      return DNS_Raw_Packet_Queue_Ptr;
   -- Returns a pointer to the packet queue

end DNSCatcher.Network.UDP.Sender;
