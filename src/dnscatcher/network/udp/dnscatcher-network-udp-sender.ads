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

-- @summary
-- Implements the Sender interface queue for UDP messages
--
-- @description
-- The outbound packet queue is used to handle all requests either being sent
-- to a client, or sending requests to upstream servers for results
--
package DNSCatcher.Network.UDP.Sender is
   -- Send queue management task
   task type Send_Packet_Task is
      -- Initializes the sender queue
      --
      -- @value Socket
      -- GNAT.Sockets Socket_Type
      --
      -- @value Packet_Queue
      -- Raw package queue to use with the sender module
      entry Initialize
        (Socket       : Socket_Type;
         Packet_Queue : DNS_Raw_Packet_Queue_Ptr);

         -- Starts the sender task
      entry Start;

      -- Stops the sender task
      entry Stop;
   end Send_Packet_Task;
   type Send_Packet_Task_Ptr is access Send_Packet_Task;

   -- UDP Sender interface used to queue outbound packets
   --
   -- @value Config
   -- Configuration Pointer
   --
   -- @value Sender_Socket
   -- Socket for the UDP port
   --
   -- @value Sender_Task
   -- Internal pointer to the Sender task
   --
   -- @value Packet_Queue
   -- Internal queue for packets to be sent
   --
   type UDP_Sender_Interface is new DNSCatcher.Network.Sender_Interface with
   record
      Config        : Configuration_Ptr;
      Sender_Socket : Socket_Type;
      Sender_Task   : Send_Packet_Task_Ptr;
      Packet_Queue  : DNS_Raw_Packet_Queue_Ptr;
   end record;
   type IPv4_UDP_Receiver_Interface_Ptr is access UDP_Sender_Interface;

   -- Initializes the sender class interface
   --
   -- @value This
   -- Class object
   --
   -- @value Config
   -- Pointer to the configuration object
   --
   -- @value Socket
   -- GNAT.Socket to use for UDP connections
   procedure Initialize
     (This   : in out UDP_Sender_Interface;
      Config :        Configuration_Ptr;
      Socket :        Socket_Type);

      -- Starts the interface
      --
      -- @value This
      -- Class Object
   procedure Start (This : in out UDP_Sender_Interface);

   -- Cleanly shuts down the interface
   --
   -- @value This
   -- Class Object
   procedure Shutdown (This : in out UDP_Sender_Interface);

   -- Returns the internal packet queue for this interface
   --
   -- @value This
   -- Clas object
   --
   -- @returns
   -- Pointer to the packet queue
   --
   function Get_Packet_Queue_Ptr
     (This : in out UDP_Sender_Interface)
      return DNS_Raw_Packet_Queue_Ptr;

end DNSCatcher.Network.UDP.Sender;
