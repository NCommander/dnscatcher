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

-- @summary
--
-- Implements the receiving front-end for UDP packets sent to the DNSCatcher
-- server
--
-- @description
--
-- This package implements one half of the UDP implementation for DNSCatcher,
-- and defacto handles requests from client receivers.
--
package DNSCatcher.Network.UDP.Receiver is
   -- Packet receiving task
   task type Receive_Packet_Task is
      -- Initializes the receiver task
      --
      -- @value Config
      -- Pointer to configuration object
      --
      -- @value Socket
      -- GNAT.Sockets socket
      --
      -- @value Transaction_Manager
      -- Pointer to Transaction Manager Task
      entry Initialize
        (Config              : Configuration_Ptr;
         Socket              : Socket_Type;
         Transaction_Manager : DNS_Transaction_Manager_Task_Ptr);

         -- Start UDP receiver
      entry Start;

      -- Stop UDP receiver
      entry Stop;
   end Receive_Packet_Task;
   type Receive_Packet_Task_Ptr is access Receive_Packet_Task;

   --- Implementation of Network Receiver Interface
   --
   -- @value Config
   -- Holds pointer to configuration object
   --
   -- @value Receiver_Socket
   -- Holds GNAT.Socket pre-initialized
   --
   -- @value Transaction_Manager
   -- Pointer to Transaction Manager Task
   --
   -- @value Receiver_Task
   -- Receiver task held by the class object
   type UDP_Receiver_Interface is new DNSCatcher.Network
     .Receiver_Interface with
   record
      Config              : Configuration_Ptr;
      Receiver_Socket     : Socket_Type;
      Transaction_Manager : DNS_Transaction_Manager_Task_Ptr;
      Receiver_Task       : Receive_Packet_Task_Ptr;
   end record;
   type IPv4_UDP_Receiver_Interface_Ptr is access UDP_Receiver_Interface;

   -- Constructor for UDP receiver interface
   --
   -- @value This
   -- Class object
   --
   -- @value Config
   -- Holds pointer to configuration object
   --
   -- @value Transaction_Manager
   -- Pointer to Transaction Manager Task
   --
   -- @value Socket
   -- Holds GNAT.Socket pre-initialized
   --
   procedure Initialize
     (This                : in out UDP_Receiver_Interface;
      Config              :        Configuration_Ptr;
      Transaction_Manager :        DNS_Transaction_Manager_Task_Ptr;
      Socket              :        Socket_Type);

      -- Start the internal receiver task
      --
      -- @value This
      -- Class object
      --
   procedure Start (This : in out UDP_Receiver_Interface);

   -- Shutdown the internal receiver task
   --
   -- @value This
   -- Class object
   --
   procedure Shutdown (This : in out UDP_Receiver_Interface);
end DNSCatcher.Network.UDP.Receiver;
