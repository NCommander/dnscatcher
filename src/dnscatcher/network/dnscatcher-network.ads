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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets;          use GNAT.Sockets;

with DNSCatcher.Config; use DNSCatcher.Config;
with DNSCatcher.DNS;    use DNSCatcher.DNS;

with DNSCatcher.DNS.Transaction_Manager;
use DNSCatcher.DNS.Transaction_Manager;

-- @summary
-- Abstraction and implementation of DNSCatcher's network protocols, including
-- UDP, TCP, DNS-over-TLS, and DNS-over-HTTPS
--
-- @description
-- As defined, the DNS protocol can be spoken over quite a few mediums, and
-- over IPv4 and IPv6. In general, there are two wire formats for DNS, UDP
-- and TCP, which only differ by sending a prefix length in the case of TCP.
--
-- As such, to seemlessly support DNS over various protocols, the Network
-- package provides a simple abstraction layer that handles most of the
-- nitty gritty of setting up and configuring the network for DNS traffic
--
package DNSCatcher.Network is

   -- Receiver Interface
   type Receiver_Interface is abstract tagged null record;

   -- Initiaizes the reader interface of a given connection method
   --
   -- @value This
   -- Class object
   --
   -- @value Config
   -- Configuration pointer
   --
   -- @value Transaction_Manager
   -- Transaction Manager instance
   --
   -- @value Socket
   -- Initialized socket from GNAT.Sockets
   procedure Initialize
     (This                : in out Receiver_Interface;
      Config              :        Configuration_Ptr;
      Transaction_Manager :        DNS_Transaction_Manager_Task_Ptr;
      Socket              :        Socket_Type) is abstract;

      -- Starts a given receiver interface
      --
      -- @value This
      -- Class object
      --
   procedure Start (This : in out Receiver_Interface) is abstract;

   -- Cleanly shuts down a receiver interface
   --
   -- @value This
   -- Class object
   --
   procedure Shutdown (This : in out Receiver_Interface) is abstract;

   -- Sender Interface
   type Sender_Interface is abstract tagged null record;

   -- Initiaizes the reader interface of a given connection method
   --
   -- @value This
   -- Class object
   --
   -- @value Config
   -- Configuration pointer
   --
   -- @value Socket
   -- Initialized socket from GNAT.Sockets
   --
   procedure Initialize
     (This   : in out Sender_Interface;
      Config :        Configuration_Ptr;
      Socket :        Socket_Type) is abstract;

      -- Starts the interface
      --
      -- @value This
      -- Class object
   procedure Start (This : in out Sender_Interface) is abstract;

   -- Stops the interface
   --
   -- @value This
   -- Class object
   procedure Shutdown (This : in out Sender_Interface) is abstract;
end DNSCatcher.Network;
