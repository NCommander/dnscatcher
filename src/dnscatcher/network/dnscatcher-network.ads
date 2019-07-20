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


with GNAT.Sockets; use GNAT.Sockets;

with DNSCatcher.Config;       use DNSCatcher.Config;
with DNS_Transaction_Manager; use DNS_Transaction_Manager;

package DNSCatcher.Network is

   -- Receiver Interface
   type Receiver_Interface is abstract tagged null record;

   procedure Initialize
     (This                : in out Receiver_Interface;
      Config              :        Configuration_Ptr;
      Transaction_Manager :        DNS_Transaction_Manager_Task_Ptr;
      Socket              :        Socket_Type) is abstract;
   -- Initializes a network interface and does any necessary prep work. It
   -- MUST be called before calling any other method

   procedure Start (This : in out Receiver_Interface) is abstract;
   -- Starts the interface

   procedure Shutdown (This : in out Receiver_Interface) is abstract;
   -- Cleanly shuts down the interface


   -- Sender Interface
   type Sender_Interface is abstract tagged null record;

   procedure Initialize
     (This   : in out Sender_Interface;
      Config :        Configuration_Ptr;
      Socket :        Socket_Type) is abstract;
   -- Initializes a network interface and does any necessary prep work. It
   -- MUST be called before calling any other method

   procedure Start (This : in out Sender_Interface) is abstract;
   -- Starts the interface

   procedure Shutdown (This : in out Sender_Interface) is abstract;
   -- Cleanly shuts down the interface

end DNSCatcher.Network;
