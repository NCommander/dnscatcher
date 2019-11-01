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

-- @summary
-- Ada module for handling Async IO network operations.
--
-- @description
-- This is a de-facto wrapper around libuv that handles all network
-- transactions in an asynchronous manner which network packets are
-- offloaded and onloaded.
--
-- This is to allow surpassing the c10k problem of handling multiple tasks.
-- Unfortunately there is no Ada native abstraction layer for libuv so this
-- is primarily implemented in C and punts code to and from Ada callbacks.

with DNSCatcher.Types; use DNSCatcher.Types;

with System;
with Interfaces.C;

package DNSCatcher.Network.ASync_IO is
   -- Task for containing libuv backend code
   task Async_IO_Task;

   -- Starts the main UV event loop
   procedure Start_UV_Event_Loop;

   procedure Handle_Inbound_Packet
     (Packet         : System.Address;
      C_Length       : Interfaces.C.size_t;
      Origin_Address : Interfaces.C.char_array;
      Origin_Port    : Interfaces.C.int);

      --!pp off
   pragma Export (Convention     => C,
                  Entity         => Handle_Inbound_Packet,
                  External_Name  => "dc_internal_handle_inbound_packet");
   --!pp on

   -- Copies a packet from the pending queue to UV for transmission
   function Spool_Packets_To_UV return Raw_Packet_Record_C_Ptr;

      --!pp off
   pragma Export (Convention    => C,
                  Entity        => Spool_Packets_To_UV,
                  External_Name => "dc_internal_spool_packet_to_uv");
   --!pp on

   procedure UV_SIGINT_Handler;
   pragma Import (C, UV_SIGINT_Handler, "dc_uv_sigint_handler");

end DNSCatcher.Network.ASync_IO;
