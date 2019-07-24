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

with DNSCatcher.DNS.Processor.Packet; use DNSCatcher.DNS.Processor.Packet;

-- @summary
-- Utility functions used by the RData processor to derieve correct information
-- seperate from the main utilities class as they're unused anywhere else
--
package DNSCatcher.DNS.Processor.RData.Utils is
   -- Helper function to help decode IPv4 addresses into actual numeric
   -- representation. Required due to bite ordering issues within Ada.
   --
   -- See comment in DNSCatcher.DNS and DNSCatcher.Utils on this.
   --
   -- @value Parsed_RR
   -- A Parsed Resource Record, should be of the A type
   --
   -- @returns
   -- An IPv4 address encoded as an unbound string
   --
   function Decode_DNS_IPv4_Address
     (Parsed_RR : Parsed_DNS_Resource_Record)
      return Unbounded_String;

end DNSCatcher.DNS.Processor.RData.Utils;
