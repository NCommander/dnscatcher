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
-- Implements the DNSCatcher internal DNS Server
--
-- @description
-- To support most of it's operations, DNSCatcher needs an intergrated DNS
-- server that can support unusual operations and dynamic record updating.
-- While it may have been possible to build out of an existing DNS server
-- product, the forks and effort required was deemed too high, hence DNSCatcher
-- implements it's own internal DNS server to work with cross-check and
-- validation
--
   package DNSCatcher.DNS.Server is
   UDP_MAX_SIZE : constant Integer := 65535;

   -- Starts the internal DNS Server
   procedure Start_Server;

   -- Stops the internal server
   procedure Stop_Server;
end DNSCatcher.DNS.Server;
