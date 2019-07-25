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

with DNSCatcher.DNS.Processor.Packet; use DNSCatcher.DNS.Processor.Packet;
with DNSCatcher.Config;               use DNSCatcher.Config;
with DNSCatcher.DNS;                  use DNSCatcher.DNS;
with DNSCatcher.Types;                use DNSCatcher.Types;

-- Creates and handles making DNS client requests
package DNSCatcher.DNS.Client is
   -- DNS Client object
   type Client is tagged private;
   type Client_Access is access all Client'Class;

   -- Populates the header field for a given DNS request
   --
   -- @value This the DNS Client object
   procedure Create_Header (This : in out Client);

   --!pp off
   -- Adds a question to the DNS request
   --
   -- @value This the DNS Client object
   -- @value QName DNS name to query
   -- @value QType The RRType to query
   -- @value QClass DNS Class to query
   --!pp on
   procedure Add_Query
     (This   : in out Client;
      QName  :        Unbounded_String;
      QType  :        RR_Types;
      QClass :        Classes);

   --!pp off
   -- Creates a DNS packet out of a client request (should be private)
   --
   -- @value This the DNS Client object
   -- @value Config to the DNSCatcher configuration
   --!pp on
   function Create_Packet
     (This   : in out Client;
      Config :        Configuration)
      return Raw_Packet_Record_Ptr;

private
   type Client is tagged record
      Header    : DNS_Packet_Header;
      Questions : Question_Vector.Vector;
   end record;

end DNSCatcher.DNS.Client;
