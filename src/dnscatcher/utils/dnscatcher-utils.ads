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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;

with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with DNSCatcher.Types; use DNSCatcher.Types;

-- @summary
-- Generic utility functions used throughout DNSCatcher are collected here.
--
-- @description
-- Some functionality is either very generic (such as ntohs), or otherwise is
-- a standalone subprogram that is used throughout the code, and this package
-- provides a home and organization pointer for them
--
-- Wrapper functions are provided for network byte order conversion as in
-- certain places, the values must be flipped on the fly, and GNAT.Sockets
-- doesn't provide an easy interface for this
--
package DNSCatcher.Utils is
   -- Network Byteorder to Host Short
   --
   -- @value Network_Short
   -- An unsigned 16-bit integer
   --
   -- @returns
   -- Local machine byte order 16-bit integer
   --
   function Ntohs
     (Network_Short : Unsigned_16)
      return Unsigned_16;

   -- Network Byteorder to Host Long
   --
   -- @value Network_Long
   -- An unsigned 32-bit integer
   --
   -- @returns
   -- Local machine byte order 32-bit integer
   --
   function Ntohl
     (Network_Long : Unsigned_32)
      return Unsigned_32;

   -- Host Short to Network Byteorder
   --
   -- @value Host_Short
   -- An unsigned 16-bit integer
   --
   -- @returns
   -- Network byte order 16-bit integer
   --
   function Htons
     (Host_Short : Unsigned_16)
      return Unsigned_16;

   -- Host Long to Network Byteorder
   --
   -- @value Host_Long
   -- An unsigned 32-bit integer
   --
   -- @returns
   -- Network byte order 32-bit integer
   --
   function Htonl
     (Host_Long : Unsigned_32)
      return Unsigned_32;

   -- Reads a 16-bit integer from a network stream and converts it to an
   -- Unsigned_16 for further evaluation
   --
   -- @value Raw_Data
   -- Pointer to a Stream_Element Array
   --
   -- @value Offset
   -- Location which to read the uint16 from
   --
   -- @returns
   -- Unsigned_16
   --
   function Read_Unsigned_16
     (Raw_Data :        Stream_Element_Array_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Unsigned_16;

   -- Reads a 32-bit integer from a network stream and converts it to an
   -- Unsigned_32 for further evaluation
   --
   -- @value Raw_Data
   -- Pointer to a Stream_Element Array
   --
   -- @value Offset
   -- Location which to read the uint32 from
   --
   -- @returns
   -- Unsigned_32
   --
   function Read_Unsigned_32
     (Raw_Data :        Stream_Element_Array_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Unsigned_32;

   -- IP_Addr_Family
   --
   -- This is a wrapper for Inet_Ntop that abstracts the values of AF_INET and
   -- AF_INET6 from the C preprocessor; allowing it to work across platforms
   -- without concern for varying preprocessor types
   --
   type IP_Addr_Family is
     (IPv4, -- IPv4 input address
      IPv6 -- IPv6 input address
      );
   for IP_Addr_Family use (IPv4 => 1, IPv6 => 2);

   -- Converts binary IPs to text form
   --
   -- @value Family
   -- IP_Addr_Family on if we're reading an IPv4 or IPv6 address
   --
   -- @value Raw_Data
   -- The raw data stored in Unbounded_String format
   --
   -- @returns
   -- Unbounded String with the IP address formatted in conventional style by
   -- the underlying inet_ntop() C function
   --
   function Inet_Ntop
     (Family   : IP_Addr_Family;
      Raw_Data : Unbounded_String)
      return Unbounded_String;

   -- Deallocators

   -- Helper function to free Stream_Element_Arrays
   procedure Free_Stream_Element_Array_Ptr is new Ada.Unchecked_Deallocation
     (Object => Stream_Element_Array, Name => Stream_Element_Array_Ptr);
end DNSCatcher.Utils;
