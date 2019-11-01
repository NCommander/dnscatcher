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
with Ada.Streams;           use Ada.Streams;
with Ada.Unchecked_Conversion;

with GNAT.Sockets; use GNAT.Sockets;

with Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with System;

-- @summary
-- Global types used throughout the DNSCatcher library
--
-- @description
-- DNSCatcher has several types used to define items like DNS Packet Headers,
-- RData data and much more. For the moment to prevent circular dependencies
-- much of this information is stored here in a Types top level module. It is
-- intended to retire this as soon as realistically possible
--
package DNSCatcher.Types is

   -- Header field for DNS Packets, common to all DNS messages as described in
   -- RFC 1035 (and others). This field is marked big-endian (network order),
   -- as it's directly laid over the starting prefix bytes of all DNS packets.
   -- This causes issues with GDB on little endian platforms, but performs
   -- correctly when compiled in the test suite.
   --
   -- @field Identifier
   -- 16-bit field used to match multiple client requests and answers back to
   -- each other
   --
   -- @field Query_Response_Flag
   -- Identifies a given DNS packet as a client request (FALSE), or server
   -- response (TRUE). This flag is used with the Identifier to complete DNS
   -- transactions.
   --
   -- @field Opcode
   -- Identifies the DNS operation in action. The values for this field are
   -- managed by IANA.
   --
   -- @field Authoritative_Answer Determines if a given answer is authoritive
   -- and that a given server is authoritive for the answers given in the
   -- Authoritive Answer section. This is known as the AA-bit in most DNS
   -- documentation.
   --
   -- @field Truncated
   -- This field is only used in UDP transmissions. It is set if the packet is
   -- greater than 512 ocelets *or* if EDNS is in use, greater than the clients
   -- max packet size. If so, the answers are cut down to the point they are
   -- fit and Truncated is set. The client is expected to reconnect via TCP/IP
   -- to get the remainder of the data it needs.
   --
   -- @field Recursion_Desired
   -- Set by the client, the RD-bit asks the server to perform a recursive
   -- lookup on it's behalf. This flag, as suggested, is a suggestion and the
   -- server may ignore it (set by the RA bit below). RD is mirrored between
   -- client response, and server response.
   --
   -- @field Recursion_Available
   -- Only used in server responses, it denotes recursive lookup services are
   -- available. RA can be used to determine if a lookup was recursed if RD was
   -- set.
   --
   -- @field Zero
   -- Must be zero; this bit is reserved for future use.
   --
   -- @field Authenticated_Data
   -- Defines if the data returned from the server was authenticated via
   -- DNSSEC. AD is set regardless if EDNS DO=1 was sent (indicating the
   -- client wishes to get DNSSEC information).
   --
   -- @field Checking_Disabled
   -- Set by the client, CD determines if DNSSEC information should be checked
   -- at all This field primarily exists to allow retrieval of missigned DNSSEC
   -- records.
   --
   -- @field Response_Code
   -- Non-EDNS result of the client's last request. Response_Code is somewhat
   -- complicated as there is a set of Extended response codes that exceed the
   -- 4-bit limit of this field. In those cases, the remaining top bits are set
   -- in the EDNS packet so handling of this field requires knowing if EDNS is
   -- in use or not, and then combining it with the OPT data record to create
   -- the actual response code.
   --
   -- @field Question_Count
   -- Number of Questions held in this DNS Packet. Questions are sent from the
   -- client to the server specifying the name, class, and record type desired.
   -- See Raw_DNS_Question below for more information.
   --
   -- @field Answer_Record_Count
   -- Number of Answers Records returned in this DNS packet. Answers are
   -- responses directly relating to the question answered by the client.
   --
   -- @field Authority_Record_Count Number of Authority Records in this packet.
   -- Authority records are pointers and hints to where AA=1 information can be
   -- found. They are optional from recursive resolvers.
   --
   -- @field Additional_Record_Count Records that the server deems "useful"
   -- even though they don't directly answer the client's question. This is
   -- primary where EDNS OPT record lives as well as glue record information
   -- for IPv4/IPv6 lookups.
   --
   type DNS_Packet_Header is record
      Identifier              : Unsigned_16;
      Query_Response_Flag     : Boolean;
      Opcode                  : Unsigned_4;
      Authoritative_Answer    : Boolean;
      Truncated               : Boolean;
      Recursion_Desired       : Boolean;
      Recursion_Available     : Boolean;
      Zero                    : Boolean;
      Authenticated_Data      : Boolean;
      Checking_Disabled       : Boolean;
      Response_Code           : Unsigned_4;
      Question_Count          : Unsigned_16;
      Answer_Record_Count     : Unsigned_16;
      Authority_Record_Count  : Unsigned_16;
      Additional_Record_Count : Unsigned_16;
   end record;
   for DNS_Packet_Header'Bit_Order use System.High_Order_First;
   for DNS_Packet_Header'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (DNS_Packet_Header);
   type DNS_Packet_Header_Ptr is access all DNS_Packet_Header;

   -- Represents the fixed size of the header section of a DNS packet.
   --
   -- I don't know if this is spec, or a bug with GNAT, but 'Size returns the
   -- wrong value and I get 128 which means I need to calculate this like this
   -- which is kinda bullshit. This is likely due to the use of Pack confusing
   -- the compiler.
   DNS_PACKET_HEADER_SIZE : constant Stream_Element_Offset := 12;

   -- Data section of a DNS packet
   type Stream_Element_Array_Ptr is access all Stream_Element_Array;
   subtype Raw_DNS_Packet_Data_Ptr is Stream_Element_Array_Ptr;

   -- Represents the raw DNS packet.
   --
   -- DNS packets have a fixed size header, and variable sized data length
   -- which is handled in the message. When used over TCP/IP, packets start
   -- with a two bit length that defines the full size of the DNS packet. This
   -- implementation detail is handled transparently by the network transport
   -- layer as it needs to be read and calculated on the fly for each DNS
   -- packet.
   --
   -- This record is stored big endian, and is packed, and used to directly map
   -- incoming data by the network stack to DNS information.
   --
   -- @field Header
   -- Fixed sized DNS packet header; mapped to the DNS_Packet_Header type by an
   -- Unchecked_Conversion in the network handling stack.
   --
   -- @field Data
   -- Variable length data section of the packet. This is a subtype of
   -- Stream_Element_Array which is what is returned from Ada.Streams
   -- and GNAT.Sockets.
   --
   type Raw_DNS_Packet is record
      Header : DNS_Packet_Header;
      Data   : Raw_DNS_Packet_Data_Ptr;
   end record;
   for Raw_DNS_Packet'Bit_Order use System.High_Order_First;
   for Raw_DNS_Packet'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (Raw_DNS_Packet);
   type Raw_DNS_Packet_Ptr is access Raw_DNS_Packet;

   -- Extracts the DNS Packet Header from the start of a Stream_Element_Array
   --
   -- Used to substatiate the unchecked conversions to and from.
   --
   subtype SEA_DNS_Packet_Header is
     Stream_Element_Array (1 .. DNS_PACKET_HEADER_SIZE);

   -- Conversion functions

   -- Converts a Stream_Element_Array to a DNS Packet Header.
   --
   -- This is implemented as an unchecked conversion so responsibility is on
   -- the caller to ensure valid data exists within the SEA.
   --
   function SEA_To_DNS_Packet_Header is new Ada.Unchecked_Conversion
     (Source => Stream_Element_Array, Target => DNS_Packet_Header);

   -- Converts a DNS Packet_Header to Stream_Element_Array
   --
   -- This allows conversion of packed data from the record to wire format,
   -- used in sending DNS packets when operating as a DNS client.
   --
   function DNS_Packet_Header_To_SEA is new Ada.Unchecked_Conversion
     (Source => DNS_Packet_Header, Target => SEA_DNS_Packet_Header);

   -- Basic unit of a raw DNS packet before any processing is performed
   --
   -- The Raw_Packet_Unit is used by the network stack to represent where a
   -- packet is going and coming from. It also contains a From/To port for UDP
   -- packets as these need to be sent in two stages to respond back properly.
   --
   -- This record is also formed for outgoing responses as part of the
   -- DNS_Client operation.
   --
   -- This record is primarily used by the transaction manager to safely return
   -- the right packet to the right client.
   --
   -- @field From_Address
   -- Origin of this packet. For packets originating from this system, it can
   -- be left blank.
   --
   -- @field From_Port
   -- Represents the source port on the remote system. This is required to
   -- match and return UDP responses.
   --
   -- @field To_Address
   -- Destinaton of a packet. For packets arriving on this system, it may be
   -- left blank.
   --
   -- @field To_Port
   -- Destination port of the packet.
   --
   -- @field Raw_Data
   -- Raw data of a given packet field already memory mapped to DNS component
   -- parts.
   --
   -- @field Raw_Data_Length
   -- Full length of the raw data component
   --
   type Raw_Packet_Record is record
      From_Address    : Unbounded_String;
      From_Port       : Port_Type;
      To_Address      : Unbounded_String;
      To_Port         : Port_Type;
      Raw_Data        : Raw_DNS_Packet;
      Raw_Data_Length : Stream_Element_Offset;
   end record;
   type Raw_Packet_Record_Ptr is access Raw_Packet_Record;

   -- C version of Raw_Packet_Record
   type Raw_Packet_Record_C is record
      From_Address : Interfaces.C.char_array(1..41);
      From_Port : Interfaces.C.int;
      To_Address : Interfaces.C.char_array(1..41);
      To_Port : Interfaces.C.int;
      Raw_Data : Stream_Element_Array_Ptr;
      Raw_Data_Length : Interfaces.C.size_t;
   end record;
   type Raw_Packet_Record_C_Ptr is access Raw_Packet_Record_C;

   -- Deallocation function for Raw_DNS_Packet
   --
   -- Due to the fact that raw DNS packet data is returned as an access type
   -- this helper procedure is used to deallocate this record correctly.
   --
   -- @value Packet Raw_DNS_Packet to be freed
   --
   procedure Free_Raw_DNS_Packet (Packet : in out Raw_DNS_Packet);

   -- Deallocation function for Raw_Packet_Record
   --
   -- Handles properly deallocating Raw_Packet_Records due to the dynamic
   -- memory components stored in these records
   --
   -- @value Ptr
   -- The raw packet to be freed.
   --
   procedure Free_Raw_Packet_Record_Ptr (Ptr : in out Raw_Packet_Record_Ptr);

   -- Conversion function to C for Raw_Packet_Record
   function To_C(RPP : Raw_Packet_Record_Ptr) return Raw_Packet_Record_C_Ptr;
end DNSCatcher.Types;
