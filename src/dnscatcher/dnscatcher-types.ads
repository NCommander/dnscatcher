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
with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;

with GNAT.Sockets; use GNAT.Sockets;

with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with System;

package DNSCatcher.Types is
   type DNS_Packet_Header is record
      Identifier              : Unsigned_16;
      Query_Response_Flag     : Boolean;
      Opcode                  : Unsigned_4;
      Authoritative_Answer    : Boolean;
      Truncated               : Boolean; -- Authoritive Answer
      Recursion_Desired       : Boolean;
      Recursion_Available     : Boolean;
      Zero                    : Boolean;
      Authenticated_Data      : Boolean;
      Checking_Disabled       : Boolean;
      Response_Code           : Unsigned_4;
      Question_Count          : Unsigned_16;
      Answer_Record_Count     : Unsigned_16;
      Authority_Record_Count  : Unsigned_16; -- NSCount
      Additional_Record_Count : Unsigned_16;
   end record;
   for DNS_Packet_Header'Bit_Order use System.High_Order_First;
   for DNS_Packet_Header'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (DNS_Packet_Header);
   type DNS_Packet_Header_Ptr is access all DNS_Packet_Header;

   -- I don't know if this is spec, or a bug with GNAT, but 'Size returns the
   -- wrong value and I get 128 which means I need to calculate this like this
   -- which is kinda bullshit ...
   DNS_PACKET_HEADER_SIZE : constant Stream_Element_Offset := 12;

   -- Represents everything past the data
   type Stream_Element_Array_Ptr is access all Stream_Element_Array;
   subtype Raw_DNS_Packet_Data is Stream_Element_Array_Ptr;

   type Raw_DNS_Packet is record
      Header : DNS_Packet_Header;
      Data   : Raw_DNS_Packet_Data;
   end record;
   for Raw_DNS_Packet'Bit_Order use System.High_Order_First;
   for Raw_DNS_Packet'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (Raw_DNS_Packet);
   type Raw_DNS_Packet_Ptr is access Raw_DNS_Packet;

   procedure Free_Raw_DNS_Packet (Packet : in out Raw_DNS_Packet);

   -- DNS packet elements can't be represented in binary form due to the fact
   -- that they're variable legnth with a non-standard encoding, so we'll have
   -- to just save an array and turn it into something parsable later

   type Raw_DNS_Packet_Question is record
      QName  : Stream_Element_Array_Ptr;
      QType  : Unsigned_16;
      QClass : Unsigned_16;
   end record;
   for Raw_DNS_Packet_Question'Bit_Order use System.High_Order_First;
   for Raw_DNS_Packet_Question'Scalar_Storage_Order use System
     .High_Order_First;
   pragma Pack (Raw_DNS_Packet_Question);

   -- For the remaining sections, they share a common data format
   type Raw_DNS_Resource_Record is record
      Name         : Stream_Element_Array_Ptr; -- Variable Length
      RR_Type      : Unsigned_16;
      DNS_Class    : Unsigned_16;
      TTL          : Unsigned_32;
      RData_Length : Unsigned_16;
      RData        : Stream_Element_Array_Ptr;
   end record;

   type Raw_DNS_Packet_Answer is new Raw_DNS_Resource_Record;
   type Raw_DNS_Packet_Authority is new Raw_DNS_Resource_Record;
   type Raw_DNS_Packet_Additional is new Raw_DNS_Resource_Record;

   subtype SEA_DNS_Packet_Header is
     Stream_Element_Array
       (1 .. DNS_PACKET_HEADER_SIZE); -- 12 is the packed sized

   function SEA_To_DNS_Packet_Header is new Ada.Unchecked_Conversion
     (Source => Stream_Element_Array, Target => DNS_Packet_Header);
   function DNS_Packet_Header_To_SEA is new Ada.Unchecked_Conversion
     (Source => DNS_Packet_Header, Target => SEA_DNS_Packet_Header);

   type Raw_Packet_Record is record
      From_Address    : Unbounded_String;
      From_Port       : Port_Type;
      To_Address      : Unbounded_String;
      To_Port         : Port_Type;
      Raw_Data        : Raw_DNS_Packet;
      Raw_Data_Length : Stream_Element_Offset;
   end record;
   type Raw_Packet_Record_Ptr is access Raw_Packet_Record;

   procedure Free_Raw_Packet_Record_Ptr (Ptr : in out Raw_Packet_Record_Ptr);
end DNSCatcher.Types;
