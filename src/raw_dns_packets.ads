with System;
with Ada.Streams;             use Ada.Streams;
with Ada.Unchecked_Conversion;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with Utils; use Utils;

package Raw_DNS_Packets is
   type DNS_Packet_Header is record
      Identifier              : Unsigned_16;
      Query_Response_Flag     : Boolean;
      Opcode                  : Unsigned_4;
      Authoritative_Answer    : Boolean;
      Truncated               : Boolean; -- Authoritive Answer
      Recursion_Desired       : Boolean; --
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

   -- I don't know if this is spec, or a bug with GNAT, but 'Size
   -- returns the wrong value and I get 128 which means I need to
   -- calculate this like this which is kinda bullshit ...
   DNS_PACKET_HEADER_SIZE : constant Stream_Element_Offset := 12;

   -- Represents everything past the data
   type Raw_DNS_Packet_Data is new Stream_Element_Array_Ptr;

   type Raw_DNS_Packet is record
      Header : DNS_Packet_Header;
      Data   : Raw_DNS_Packet_Data;
   end record;
   for Raw_DNS_Packet'Bit_Order use System.High_Order_First;
   for Raw_DNS_Packet'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (Raw_DNS_Packet);

   procedure Free_Raw_DNS_Packet(Packet: in out Raw_DNS_Packet);

   -- DNS packet elements can't be represented in binary form
   -- due to the fact that they're variable legnth with a non-standard
   -- encoding, so we'll have to just save an array and turn it into
   -- something parsable later

   type Raw_DNS_Packet_Question is record
      QName : Stream_Element_Array_Ptr;
      QType : Unsigned_16;
      QClass : Unsigned_16;
   end record;
   for Raw_DNS_Packet_Question'Bit_Order use System.High_Order_First;
   for Raw_DNS_Packet_Question'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (Raw_DNS_Packet_Question);

   -- For the remaining sections, they share a common data format
   type Raw_DNS_Resource_Record is record
      Name: Stream_Element_Array_Ptr; -- Variable Length
      RR_Type: Unsigned_16;
      DNS_Class: Unsigned_16;
      TTL: Unsigned_32;
      RData_Length: Unsigned_16;
      RData: Stream_Element_Array_Ptr;
   end record;

   type Raw_DNS_Packet_Answer is new Raw_DNS_Resource_Record;
   type Raw_DNS_Packet_Authority is new Raw_DNS_Resource_Record;
   type Raw_DNS_Packet_Additional is new Raw_DNS_Resource_Record;

   subtype SEA_DNS_Packet_Header is Stream_Element_Array(1..DNS_PACKET_HEADER_SIZE); -- 12 is the packed sized
   function SEA_To_DNS_Packet_Header is new Ada.Unchecked_Conversion
     (Source => Stream_Element_Array, Target => DNS_Packet_Header);
   function DNS_Packet_Header_To_SEA is new Ada.Unchecked_Conversion
     (Source => DNS_Packet_Header, Target => SEA_DNS_Packet_Header);

end Raw_DNS_Packets;
