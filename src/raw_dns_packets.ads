with System;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Streams;             use Ada.Streams;
with GNAT.Sockets;            use GNAT.Sockets;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Ada.Containers.Vectors;  use Ada.Containers;

with Utils;

package Raw_DNS_Packets is
   type Raw_DNS_Packet is record
      From_Address    : Unbounded_String;
      From_Port       : Port_Type;
      To_Address      : Unbounded_String;
      To_Port         : Port_Type;
      Raw_Data        : Utils.Stream_Element_Array_Ptr;
      Raw_Data_Length : Stream_Element_Offset;
   end record;

   package Stored_Packets_Vector is new Vectors (Natural, Raw_DNS_Packet);
   use Stored_Packets_Vector;

   task type Raw_DNS_Packet_Queue is
      entry Put (Packet : in Raw_DNS_Packet);
      entry Get (Packet : out Raw_DNS_Packet);
      entry Count (Count : out Integer);
      entry Empty;
   end Raw_DNS_Packet_Queue;
   type Raw_DNS_Packet_Queue_Ptr is access Raw_DNS_Packet_Queue;

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
      Question_Count          : Unsigned_2;
      Answer_Record_Count     : Unsigned_2;
      Authority_Record_Count  : Unsigned_2; -- NSCount
      Additional_Record_Count : Unsigned_2;
   end record;
   for DNS_Packet_Header'Bit_Order use System.High_Order_First;
   for DNS_Packet_Header'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (DNS_Packet_Header);

end Raw_DNS_Packets;
