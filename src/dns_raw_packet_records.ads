with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Streams;             use Ada.Streams;
with GNAT.Sockets;            use GNAT.Sockets;
with Ada.Containers.Vectors;  use Ada.Containers;
with Raw_DNS_Packets;         use Raw_DNS_Packets;
with Utils;

package DNS_Raw_Packet_Records is
   type DNS_Raw_Packet_Record is record
      From_Address    : Unbounded_String;
      From_Port       : Port_Type;
      To_Address      : Unbounded_String;
      To_Port         : Port_Type;
      Raw_Data        : Raw_DNS_Packet;
      Raw_Data_Length : Stream_Element_Offset;
   end record;

   package Stored_Packets_Vector is new Vectors (Natural, DNS_Raw_Packet_Record);
   use Stored_Packets_Vector;

   task type DNS_Raw_Packet_Record_Queue is
      entry Put (Packet : in DNS_Raw_Packet_Record);
      entry Get (Packet : out DNS_Raw_Packet_Record);
      entry Count (Count : out Integer);
      entry Empty;
   end DNS_Raw_Packet_Record_Queue;
   type DNS_Raw_Packet_Queue_Ptr is access DNS_Raw_Packet_Record_Queue;
end DNS_Raw_Packet_Records;
