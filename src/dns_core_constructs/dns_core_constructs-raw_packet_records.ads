with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with GNAT.Sockets;           use GNAT.Sockets;
with Ada.Containers.Vectors; use Ada.Containers;
with DNS_Core_Constructs;    use DNS_Core_Constructs;

package DNS_Core_Constructs.Raw_Packet_Records is
   type Raw_Packet_Record is record
      From_Address    : Unbounded_String;
      From_Port       : Port_Type;
      To_Address      : Unbounded_String;
      To_Port         : Port_Type;
      Raw_Data        : Raw_DNS_Packet;
      Raw_Data_Length : Stream_Element_Offset;
   end record;
   type Raw_Packet_Record_Ptr is access Raw_Packet_Record;

   package Stored_Packets_Vector is new Vectors (Natural, Raw_Packet_Record);
   use Stored_Packets_Vector;

   protected type Raw_Packet_Record_Queue is
      entry Put (Packet : in Raw_Packet_Record);
      entry Get (Packet : out Raw_Packet_Record);
      entry Count (Count : out Integer);
      entry Empty;
   private
      Stored_Packets : Vector;
      Packet_Count   : Integer := 0;
   end Raw_Packet_Record_Queue;
   type DNS_Raw_Packet_Queue_Ptr is access Raw_Packet_Record_Queue;

end DNS_Core_Constructs.Raw_Packet_Records;
