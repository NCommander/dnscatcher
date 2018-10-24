with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Streams;
use Ada.Streams;

with Ada.Containers.Vectors;
use Ada.Containers;

package Packet_Catcher is
   UDP_MAX_SIZE : constant Integer := 65535;
   procedure Run_Catcher;
   procedure Stop_Catcher;

   -- Protected Elements
   type Raw_DNS_Packet is record
      From_Address: Unbounded_String;
      To_Address: Unbounded_String;
      Raw_Data: access Stream_Element_Array;
      Raw_Data_Length: Stream_Element_Offset;
   end record;

   package  Stored_Packets_Vector is new Vectors(Natural, Raw_DNS_Packet);
   use Stored_Packets_Vector;

   protected type Raw_DNS_Packet_Queue is
      entry Put (Packet: in Raw_Dns_Packet);
      entry Get (Packet: out Raw_Dns_Packet);
   private
      Stored_Packets: Vector;
   end Raw_DNS_Packet_Queue;

end Packet_Catcher;
