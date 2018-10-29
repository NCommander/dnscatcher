with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Streams; use Ada.Streams;

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

with GNAT.Sockets; use GNAT.Sockets;

with Interfaces.C.Extensions; use Interfaces.C.Extensions;

package Packet_Catcher is
   UDP_MAX_SIZE             : constant Integer := 65535;
   UPSTREAM_DNS_SERVER_PORT : constant Integer := 53;

   procedure Run_Catcher;
   procedure Stop_Catcher;

   -- Protected Elements
   type Stream_Element_Array_Ptr is access Stream_Element_Array;

   type Raw_DNS_Packet is record
      From_Address    : Unbounded_String;
      From_Port       : Port_Type;
      To_Address      : Unbounded_String;
      To_Port         : Port_Type;
      Raw_Data        : Stream_Element_Array_Ptr;
      Raw_Data_Length : Stream_Element_Offset;
   end record;

   package Stored_Packets_Vector is new Vectors (Natural, Raw_DNS_Packet);
   use Stored_Packets_Vector;

   task type Raw_DNS_Packet_Queue is
      entry Put (Packet : in Raw_DNS_Packet);
      entry Get (Packet : out Raw_DNS_Packet);
      entry Count(Count : out Integer);
      entry Shutdown_Task;
   end Raw_DNS_Packet_Queue;

   type DNS_Transaction is record
      Client_Resolver_Address        : Unbounded_String;
      Client_Resolver_Port           : Port_Type;
      Server_Resolver_Address        : Unbounded_String;
      Server_Resolver_Port           : Port_Type;
      DNS_Transaction_Id             : Unsigned_16;
      From_Client_Resolver_Packets   : Vector;
      From_Upstream_Resolver_Packets : Vector;
   end record;

   type IP_Transaction_Key is new Unbounded_String;

   function IP_Transaction_Key_HashID
     (id : IP_Transaction_Key) return Hash_Type;

   package DNS_Transaction_Maps is new Hashed_Maps
     (Key_Type => IP_Transaction_Key, Element_Type => DNS_Transaction,
      Hash     => IP_Transaction_Key_HashID, Equivalent_Keys => "=");
   use DNS_Transaction_Maps;

   task type DNS_Transaction_Manager is
      entry From_Client_Resolver_Packet (Packet : Raw_DNS_Packet);
      entry From_Upstream_Resolver_Packet (Packet : Raw_DNS_Packet);
      entry Shutdown_Task;
   end DNS_Transaction_Manager;

end Packet_Catcher;
