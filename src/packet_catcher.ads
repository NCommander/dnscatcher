with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Streams; use Ada.Streams;

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

with Interfaces.C.Extensions;
use Interfaces.C.Extensions;

package Packet_Catcher is
   UDP_MAX_SIZE : constant Integer := 65535;
   UPSTREAM_DNS_SERVER_PORT : constant Integer := 53;

   procedure Run_Catcher;
   procedure Stop_Catcher;

   -- Protected Elements
   type Raw_DNS_Packet is record
      From_Address    : Unbounded_String;
      From_Port       : Integer;
      To_Address      : Unbounded_String;
      To_Port         : Integer;
      Raw_Data        : access Stream_Element_Array;
      Raw_Data_Length : Stream_Element_Offset;
   end record;

   package Stored_Packets_Vector is new Vectors (Natural, Raw_DNS_Packet);
   use Stored_Packets_Vector;

   protected type Raw_DNS_Packet_Queue is
      entry Put (Packet : in Raw_DNS_Packet);
      entry Get (Packet : out Raw_DNS_Packet);
   private
      Stored_Packets : Vector;
   end Raw_DNS_Packet_Queue;

   type DNS_Transaction is record
      Client_Resolver_Address      : Unbounded_String;
      Client_Resolver_Port         : Integer;
      Server_Resolver_Address      : Unbounded_String;
      Server_Resolver_Port         : Integer;
      DNS_Transaction_Id           : Unsigned_16;
      From_Client_Resolver_Packets : Vector;
      To_Upstream_Resolver_Packets : Vector;
   end record;

   type IP_Transaction_Key is new Unbounded_String;

   function IP_Transaction_Key_HashID
     (id : IP_Transaction_Key) return Hash_Type;

   package DNS_Transaction_Maps is new Hashed_Maps
     (Key_Type => IP_Transaction_Key, Element_Type => DNS_Transaction,
      Hash     => IP_Transaction_Key_HashID, Equivalent_Keys => "=");
   use DNS_Transaction_Maps;
   protected type DNS_Transaction_Manager is
      entry Append_Client_Resolver_Packet (Packet : Raw_DNS_Packet);
      entry Append_Upstream_Resolver_Packet (Packet : Raw_DNS_Packet);
   private
      Transaction_Hashmap : DNS_Transaction_Maps.Map;
   end DNS_Transaction_Manager;

end Packet_Catcher;
