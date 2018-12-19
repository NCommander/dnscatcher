with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

with GNAT.Sockets;            use GNAT.Sockets;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with DNS_Core_Constructs.Raw_Packet_Records; use DNS_Core_Constructs.Raw_Packet_Records;

package DNS_Transaction_Manager is
   package Stored_Packets_Vector is new Vectors (Natural, Raw_Packet_Record_Ptr);

   type DNS_Transaction is record
      Client_Resolver_Address       : Unbounded_String;
      Client_Resolver_Port          : Port_Type;
      Server_Resolver_Address       : Unbounded_String;
      Server_Resolver_Port          : Port_Type;
      DNS_Transaction_Id            : Unsigned_16;
      Local_Request                 : Boolean;
      From_Client_Resolver_Packet   : Raw_Packet_Record_Ptr;
      From_Upstream_Resolver_Packet : Raw_Packet_Record_Ptr;
   end record;
   type DNS_Transaction_Ptr is access DNS_Transaction;

   type IP_Transaction_Key is new Unbounded_String;

   function IP_Transaction_Key_HashID (id : IP_Transaction_Key) return Hash_Type;

   package DNS_Transaction_Maps is new Hashed_Maps (Key_Type => IP_Transaction_Key,
      Element_Type => DNS_Transaction_Ptr, Hash => IP_Transaction_Key_HashID,
      Equivalent_Keys                                        => "=");
   use DNS_Transaction_Maps;

   task type DNS_Transaction_Manager_Task is
      entry Start;
      entry Set_Packet_Queue (Queue : DNS_Raw_Packet_Queue_Ptr);
      entry From_Client_Resolver_Packet (Packet : Raw_Packet_Record_Ptr; Local: Boolean);
      entry From_Upstream_Resolver_Packet (Packet : Raw_Packet_Record_Ptr);
      entry Stop;
   end DNS_Transaction_Manager_Task;

   type DNS_Transaction_Manager_Task_Ptr is access DNS_Transaction_Manager_Task;
end DNS_Transaction_Manager;
