with Ada.Unchecked_Conversion;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Hash;

package body DNS_Transaction_Manager is
   -- Conversion functions
   function SEA_To_DNS_Packet_Header is new Ada.Unchecked_Conversion
     (Source => Stream_Element_Array, Target => Raw_Dns_Packets.Dns_Packet_Header);

   -- Handle the map for tracking transactions to/from source
   task body DNS_Transaction_Manager_Task is
      Outbound_Packet_Queue : Raw_DNS_Packets.Raw_DNS_Packet_Queue_Ptr;
      Transaction           : DNS_Transaction;
      Packet_Header         : Raw_DNS_Packets.DNS_Packet_Header;
      Hashmap_Key           : IP_Transaction_Key;
      Hashmap_Cursor        : DNS_Transaction_Maps.Cursor;
      Outbound_Packet       : Raw_DNS_Packet;
      Transaction_Hashmap   : DNS_Transaction_Maps.Map;
   begin
      loop
         select
            accept Set_Packet_Queue (Queue : in Raw_DNS_Packet_Queue_Ptr) do
               Outbound_Packet_Queue := Queue;
            end Set_Packet_Queue;
         or
            accept From_Client_Resolver_Packet (Packet : Raw_DNS_Packet) do
               Put_Line ("HERE!!!");
               New_Line;

               Packet_Header := SEA_To_Dns_Packet_Header (Packet.Raw_Data.all);
               Put ("  DNS Transaction ID: ");
               Put (Standard_Output, Integer (Packet_Header.Identifier), Base => 16);
               New_Line;

               Hashmap_Key :=
                 IP_Transaction_Key
                   (Packet.To_Address & Packet.To_Port'Image & Packet_Header.Identifier'Image);

               Put_Line (To_String (Hashmap_Key));
               -- Create the key if necessary
               Hashmap_Cursor := Transaction_Hashmap.Find (Hashmap_Key);

               if Hashmap_Cursor = DNS_Transaction_Maps.No_Element then
                  Transaction.Client_Resolver_Address := Packet.From_Address;
                  Transaction.Client_Resolver_Port    := Packet.From_Port;
                  Transaction.Server_Resolver_Address := Packet.To_Address;
                  Transaction.Server_Resolver_Port    := Packet.To_Port;
                  Transaction.DNS_Transaction_Id      := Packet_Header.Identifier;
                  Transaction_Hashmap.Insert (Hashmap_Key, Transaction);
               end if;

               -- Now append to the vector
               -- FIXME: This likely needs to be a ordered hashmap ...
               Transaction_Hashmap.Reference (Hashmap_Key).From_Client_Resolver_Packets.Append
                 (Packet);

               Put_Line
                 ("Inbound has" &
                  Transaction_Hashmap.Reference (Hashmap_Key).From_Client_Resolver_Packets.Length'
                    Image);
               Put_Line
                 ("Server Resolver" &
                  Transaction_Hashmap.Reference (Hashmap_Key).From_Upstream_Resolver_Packets
                    .Length'
                    Image);

               -- Rewrite the DNS Packet and send it on it's way
               Outbound_Packet              := Packet;
               Outbound_Packet.Raw_Data.all := Packet.Raw_Data.all;
               Outbound_Packet_Queue.Put (Outbound_Packet);
            end From_Client_Resolver_Packet;
         or
            accept From_Upstream_Resolver_Packet (Packet : Raw_DNS_Packet) do
               Put_Line ("HERE2!!!");
               New_Line;

               Packet_Header := SEA_To_Dns_Packet_Header (Packet.Raw_Data.all);
               Put ("  DNS Transaction ID: ");
               Put (Standard_Output, Integer (Packet_Header.Identifier), Base => 16);
               New_Line;

               Hashmap_Key :=
                 IP_Transaction_Key
                   (Packet.From_Address & Packet.From_Port'Image & Packet_Header.Identifier'Image);

               Put_Line (To_String (Hashmap_Key));
               -- Create the key if necessary
               Hashmap_Cursor := Transaction_Hashmap.Find (Hashmap_Key);

               if Hashmap_Cursor = DNS_Transaction_Maps.No_Element then
                  Transaction.Client_Resolver_Address := Packet.To_Address;
                  Transaction.Client_Resolver_Port    := Packet.To_Port;
                  Transaction.Server_Resolver_Address := Packet.From_Address;
                  Transaction.Server_Resolver_Port    := Packet.From_Port;
                  Transaction.DNS_Transaction_Id      := Packet_Header.Identifier;
                  Transaction_Hashmap.Insert (Hashmap_Key, Transaction);
               end if;

               -- Now append to the vector
               -- FIXME: This likely needs to be a ordered hashmap ...
               Transaction := Transaction_Hashmap.Reference (Hashmap_Key);
               Transaction_Hashmap.Reference (Hashmap_Key).From_Upstream_Resolver_Packets.Append
                 (Packet);

               Put_Line
                 ("To Upstream has" &
                  Transaction_Hashmap.Reference (Hashmap_Key).From_Client_Resolver_Packets.Length'
                    Image);
               Put_Line
                 ("Server Resolver" &
                  Transaction_Hashmap.Reference (Hashmap_Key).From_Client_Resolver_Packets.Length'
                    Image);

               -- Flip the packet around so it goes to the right place
               Outbound_Packet              := Packet;
               Outbound_Packet.To_Address   := Transaction.Client_Resolver_Address;
               Outbound_Packet.To_Port      := Transaction.Client_Resolver_Port;
               Outbound_Packet.Raw_Data.all := Packet.Raw_Data.all;
               Outbound_Packet_Queue.Put (Outbound_Packet);
            end From_Upstream_Resolver_Packet;
         or
            accept Shutdown_Task do
               null;
            end Shutdown_Task;
         or
            terminate;
         end select;
      end loop;
   end DNS_Transaction_Manager_Task;

   function IP_Transaction_Key_HashID (id : IP_Transaction_Key) return Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (id));
   end IP_Transaction_Key_HashID;
end DNS_Transaction_Manager;
