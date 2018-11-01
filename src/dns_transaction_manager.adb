with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Strings.Hash;
with DNS_Raw_Packet_Records; use DNS_Raw_Packet_Records;
with Raw_DNS_Packets;        use Raw_DNS_Packets;
with Packet_Parser;          use Packet_Parser;

with Utils;

package body DNS_Transaction_Manager is
   -- Handle the map for tracking transactions to/from source
   task body DNS_Transaction_Manager_Task is
      Outbound_Packet_Queue : DNS_Raw_Packet_Queue_Ptr;
      Hashmap_Key           : IP_Transaction_Key;
      Hashmap_Cursor        : DNS_Transaction_Maps.Cursor;
      Outbound_Packet       : DNS_Raw_Packet_Record;
      Transaction_Hashmap   : DNS_Transaction_Maps.Map;
      Transaction           : DNS_Transaction_Ptr;
      Running               : Boolean := False;
   begin
      loop
         Put_Line("Transaction Manager STOPPED");
         while Running = False loop
            Transaction := null;
            select
               accept Set_Packet_Queue (Queue : in DNS_Raw_Packet_Queue_Ptr) do
                  Outbound_Packet_Queue := Queue;
               end Set_Packet_Queue;
               accept Start do
                  if Outbound_Packet_Queue /= null then
                     Running := True;
                  end if;
               end Start;
            or
               terminate;
            end select;
         end loop;

         Put_Line("Transaction Manager STARTED");
         while Running loop
            select
               accept From_Client_Resolver_Packet (Packet : DNS_Raw_Packet_Record_Ptr) do

                  Put ("  Downstream DNS Transaction ID: ");
                  Put (Standard_Output, Integer (Packet.Raw_Data.Header.Identifier), Base => 16);
                  New_Line;

                  Hashmap_Key :=
                    IP_Transaction_Key
                      (Packet.To_Address & Packet.To_Port'Image &
                         Packet.Raw_Data.Header.Identifier'Image);

                  -- Create the key if necessary
                  Hashmap_Cursor := Transaction_Hashmap.Find (Hashmap_Key);

                  if Hashmap_Cursor = DNS_Transaction_Maps.No_Element then
                     Transaction := new DNS_Transaction;
                     Transaction.Client_Resolver_Address := Packet.From_Address;
                     Transaction.Client_Resolver_Port    := Packet.From_Port;
                     Transaction.Server_Resolver_Address := Packet.To_Address;
                     Transaction.Server_Resolver_Port    := Packet.To_Port;
                     Transaction.DNS_Transaction_Id      := Packet.Raw_Data.Header.Identifier;
                     Transaction_Hashmap.Insert (Hashmap_Key, Transaction);
                  end if;

                  -- Save the packet
                  Transaction                             := Transaction_Hashmap (Hashmap_Key);
                  Transaction.From_Client_Resolver_Packet := Packet;

                  -- Try to parse the packet
                  Packet_Parser.Packet_Parser(Packet);

                  -- Rewrite the DNS Packet and send it on it's way
                  Outbound_Packet_Queue.Put (Packet.all);
               exception
                  when Error : others =>
                     begin
                        Put (Standard_Error, "Transaction error: ");
                        Put_Line (Standard_Error, Exception_Information (Error));
                     end;
               end From_Client_Resolver_Packet;
            or
               accept From_Upstream_Resolver_Packet (Packet : DNS_Raw_Packet_Record_Ptr) do
                  Put ("  Upstream DNS Transaction ID: ");
                  Put (Standard_Output, Integer (Packet.Raw_Data.Header.Identifier), Base => 16);
                  New_Line;

                  Hashmap_Key :=
                    IP_Transaction_Key
                      (Packet.From_Address & Packet.From_Port'Image &
                         Packet.Raw_Data.Header.Identifier'Image);

                  -- Create the key if necessary
                  Hashmap_Cursor := Transaction_Hashmap.Find (Hashmap_Key);

                  if Hashmap_Cursor = DNS_Transaction_Maps.No_Element then
                     Transaction := new DNS_Transaction;
                     Transaction.Client_Resolver_Address := Packet.To_Address;
                     Transaction.Client_Resolver_Port    := Packet.To_Port;
                     Transaction.Server_Resolver_Address := Packet.From_Address;
                     Transaction.Server_Resolver_Port    := Packet.From_Port;
                     Transaction.DNS_Transaction_Id      := Packet.Raw_Data.Header.Identifier;
                     Transaction_Hashmap.Insert (Hashmap_Key, Transaction);
                  end if;

                  -- Save the packet
                  Transaction                               := Transaction_Hashmap (Hashmap_Key);
                  Transaction.From_Upstream_Resolver_Packet := Packet;

                  -- Only server packets should be trunciated if at all
                  if (Packet.Raw_Data.Header.Truncated) then
                     Put_Line("WARNING: Trunciated packet response!");
                  end if;

                  -- Try to parse the packet
                  Packet_Parser.Packet_Parser(Packet);

                  -- Flip the packet around so it goes to the right place
                  Outbound_Packet            := Packet.all;
                  Outbound_Packet.To_Address := Transaction.Client_Resolver_Address;
                  Outbound_Packet.To_Port    := Transaction.Client_Resolver_Port;
                  Outbound_Packet_Queue.Put (Outbound_Packet);

               exception
                  when Error : others =>
                     begin
                        Put (Standard_Error, "Transaction error: ");
                        Put_Line (Standard_Error, Exception_Information (Error));
                     end;
               end From_Upstream_Resolver_Packet;
            or
               accept Stop do
                  declare

                     -- Clean up the pool and get rid of everything we don't need
                     procedure Empty_Hash_Map (c : DNS_Transaction_Maps.Cursor) is
                        procedure Free_Transaction is new Ada.Unchecked_Deallocation
                          (Object => DNS_Transaction, Name => DNS_Transaction_Ptr);
                        procedure Free_Packet is new Ada.Unchecked_Deallocation
                          (Object => DNS_Raw_Packet_Record, Name => DNS_Raw_Packet_Record_Ptr);
                        P : DNS_Transaction_Ptr;
                     begin
                        P := Element (c);

                        if P.From_Client_Resolver_Packet /= null then
                           if P.From_Client_Resolver_Packet.Raw_Data.Data /= null then
                              Free_Stream_Element_Array_Ptr(P.From_Client_Resolver_Packet.Raw_Data.Data);
                           end if;
                           Free_Packet(P.From_Client_Resolver_Packet);
                        end if;


                        if P.From_Upstream_Resolver_Packet /= null then
                           if P.From_Upstream_Resolver_Packet.Raw_Data.Data /= null then
                              Free_Stream_Element_Array_Ptr(P.From_Upstream_Resolver_Packet.Raw_Data.Data);
                           end if;
                           Free_Packet(P.From_Upstream_Resolver_Packet);
                        end if;

                        Free_Transaction (P);
                     end Empty_Hash_Map;
                  begin
                     Transaction_Hashmap.Iterate (Empty_Hash_Map'Access);
                  end;

                  Running := False;
               end Stop;
            or
               delay 0.1;
            end select;
         end loop;
      end loop;
   end DNS_Transaction_Manager_Task;

   function IP_Transaction_Key_HashID (id : IP_Transaction_Key) return Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (id));
   end IP_Transaction_Key_HashID;
end DNS_Transaction_Manager;
