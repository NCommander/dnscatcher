with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Sockets; use GNAT.Sockets;

with Ada.Task_Identification; use Ada.Task_Identification;

with Ada.Streams; use Ada.Streams;

with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;

with Ada.Unchecked_Conversion;

with Ada.Containers; use Ada.Containers;

with Ada.Unchecked_Deallocation;

with Raw_Dns_Packets;

package body Packet_Catcher is
   Outbound_Packet_Queue : Raw_DNS_Packet_Queue;
   Shutting_Down         : Boolean := False;

   -- Conversion functions
   function SEA_To_DNS_Packet_Header is new Ada.Unchecked_Conversion
     (Source => Stream_Element_Array,
      Target => Raw_Dns_Packets.Dns_Packet_Header);

   procedure Free_Stream_Element_Array_Ptr is new Ada.Unchecked_Deallocation
     (Object => Stream_Element_Array, Name => Stream_Element_Array_Ptr);

   -- Handles DNS Packets in a FIFO queue; built around Vectors, this may need
   -- to be changed for performance reasons at some point
   task body Raw_DNS_Packet_Queue is
      Stored_Packets : Vector;
      Packet_Count   : Integer := 0;
   begin
      loop
         select
            accept Put (Packet : in Raw_DNS_Packet) do
               Stored_Packets.Append (Packet);
            end Put;
            Packet_Count := Packet_Count + 1;
         or
            accept Get (Packet : out Raw_DNS_Packet) do
               Packet := Stored_Packets.First_Element;
               Stored_Packets.Delete_First;
            end Get;
            Packet_Count := Packet_Count - 1;
         or
            accept Count (Count : out Integer) do
               Count := Packet_Count;
            end Count;
         or
            accept Shutdown_Task do
               -- Release all memory
               declare
                  procedure Dump_Vector_Data
                    (c : Stored_Packets_Vector.Cursor) is
                  begin
                     null;
                     --Free_Packet_Raw_Data (Stored_Packets (c).Raw_Data);
                  end Dump_Vector_Data;
               begin
                  Stored_Packets.Iterate (Dump_Vector_Data'access);
               end;
            end Shutdown_Task;
         or
            terminate;
         end select;
      end loop;
   end Raw_DNS_Packet_Queue;

   -- Handle the map for tracking transactions to/from source
   task body DNS_Transaction_Manager is
      Transaction         : DNS_Transaction;
      Packet_Header       : Raw_DNS_Packets.DNS_Packet_Header;
      Hashmap_Key         : IP_Transaction_Key;
      Hashmap_Cursor      : DNS_Transaction_Maps.Cursor;
      Outbound_Packet     : Raw_DNS_Packet;
      Transaction_Hashmap : DNS_Transaction_Maps.Map;
   begin
      loop
         select
            accept From_Client_Resolver_Packet (Packet : Raw_DNS_Packet) do
               Put_Line ("HERE!!!");
               New_Line;

               Packet_Header := SEA_To_Dns_Packet_Header (Packet.Raw_Data.all);
               Put ("  DNS Transaction ID: ");
               Put
                 (Standard_Output, Integer (Packet_Header.Identifier),
                  Base => 16);
               New_Line;

               Hashmap_Key :=
                 IP_Transaction_Key
                   (Packet.To_Address & Packet.To_Port'Image &
                    Packet_Header.Identifier'Image);

               Put_Line (To_String (Hashmap_Key));
               -- Create the key if necessary
               Hashmap_Cursor := Transaction_Hashmap.Find (Hashmap_Key);

               if Hashmap_Cursor = DNS_Transaction_Maps.No_Element then
                  Transaction.Client_Resolver_Address := Packet.From_Address;
                  Transaction.Client_Resolver_Port    := Packet.From_Port;
                  Transaction.Server_Resolver_Address := Packet.To_Address;
                  Transaction.Server_Resolver_Port    := Packet.To_Port;
                  Transaction.DNS_Transaction_Id := Packet_Header.Identifier;
                  Transaction_Hashmap.Insert (Hashmap_Key, Transaction);
               end if;

               -- Now append to the vector
               -- FIXME: This likely needs to be a ordered hashmap ...
               Transaction_Hashmap.Reference (Hashmap_Key)
                 .From_Client_Resolver_Packets
                 .Append
                 (Packet);

               Put_Line
                 ("Inbound has" &
                  Transaction_Hashmap.Reference (Hashmap_Key)
                    .From_Client_Resolver_Packets
                    .Length'
                    Image);
               Put_Line
                 ("Server Resolver" &
                  Transaction_Hashmap.Reference (Hashmap_Key)
                    .From_Upstream_Resolver_Packets
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
               Put
                 (Standard_Output, Integer (Packet_Header.Identifier),
                  Base => 16);
               New_Line;

               Hashmap_Key :=
                 IP_Transaction_Key
                   (Packet.From_Address & Packet.From_Port'Image &
                    Packet_Header.Identifier'Image);

               Put_Line (To_String (Hashmap_Key));
               -- Create the key if necessary
               Hashmap_Cursor := Transaction_Hashmap.Find (Hashmap_Key);

               if Hashmap_Cursor = DNS_Transaction_Maps.No_Element then
                  Transaction.Client_Resolver_Address := Packet.To_Address;
                  Transaction.Client_Resolver_Port    := Packet.To_Port;
                  Transaction.Server_Resolver_Address := Packet.From_Address;
                  Transaction.Server_Resolver_Port    := Packet.From_Port;
                  Transaction.DNS_Transaction_Id := Packet_Header.Identifier;
                  Transaction_Hashmap.Insert (Hashmap_Key, Transaction);
               end if;

               -- Now append to the vector
               -- FIXME: This likely needs to be a ordered hashmap ...
               Transaction := Transaction_Hashmap.Reference (Hashmap_Key);
               Transaction_Hashmap.Reference (Hashmap_Key)
                 .From_Upstream_Resolver_Packets
                 .Append
                 (Packet);

               Put_Line
                 ("To Upstream has" &
                  Transaction_Hashmap.Reference (Hashmap_Key)
                    .From_Client_Resolver_Packets
                    .Length'
                    Image);
               Put_Line
                 ("Server Resolver" &
                  Transaction_Hashmap.Reference (Hashmap_Key)
                    .From_Client_Resolver_Packets
                    .Length'
                    Image);

               -- Flip the packet around so it goes to the right place
               Outbound_Packet            := Packet;
               Outbound_Packet.To_Address :=
                 Transaction.Client_Resolver_Address;
               Outbound_Packet.To_Port := Transaction.Client_Resolver_Port;
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
   end DNS_Transaction_Manager;

   function IP_Transaction_Key_HashID
     (id : IP_Transaction_Key) return Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (id));
   end IP_Transaction_Key_HashID;

   procedure Run_Catcher is
      -- Input and Output Sockets
      UPSTREAM_DNS_SERVER : String         := "4.2.2.2";
      DNS_Socket          : Socket_Type;
      To_Resolver         : Inet_Addr_Type := Inet_Addr ("4.2.2.2");
      DNS_Transactions    : DNS_Transaction_Manager;

      -- Tasks Definition
      task Receive_Packet is
         entry Start;
         entry Stop;
      end Receive_Packet;

      task Send_Packet is
         entry Start;
         entry Stop;
      end Send_Packet;

      task body Receive_Packet is
         Buffer           : Stream_Element_Array_Ptr;
         Offset           : Stream_Element_Offset;
         Incoming_Address : Sock_Addr_Type;
         DNS_Packet       : Raw_DNS_Packet;
         Process_Packets  : Boolean := False;
      begin
         Buffer :=
           new Stream_Element_Array (1 .. Stream_Element_Offset (1500));
         loop
            -- Either just started or stopping, we're terminatable in this
            -- state
            Put_Line ("Received State STOPPED");
            while Process_Packets = False loop
               select
                  accept Start do
                     Process_Packets := True;
                  end Start;
               or
                  accept Stop do
                     null;
                  end Stop;
               or
                  terminate;
               end select;
            end loop;

            Put_Line ("Received State STARTED");

            while Process_Packets loop -- Main receiving loop
               select
                  accept Start do
                     null;
                  end Start;
               or
                  accept Stop do
                     Process_Packets := False;
                  end Stop;
               else
                  begin
                     Receive_Socket
                       (Socket => DNS_Socket, Item => Buffer.all,
                        Last   => Offset, From => Incoming_Address);

                     Put_Line
                       ("Received UDP Packet From " &
                        Image (Incoming_Address.Addr) & ":" &
                        Port_Type'Image (Incoming_Address.Port));

                     Put_Line
                       ("Read " & Stream_Element_Offset'Image (Offset) &
                        " bytes");

                     -- Copy the packet for further processing
                     DNS_Packet.From_Address :=
                       To_Unbounded_String (Image (Incoming_Address.Addr));
                     DNS_Packet.From_Port  := Incoming_Address.Port;
                     DNS_Packet.To_Address :=
                       To_Unbounded_String (UPSTREAM_DNS_SERVER);
                     DNS_Packet.To_Port := 53;

                     -- Create a new buffer of the right length and copy the
                     -- data in
                     DNS_Packet.Raw_Data :=
                       new Stream_Element_Array (1 .. Offset);
                     DNS_Packet.Raw_Data.all    := Buffer (1 .. Offset);
                     DNS_Packet.Raw_Data_Length := Offset;

                     -- Was this a server response, or client response?
                     if UPSTREAM_DNS_SERVER /=
                       (Image (Incoming_Address.Addr)) then
                        DNS_Transactions.From_Client_Resolver_Packet
                          (Packet => DNS_Packet);
                     else
                        DNS_Transactions.From_Upstream_Resolver_Packet
                          (Packet => DNS_Packet);
                     end if;

                  exception

                     -- Socket Errors will happen due to time out all the time,
                     -- we just need to restart and recover
                     when Error : GNAT.Sockets.Socket_Error =>
                        begin
                           -- This isn't the RIGHT thing to do, but its the
                           -- only way to handle Recv Timeouts because it
                           -- doesn't use a unique exception. We'll hope the
                           -- right exception pops up on write cause otherwise
                           -- we're screwed
                           null;
                        end;
                  end;

               end select;
            end loop;
         end loop;
      exception
         when Error : others =>
            begin
               Put (Standard_Error, "Unknown error: ");
               Put_Line (Standard_Error, Exception_Information (Error));
               Packet_Catcher.Stop_Catcher;
            end;

            -- Clean up after ourselves
            Free_Stream_Element_Array_Ptr (Buffer);
      end Receive_Packet;

      task body Send_Packet is
         DNS_Packet       : Raw_DNS_Packet;
         Outgoing_Address : Inet_Addr_Type;
         Outgoing_Port    : Port_Type;
         Length           : Stream_Element_Offset;
         Process_Packets  : Boolean := False;
         Packet_Count     : Integer := 0;
      begin
         Put_Line ("Send packet task started");
         loop
            -- Either just started or stopping, we're terminatable in this
            -- state
            Put_Line ("Send State STOPPED");
            while Process_Packets = False loop
               select
                  accept Start do
                     Process_Packets := True;
                  end Start;
               or
                  accept Stop do
                     null;
                  end Stop;
               or
                  terminate;
               end select;
            end loop;

            Put_Line ("Send State STARTED");
            -- We're actively processing packets
            while Process_Packets loop
               select
                  accept Start do
                     null;
                  end Start;
               or
                  accept Stop do
                     Process_Packets := False;
                  end Stop;
               else
                  Outbound_Packet_Queue.Count (Packet_Count);
                  if Packet_Count > 0 then
                     Outbound_Packet_Queue.Get (DNS_Packet);
                     Outgoing_Address :=
                       Inet_Addr (To_String (DNS_Packet.To_Address));
                     Outgoing_Port := Port_Type (DNS_Packet.To_Port);

                     -- And send the damn thing
                     Put ("Sent packet to ");
                     Put_Line (Image (Outgoing_Address));
                     Put_Line (Port_Type'Image (Outgoing_Port));

                     Send_Socket
                       (Socket => DNS_Socket,
                        Item   =>
                          DNS_Packet.Raw_Data.all
                            (1 .. DNS_Packet.Raw_Data_Length),
                        Last => Length,
                        To   =>
                          (Family => Family_Inet, Addr => Outgoing_Address,
                           Port   => Outgoing_Port));

                     Put ("Sent packet to ");
                     Put_Line (Image (Outgoing_Address));
                  end if;
               end select;
            end loop;
         end loop;

      exception
         when Error : others =>
            begin
               Put (Standard_Error, "Unknown error: ");
               Put_Line (Standard_Error, Exception_Information (Error));
            end;
      end Send_Packet;

   begin
      -- Setup the inbound port 53 socket
      GNAT.Sockets.Initialize;
      Create_Socket (Socket => DNS_Socket, Mode => Socket_Datagram);
      Set_Socket_Option
        (Socket => DNS_Socket,
         Option => (GNAT.Sockets.Receive_Timeout, Timeout => 1.0));
      Bind_Socket
        (Socket  => DNS_Socket,
         Address =>
           (Family => Family_Inet, Addr => Any_Inet_Addr, Port => 53));

      -- Start our processing loops
      Receive_Packet.Start;
      Send_Packet.Start;

      loop
         if Shutting_Down = True then
            Put_Line ("Starting shutdown");
            Receive_Packet.Stop;
            Send_Packet.Stop;
            Close_Socket (DNS_Socket);
            Put_Line ("Sockets Stopped");
            return;
         else
            delay 1.0;
         end if;
      end loop;

   end Run_Catcher;

   -- And shutdown
   procedure Stop_Catcher is
   begin
      --Put (Standard_Error, "[CRITICAL] Plug has been pulled!");
      Shutting_Down := True;
   end Stop_Catcher;

end Packet_Catcher;
