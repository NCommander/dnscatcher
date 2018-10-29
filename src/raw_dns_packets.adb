package body Raw_DNS_Packets is
   -- Handles DNS Packets in a FIFO queue; built around Vectors, this may need to be changed for
   -- performance reasons at some point
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
            accept Empty do
               -- Release all memory
               declare
                  procedure Dump_Vector_Data (c : Stored_Packets_Vector.Cursor) is
                  begin
                     null;
                     --Free_Packet_Raw_Data (Stored_Packets (c).Raw_Data);
                  end Dump_Vector_Data;
               begin
                  Stored_Packets.Iterate (Dump_Vector_Data'access);
               end;
            end Empty;
         or
            terminate;
         end select;
      end loop;
   end Raw_DNS_Packet_Queue;
   end Raw_DNS_Packets;
