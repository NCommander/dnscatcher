with Utils;
package body Raw_DNS_Packets is

   -------------------------
   -- Free_Raw_DNS_Packet --
   -------------------------

   procedure Free_Raw_DNS_Packet (Packet: in out Raw_DNS_Packet) is
   begin
      Free_Stream_Element_Array_Ptr(Packet.Data);
   end Free_Raw_DNS_Packet;

end Raw_DNS_Packets;
