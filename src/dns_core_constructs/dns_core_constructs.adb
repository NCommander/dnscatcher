with DNS_Core_Constructs.Utils; use DNS_Core_Constructs.Utils;

package body DNS_Core_Constructs is
   function To_String(RR_Type : RR_Types) return String is
   begin
      -- This **** is required because 'in' is a keyword and Ada
      -- is case insensitive
      case RR_Type is
      when WILDCARD =>
         return "*";
      when others =>
         return RR_Types'Image(RR_Type);
      end case;

   end;

   function To_String(DNS_Class : Classes) return String is
   begin
      -- This **** is required because 'in' is a keyword and Ada
      -- is case insensitive
      case DNS_Class is
      when INternet =>
         return "IN";
      when others =>
         return Classes'Image(DNS_Class);
      end case;

   end;

   -------------------------
   -- Free_Raw_DNS_Packet --
   -------------------------

   procedure Free_Raw_DNS_Packet (Packet: in out Raw_DNS_Packet) is
   begin
      Free_Stream_Element_Array_Ptr(Packet.Data);
   end Free_Raw_DNS_Packet;
end DNS_Core_Constructs;
