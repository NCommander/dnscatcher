package body DNS_RR_Types is
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

end DNS_RR_Types;
