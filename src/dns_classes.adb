package body DNS_Classes is
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
   
end DNS_Classes;
