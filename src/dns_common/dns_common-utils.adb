with Interfaces; use Interfaces;
with Interfaces.C;

with System;

package body DNS_Common.Utils is
   function Inet_Ntop
     (Family   : IP_Addr_Family;
      Raw_Data : Unbounded_String)
      return Unbounded_String
   is
      procedure Internal
        (Family : C.int;
         Src    : System.Address;
         Dst    : System.Address;
         Len    : C.int);
      pragma Import (C, Internal, "ada_inet_ntop");

      -- 16 is the max length of a v4 string + null
      C_IP_String : C.char_array (1 .. 16);
   begin
      -- Call the ada helper function
      Internal
        (C.int (Family'Enum_Rep), To_String (Raw_Data)'Address,
         C_IP_String'Address, 15);
      return To_Unbounded_String (C.To_Ada (C_IP_String));
   end Inet_Ntop;
end DNS_Common.Utils;
