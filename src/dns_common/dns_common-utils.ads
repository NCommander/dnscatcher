with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package DNS_Common.Utils is
   function Inet_Ntop (Family : IP_Addr_Family;
      Raw_Data                : Unbounded_String) return Unbounded_String;
end DNS_Common.Utils;
