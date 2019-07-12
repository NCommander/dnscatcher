package DNS_Common is
   type IP_Addr_Family is
     (IPv4,
      IPv6);
   for IP_Addr_Family use (IPv4 => 1, IPv6 => 2);
end DNS_Common;
