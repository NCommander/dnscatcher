package DNS_Classes is
   -- IANA registered DNS classes
   type DNS_Classes is
     (INternet, -- IN (IN is an Ada keyword, we'll have to handle this specially)
      CH,
      HS,
      QCLASS_NONE,
      QCLASS_ANY);
   for DNS_Classes use
     (INternet    => 1,
      CH          => 3,
      HS          => 4,
      QCLASS_NONE => 254,
      QCLASS_ANY  => 255);
end DNS_Classes;
