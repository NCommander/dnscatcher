package DNS_Classes is
   -- IANA registered DNS classes
   type Classes is
     (INternet, -- IN (IN is an Ada keyword, we'll have to handle this specially)
      CH,
      HS,
      QCLASS_NONE,
      QCLASS_ANY);
   for Classes use
     (INternet    => 1,
      CH          => 3,
      HS          => 4,
      QCLASS_NONE => 254,
      QCLASS_ANY  => 255);

   function To_String(DNS_Class : Classes) return String;
end DNS_Classes;
