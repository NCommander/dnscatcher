package DNS_RCodes is
   type RCodes is
     (NoError,
      FormErr,
      ServFail,
      NXDomain,
      NotImp,
      YXDomain,
      YXRRSet,
      NXRRSet,
      NotAuth,
      NotZone,
      BADVERS,
      BADSIG,
      BADVERS,
      BADKEY,
      BADTIME,
      BADMODE,
      BADNAME,
      BADALG,
      BADTRUNC,
      BADCOOKIE);
   for DNS_RCodes use
     (NoError   => 0,
      FormErr   => 1,
      ServFail  => 2,
      NXDomain  => 3,
      NotImp    => 4,
      Refused   => 5,
      YXDomain  => 6,
      YXRRSet   => 7,
      NXRRSet   => 8,
      NotAuth   => 9,
      NotZone   => 10,
      BADVERS   => 16,
      BADSIG    => 16,
      BADKEY    => 17,
      BADTIME   => 18,
      BADMODE   => 19,
      BADNAME   => 20,
      BADALG    => 21,
      BADTRUNC  => 22,
      BADCOOKIE => 23);
end DNS_RCodes;
