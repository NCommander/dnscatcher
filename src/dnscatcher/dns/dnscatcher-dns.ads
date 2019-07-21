-- Copyright 2019 Michael Casadevall <michael@casadevall.pro>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

package DNSCatcher.DNS is
   -- IANA Registered RRTypes
   type RR_Types is
     (A,
      NS,
      MD,
      MF,
      CNAME,
      SOA,
      MB,
      MG,
      MR,
      DNS_NULL,
      WKS,
      PTR,
      HINFO,
      MINFO,
      MX,
      TXT,
      RP,
      AFSDB,
      X25,
      ISDN,
      RT,
      NSAP,
      NSAP_PTR,
      SIG,
      KEY,
      PX,
      GPOS,
      AAAA,
      LOC,
      NXT,
      EID,
      NIMLOC,
      SRV,
      ATMA,
      NAPTR,
      KX,
      CERT,
      A6,
      DNAME,
      SINK,
      OPT,
      APL,
      DS,
      SSHFP,
      IPSECKEY,
      RRSIG,
      NSEC,
      DNSKEY,
      DHCID,
      NSEC3,
      NSEC3PARAM,
      TLSA,
      SMIMEA,
      HIP,
      NINFO,
      RKEY,
      TALINK,
      CDS,
      CDNSKEY,
      OPENPGPKEY,
      CSYNC,
      SPF,
      UINFO,
      UID,
      GID,
      UNSPEC,
      NID,
      L32,
      L64,
      LP,
      EUI48,
      EUI64,
      TKEY,
      TSIG,
      IXFR,
      AXFR,
      MAILB,
      MAILA,
      WILDCARD,
      URI,
      CAA,
      AVC,
      DOA,
      TA,
      DLV);

   --!pp off
   for RR_Types use (A => 1,
                     NS => 2,
                     MD => 3,
                     MF => 4,
                     CNAME => 5,
                     SOA => 6,
                     MB => 7,
                     MG => 8,
                     MR => 9,
                     DNS_NULL => 10,
                     WKS => 11,
                     PTR => 12,
                     HINFO => 13,
                     MINFO => 14,
                     MX => 15,
                     TXT => 16,
                     RP => 17,
                     AFSDB => 18,
                     X25 => 19,
                     ISDN => 20,
                     RT => 21,
                     NSAP => 22,
                     NSAP_PTR => 23,
                     SIG => 24,
                     KEY => 25,
                     PX => 26,
                     GPOS => 27,
                     AAAA => 28,
                     LOC => 29,
                     NXT => 30,
                     EID => 31,
                     NIMLOC => 32,
                     SRV => 33,
                     ATMA => 34,
                     NAPTR => 35,
                     KX => 36,
                     CERT => 37,
                     A6 => 38,
                     DNAME => 39,
                     SINK => 40,
                     OPT => 41,
                     APL => 42,
                     DS => 43,
                     SSHFP => 44,
                     IPSECKEY => 45,
                     RRSIG => 46,
                     NSEC => 47,
                     DNSKEY => 48,
                     DHCID => 49,
                     NSEC3 => 50,
                     NSEC3PARAM => 51,
                     TLSA => 52,
                     SMIMEA => 53,
                     HIP => 55,
                     NINFO => 56,
                     RKEY => 57,
                     TALINK => 58,
                     CDS => 59,
                     CDNSKEY => 60,
                     OPENPGPKEY => 61,
                     CSYNC => 62,
                     SPF => 99,
                     UINFO => 100,
                     UID => 101,
                     GID => 102,
                     UNSPEC => 103,
                     NID => 104,
                     L32 => 105,
                     L64 => 106,
                     LP => 107,
                     EUI48 => 108,
                     EUI64 => 109,
                     TKEY => 249,
                     TSIG => 250,
                     IXFR => 251,
                     AXFR => 252,
                     MAILB => 253,
                     MAILA => 254,
                     WILDCARD => 255,
                     URI => 256,
                     CAA => 257,
                     AVC => 258,
                     DOA => 259,
                     TA => 32768,
                     DLV => 32769);
   --!pp on

   function To_String
     (RR_Type : RR_Types)
      return String;

   type Classes is
     (INternet, -- IN (IN is an Ada keyword, we'll have to handle this specially)
      CH,
      HS,
      QCLASS_NONE,
      QCLASS_ANY);

   --!pp off
   for Classes use (INternet => 1,
                    CH => 3,
                    HS => 4,
                    QCLASS_NONE => 254,
                    QCLASS_ANY => 255);
   --!pp on

   function To_String
     (DNS_Class : Classes)
      return String;

   type RCodes is
     (NoError,
      FormErr,
      ServFail,
      NXDomain,
      NotImp,
      Refused,
      YXDomain,
      YXRRSet,
      NXRRSet,
      NotAuth,
      NotZone,
      BADVERS,
      -- 16 can also be BADSIG but that's not trivial to represent
      BADKEY,
      BADTIME,
      BADMODE,
      BADNAME,
      BADALG,
      BADTRUNC,
      BADCOOKIE);

   --!pp off
   for RCodes use (NoError => 0,
                   FormErr => 1,
                   ServFail => 2,
                   NXDomain => 3,
                   NotImp => 4,
                   Refused => 5,
                   YXDomain => 6,
                   YXRRSet => 7,
                   NXRRSet => 8,
                   NotAuth => 9,
                   NotZone => 10,
                   BADVERS => 16,
                   BADKEY => 17,
                   BADTIME => 18,
                   BADMODE => 19,
                   BADNAME => 20,
                   BADALG => 21,
                   BADTRUNC => 22,
                   BADCOOKIE => 23);
   --!pp on

   -- Casing and spelling comes directly from IANA reserved list
   type EDNS0_Option_Codes is
     (LLQ,
      UL,
      NSID,
      DAU,
      DHU,
      N3U,
      edns_client_subnet,
      EDNS_EXPIRE,
      COOKIE,
      edns_tcp_keepalive,
      Padding,
      CHAIN,
      edns_key_tag,
      EDNS_Client_Tag,
      EDNS_Server_Tag,
      DeviceID);

   --!pp off
   for EDNS0_Option_Codes use (LLQ => 1,
                               UL => 2,
                               NSID => 3,
                               DAU => 5,
                               DHU => 6,
                               N3U => 7,
                               edns_client_subnet => 8,
                               EDNS_EXPIRE => 9,
                               COOKIE => 10,
                               edns_tcp_keepalive => 11,
                               Padding => 12,
                               CHAIN => 13,
                               edns_key_tag => 14,
                               EDNS_Client_Tag => 16,
                               EDNS_Server_Tag => 17,
                               DeviceID => 26946);
   --!pp on
end DNSCatcher.DNS;
