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

-- @summary
-- The DNS subpackage covers all basic functionality in regards to handling the
-- DNS wire protocol.
--
-- @description
-- Included in this package are the following bits of information:
--
-- - Mapping of RRTypes and Classes to/from Integers - DNS client
-- implementation - DNS server implementation - RData processor and encoder -
-- Packet processor (including handling DNS compression) - Other constants and
-- constraints included by DNS
--
   package DNSCatcher.DNS is

   -- IANA defined RRTypes.
   type RR_Types is
     (A, -- An IPv4 Address
      NS, -- Reference to another nameserver (encoded as a domain name)
      MD, -- Mail Destination - Obsolete (RFC 973)
      MF, -- Mail Forwarder - Obsolete
      CNAME, -- Canonical Name Record - Alias of of domain name to another
      SOA, -- Start of Authority - Defines the authoritative information of a DNS zone
      MB, -- Mailbox Domain Name - Obsolete (RFC883)
      MG, -- Mail Group Member - Obsolete (RFC883)
      MR, -- Mail Rename Domain - Obsolete (RFC883)
      DNS_NULL, -- NULL RRType - Called DNS_NULL due to keyword conflict in Ada
      WKS, -- Well Known Service - Obsolete
      PTR, -- Reverse lookup Information for a zone
      HINFO, -- Host Information
      MINFO, -- Mailbox or Mailing List Information
      MX, -- Mail eXchange
      TXT, -- Text Record
      RP, -- Responsible Person - RFC1183
      AFSDB, -- AFS Database Location
      X25, -- X.25 PSDN address
      ISDN, -- ISDN address
      RT, -- Route Through (RFC1183)
      NSAP, -- for NSAP address, NSAP style A record
      NSAP_PTR, -- for domain name pointer, NSAP style
      SIG, -- security signature
      KEY, -- security key
      PX, -- X.400 mail mapping information
      GPOS, -- Geographical Position
      AAAA, -- IPv6 location
      LOC, -- Location Information
      NXT, -- Next Domain (Obsolete)
      EID, -- Endpoint Identifier
      NIMLOC, -- Nimrod Locator
      SRV, -- Server/Service Selection
      ATMA, -- ATM Address
      NAPTR, -- Naming Authority Pointer
      KX, -- Kex Exchanger
      CERT, -- Certificate Record
      A6, -- Old-style IPv6 Record
      DNAME, -- DNAME
      SINK, -- Kitchen Sink
      OPT, -- EDNS Special Record
      APL, -- APL
      DS, -- Delegation Signer
      SSHFP, -- SSH Key Fingerprint
      IPSECKEY, -- IPSEC Key
      RRSIG, -- Resource Record SIGnature
      NSEC, -- Next SECure
      DNSKEY, -- DNSSEC Public Key
      DHCID, -- DHCID
      NSEC3, -- Next SECure Version 3
      NSEC3PARAM, -- NSEC3 Parameters
      TLSA, -- DANE Record Implementation
      SMIMEA, -- S/MIME Certificate Assoication
      HIP, -- Host Identity Protocol
      NINFO, -- NINFO
      RKEY, -- RKEY
      TALINK, -- Trust Anchor Link
      CDS, -- Child DS
      CDNSKEY, -- DNSKEY(s) child wants in parent DS
      OPENPGPKEY, -- OpenPGP Key
      CSYNC, -- Child-To-Parent Sync
      ZONEMD, -- Message Digest for DNS Zone
      SPF, -- SPF (RFC7208)
      UINFO, -- User Info (Reserved)
      UID, -- User Identifier (Reversed)
      GID, -- Group Info (Reserved)
      UNSPEC, -- Reversed
      NID, -- RFC6742
      L32, -- RFC6742
      L64, -- RFC6742
      LP, -- RFC6742
      EUI48, -- EUI-48 Address
      EUI64, -- EUI-64 Address
      TKEY, -- Transaction Key
      TSIG, -- Transaction Signature
      IXFR, -- Incremental Zone Transfer
      AXFR, -- Zone Transfer
      MAILB, -- Request for mailbox related records (MB/MG/MR)
      MAILA, -- Request for mail agent RRs (MD/MF)
      WILDCARD, -- Request for all records ("*")
      URI, -- URI
      CAA, -- Certificate Authority Restriction
      AVC, -- Application Visibility and Control
      DOA, -- Digital Object Architecture
      AMTRELAY, -- Automatic Multicast Tunneling Relay
      TA, -- DNSSEC Trust Authorities
      DLV -- DNSSEC Lookaside Validation
      );

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
                     ZONEMD => 63,
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
                     AMTRELAY => 260,
                     TA => 32768,
                     DLV => 32769);
   --!pp on

   -- Helper function to convert RR_Type to string
   --
   -- @value RR_Type RR_Type to convert
   function To_String
     (RR_Type : RR_Types)
      return String;

   type Classes is
     (INternet, -- INternet Class
      CS, -- CSNet Class
      CH, -- ChaosNet Class
      HS, -- Hesiod
      QCLASS_NONE, -- QCLASS None
      QCLASS_ANY -- QClass (Any)
      );

      --!pp off
   for Classes use (INternet => 1,
                    CS => 2,
                    CH => 3,
                    HS => 4,
                    QCLASS_NONE => 254,
                    QCLASS_ANY => 255);
   --!pp on

   -- Helper function to convert DNS_Classes to String
   --
   -- @value DNS_Class Class to convert
   function To_String
     (DNS_Class : Classes)
      return String;

   -- DNS Return Codes
   type RCodes is
     (NoError, -- No Error
      FormErr, -- Format Error
      ServFail, -- Server Failure
      NXDomain, -- Non-Existent Domain
      NotImp, -- Not Implemented
      Refused, -- Query Refused
      YXDomain, -- Name Exists when it shouldn't
      YXRRSet, -- RRSet Exists when it shouldn't
      NXRRSet, -- RRSet doesn't exist and it should
      NotAuth, -- Server is not authoritive for zone
      NotZone, -- Not authorized
      BADVERS, -- Bad OPT version
      -- 16 can also be BADSIG but that's not trivial to represent
      BADKEY, -- Key not recognized
      BADTIME, -- Signature out of time window
      BADMODE, -- Bad TKEY Mode
      BADNAME, -- Duplicate key name
      BADALG, -- Algorithm not supported
      BADTRUNC, -- Bad truncation
      BADCOOKIE -- Bad/missing server cookie
      );

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
     (LLQ, -- Reserved (on hold)
      UL, -- Reserved (on hold)
      NSID, -- Standard (RFC5001)
      DAU, -- Standard (RFC6975)
      DHU, -- Standard (RFC6975)
      N3U, -- Standard (RFC6975)
      edns_client_subnet, -- Optional (RFC7314)
      EDNS_EXPIRE, -- Optional (RFC7314)
      COOKIE, -- Standard (RFC7873)
      edns_tcp_keepalive, -- Standard (RFC7828)
      Padding, -- Standard (RFC7830)
      CHAIN, -- Standard (RFC7901)
      edns_key_tag, -- Optional (RFC8145)
      EDNS_Client_Tag, -- Optional (draft)
      EDNS_Server_Tag, -- Optional (draft)
      DeviceID -- Optional (draft)
      );

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
