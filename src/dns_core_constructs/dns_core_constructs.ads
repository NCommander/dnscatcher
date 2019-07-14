with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;

with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with System;

package DNS_Core_Constructs is

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
      BADSIG,
      -- 16 can also be BADVERS but that's not trivial to represent
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
                   BADSIG => 16,
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

   type DNS_Packet_Header is record
      Identifier              : Unsigned_16;
      Query_Response_Flag     : Boolean;
      Opcode                  : Unsigned_4;
      Authoritative_Answer    : Boolean;
      Truncated               : Boolean; -- Authoritive Answer
      Recursion_Desired       : Boolean;
      Recursion_Available     : Boolean;
      Zero                    : Boolean;
      Authenticated_Data      : Boolean;
      Checking_Disabled       : Boolean;
      Response_Code           : Unsigned_4;
      Question_Count          : Unsigned_16;
      Answer_Record_Count     : Unsigned_16;
      Authority_Record_Count  : Unsigned_16; -- NSCount
      Additional_Record_Count : Unsigned_16;
   end record;
   for DNS_Packet_Header'Bit_Order use System.High_Order_First;
   for DNS_Packet_Header'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (DNS_Packet_Header);
   type DNS_Packet_Header_Ptr is access all DNS_Packet_Header;

   -- I don't know if this is spec, or a bug with GNAT, but 'Size returns the
   -- wrong value and I get 128 which means I need to calculate this like this
   -- which is kinda bullshit ...
   DNS_PACKET_HEADER_SIZE : constant Stream_Element_Offset := 12;

   -- Represents everything past the data
   type Stream_Element_Array_Ptr is access all Stream_Element_Array;
   subtype Raw_DNS_Packet_Data is Stream_Element_Array_Ptr;

   type Raw_DNS_Packet is record
      Header : DNS_Packet_Header;
      Data   : Raw_DNS_Packet_Data;
   end record;
   for Raw_DNS_Packet'Bit_Order use System.High_Order_First;
   for Raw_DNS_Packet'Scalar_Storage_Order use System.High_Order_First;
   pragma Pack (Raw_DNS_Packet);
   type Raw_DNS_Packet_Ptr is access Raw_DNS_Packet;

   procedure Free_Raw_DNS_Packet (Packet : in out Raw_DNS_Packet);

   -- DNS packet elements can't be represented in binary form due to the fact
   -- that they're variable legnth with a non-standard encoding, so we'll have
   -- to just save an array and turn it into something parsable later

   type Raw_DNS_Packet_Question is record
      QName  : Stream_Element_Array_Ptr;
      QType  : Unsigned_16;
      QClass : Unsigned_16;
   end record;
   for Raw_DNS_Packet_Question'Bit_Order use System.High_Order_First;
   for Raw_DNS_Packet_Question'Scalar_Storage_Order use System
       .High_Order_First;
   pragma Pack (Raw_DNS_Packet_Question);

   -- For the remaining sections, they share a common data format
   type Raw_DNS_Resource_Record is record
      Name         : Stream_Element_Array_Ptr; -- Variable Length
      RR_Type      : Unsigned_16;
      DNS_Class    : Unsigned_16;
      TTL          : Unsigned_32;
      RData_Length : Unsigned_16;
      RData        : Stream_Element_Array_Ptr;
   end record;

   type Raw_DNS_Packet_Answer is new Raw_DNS_Resource_Record;
   type Raw_DNS_Packet_Authority is new Raw_DNS_Resource_Record;
   type Raw_DNS_Packet_Additional is new Raw_DNS_Resource_Record;

   subtype SEA_DNS_Packet_Header is
     Stream_Element_Array
       (1 .. DNS_PACKET_HEADER_SIZE); -- 12 is the packed sized

   function SEA_To_DNS_Packet_Header is new Ada.Unchecked_Conversion
     (Source => Stream_Element_Array, Target => DNS_Packet_Header);
   function DNS_Packet_Header_To_SEA is new Ada.Unchecked_Conversion
     (Source => DNS_Packet_Header, Target => SEA_DNS_Packet_Header);
end DNS_Core_Constructs;
