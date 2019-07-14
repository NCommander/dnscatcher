with DNS_Packet_Processor; use DNS_Packet_Processor;

package DNS_RData_Processor.OPT_Parser is

   type Parsed_OPT_RData is new DNS_RData_Processor.Parsed_RData with record
      -- Defined as CLASS in RFC6891
      Requester_UDP_Size : Unsigned_16;

      -- These bits come out of the TTL part of the record
      Extended_RCode : DNS_Core_Constructs.RCodes;
      EDNS_Version   : Unsigned_8;
      DNSSEC_OK      : Boolean;
      Z_Section      : Unsigned_15;

      -- Length of OPT attribute/values below
      RData_Len : Unsigned_16;

      -- Raw OPT data
      RData_Data : Unbounded_String;
   end record;
   type Parsed_OPT_RData_Access is access all Parsed_OPT_RData;

   procedure From_Parsed_RR
     (This       : in out Parsed_OPT_RData;
      DNS_Header :        DNS_Packet_Header;
      Parsed_RR  :        Parsed_DNS_Resource_Record);
   function RData_To_String
     (This : in Parsed_OPT_RData)
      return String;
   function RClass_To_String
     (This : in Parsed_OPT_RData)
      return String;
   function Print_Packet
     (This : in Parsed_OPT_RData)
      return String;
   procedure Delete (This : in out Parsed_OPT_RData);

   Unknown_EDNS_Version : exception;
end DNS_RData_Processor.OPT_Parser;
