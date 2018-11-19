with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

limited with DNS_Packet_Processor;
with DNS_Core_Constructs; use DNS_Core_Constructs;

package DNS_RData_Processor is

   type Parsed_RData is abstract tagged record
      RName : Unbounded_String;
      RType : RR_Types;
      TTL   : Unsigned_32;
   end record;

   type Parsed_RData_Access is access all Parsed_RData'Class;

   function To_Parsed_RData
     (Parsed_RR : DNS_Packet_Processor.Parsed_DNS_Resource_Record) return Parsed_RData_Access;

   -- Represents RData in a string like fashion
   procedure From_Parsed_RR (This : in out Parsed_RData;
      Parsed_RR                   : DNS_Packet_Processor.Parsed_DNS_Resource_Record) is abstract;
   function RClass_To_String (This : in Parsed_RData) return String is abstract;
   function RData_To_String (This : in Parsed_RData) return String is abstract;
   function Print_Packet (This : in Parsed_RData) return String is abstract;
end DNS_RData_Processor;
