with Ada.Containers.Vectors;  use Ada.Containers;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Streams;             use Ada.Streams;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with DNS_Core_Constructs.Raw_Packet_Records; use DNS_Core_Constructs.Raw_Packet_Records;
with DNS_Core_Constructs;                    use DNS_Core_Constructs;
with DNS_RData_Processor;                    use DNS_RData_Processor;

with System;

package DNS_Packet_Processor is
   -- Create vector types for each type of section
   -- DNS question converted from wire format to human parsable format
   type Parsed_DNS_Question is record
      QName  : Unbounded_String;
      QType  : RR_Types;
      QClass : Classes;
   end record;

   type Parsed_DNS_Resource_Record is record
      RName  : Unbounded_String;
      RType  : RR_Types;
      RClass : Unsigned_16;
      TTL    : Unsigned_32;
      RData  : Unbounded_String;
   end record;

   package Question_Vector is new Vectors (Natural, Parsed_DNS_Question);
   package Resource_Record_Vector is new Vectors (Natural, Parsed_RData_Access);

   type Parsed_DNS_Packet is record
      Header     : DNS_Packet_Header;
      Questions  : Question_Vector.Vector;
      Answer     : Resource_Record_Vector.Vector;
      Authority  : Resource_Record_Vector.Vector;
      Additional : Resource_Record_Vector.Vector;
   end record;
   type Parsed_DNS_Packet_Ptr is access Parsed_DNS_Packet;

   procedure Packet_Parser (Packet : Raw_Packet_Record_Ptr);

   function Parse_DNS_Packet_Name_Records (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Unbounded_String;

   function Parse_DNS_RR_Type (Raw_Data :        Raw_DNS_Packet_Data;
      Offset                            : in out Stream_Element_Offset) return RR_Types;

   function Parse_DNS_Class (Raw_Data :        Raw_DNS_Packet_Data;
      Offset                          : in out Stream_Element_Offset) return Classes;

   function Parse_Question_Record (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Parsed_DNS_Question;

   function Parse_Resource_Record_Response (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Parsed_RData_Access;

   Unknown_RCode              : exception;
   Unknown_RR_Type            : exception;
   Unknown_Class              : exception;
   Unknown_Compression_Method : exception;

end DNS_Packet_Processor;
