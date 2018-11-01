with Ada.Containers.Vectors;  use Ada.Containers;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Streams;             use Ada.Streams;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with Raw_DNS_Packets;        use Raw_DNS_Packets;
with DNS_Raw_Packet_Records; use DNS_Raw_Packet_Records;
with DNS_RR_Types;           use DNS_RR_Types;
with DNS_Classes;            use DNS_Classes;

package Packet_Parser is

   -- Create vector types for each type of section
   -- DNS question converted from wire format to human parsable format
   type Parsed_DNS_Question is record
      QName  : Unbounded_String;
      QType  : RR_Types;
      QClass : DNS_Classes.Classes;
   end record;

   type Parsed_DNS_Resource_Record is record
      RName     : Unbounded_String;
      RType     : RR_Types;
      RClass    : DNS_Classes.Classes;
      TTL       : Unsigned_16;
      RD_Length : Unsigned_16;
      RData     : Unbounded_String;
   end record;

   package Question_Vector is new Vectors (Natural, Parsed_DNS_Question);
   package Resource_Record_Vector is new Vectors (Natural, Parsed_DNS_Resource_Record);

   type Parsed_DNS_Packet is record
      Header     : DNS_Packet_Header;
      Questions  : Question_Vector.Vector;
      Answer     : Resource_Record_Vector.Vector;
      Authority  : Resource_Record_Vector.Vector;
      Additional : Resource_Record_Vector.Vector;
   end record;
   type Parsed_DNS_Packet_Ptr is access Parsed_DNS_Packet;

   procedure Packet_Parser (Packet : DNS_Raw_Packet_Record_Ptr);

   function Parse_DNS_Packet_Name_Records (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Unbounded_String;

   function Parse_DNS_RR_Type (Raw_Data :        Raw_DNS_Packet_Data;
      Offset                            : in out Stream_Element_Offset) return RR_Types;

   function Parse_DNS_Class (Raw_Data :        Raw_DNS_Packet_Data;
      Offset                          : in out Stream_Element_Offset) return DNS_Classes.Classes;

   function Parse_Question_Record (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Parsed_DNS_Question;

   function Parse_Resource_Record_Response (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Parsed_DNS_Resource_Record;

   Unknown_RCode   : exception;
   Unknown_RR_Type : exception;
   Unknown_Class   : exception;
end Packet_Parser;
