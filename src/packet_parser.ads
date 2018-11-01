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
   package Resource_Record_Vector is new Vectors (Natural,
      Raw_DNS_Packets.Raw_DNS_Resource_Record);

   -- DNS question converted from wire format to human parsable format
   type Parsed_DNS_Question is record
      QName  : Unbounded_String;
      QType  : RR_Types;
      QClass : DNS_Classes.Classes;
   end record;

   package Question_Vector is new Vectors (Natural, Parsed_DNS_Question);

   type Parsed_DNS_Packet is record
      Header     : DNS_Packet_Header;
      Questions  : Question_Vector.Vector;
      Answer     : Resource_Record_Vector.Vector;
      Authority  : Resource_Record_Vector.Vector;
      Additional : Resource_Record_Vector.Vector;
   end record;
   type Parsed_DNS_Packet_Ptr is access Parsed_DNS_Packet;

   procedure Packet_Parser (Packet : DNS_Raw_Packet_Record_Ptr);
   procedure Parse_Question_Record (Raw_Data :        Raw_DNS_Packet_Data;
      Offset                                 : in out Stream_Element_Offset);

   Unknown_RCode   : exception;
   Unknown_RR_Type : exception;
   Unknown_Class   : exception;
end Packet_Parser;
