with DNS_Core_Constructs; use DNS_Core_Constructs;
with DNS_Packet_Processor; use DNS_Packet_Processor;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors; use Ada.Containers;

with Interfaces.C.Extensions; use Interfaces.C.Extensions;

-- Creates and handles making DNS client requests
package DNS_Client is
   -- Subtypes for handing transaction generation
   subtype DNS_Transaction_ID is Unsigned_16;
   package Random_Transaction_ID is new Ada.Numerics.Discrete_Random(DNS_Transaction_ID);

   type Client is tagged record
      Header: DNS_Packet_Header;
      Questions: Question_Vector.Vector;
   end record;
   type Client_Access is access all Client'Class;

   -- Populates the header field on the fly
   procedure Create_Header(This: in out Client);

   -- Adds a question to the DNS request
   procedure Add_Query(This: in out Client;
                       QName: Unbounded_String;
                       QType: RR_Types;
                       QClass: Classes);

   -- Placeholder for now until we figure out what this is supposed to
   -- actually look like

   procedure Run_Query(This: in out Client);
   -- Utility functions;
   function Create_DNS_Packet_Name_Record(Question: Parsed_DNS_Question) return Unbounded_String;
end DNS_Client;
