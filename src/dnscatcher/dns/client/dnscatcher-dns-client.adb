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

with Ada.Unchecked_Conversion;
with Ada.Streams; use Ada.Streams;

with DNS_Packet_Processor.Utils; use DNS_Packet_Processor.Utils;

package body DNSCatcher.DNS.Client is
   -- Creates DNS Packet Header
   procedure Create_Header (This : in out Client) is
      Generator : Random_Transaction_ID.Generator;
   begin
      -- Generate a random number and assign it
      Random_Transaction_ID.Reset (Generator);
      This.Header.Identifier := Random_Transaction_ID.Random (Generator);

      -- We're a client, set the flags
      This.Header.Query_Response_Flag  := False;
      This.Header.Opcode               := 0; -- Client response
      This.Header.Authoritative_Answer := False;
      This.Header.Truncated            := False;
      This.Header.Recursion_Desired    := True;
      This.Header.Recursion_Available  :=
        False; -- Should be false for client requests
      This.Header.Zero               := False;
      This.Header.Authenticated_Data := True;
      This.Header.Checking_Disabled  :=
        False; -- Recursive server should check DNSSEC for us
      This.Header.Response_Code := 0;

      -- Zero the question counts
      This.Header.Question_Count          := 0;
      This.Header.Answer_Record_Count     := 0;
      This.Header.Authority_Record_Count  := 0;
      This.Header.Additional_Record_Count := 0;
   end Create_Header;

   -- Adds a query to the DNS Packet
   procedure Add_Query
     (This   : in out Client;
      QName  :        Unbounded_String;
      QType  :        RR_Types;
      QClass :        Classes)
   is
      Question : Parsed_DNS_Question;
   begin
      Question.QName  := QName;
      Question.QType  := QType;
      Question.QClass := QClass;

      This.Questions.Append (Question);
      This.Header.Question_Count := This.Header.Question_Count + 1;
   end Add_Query;

   -- Convert a DNS Name Record (this probably belongs somewhere else)
   function Create_DNS_Packet_Name_Record
     (Question : Parsed_DNS_Question)
      return Unbounded_String
   is
      subtype QAttributes is String (1 .. 2);
      DNS_Name_Record     : Unbounded_String;
      String_Offset       : Positive := 1;
      Last_Section_Offset : Integer  := 1;
      QType_Str           : QAttributes;
      QClass_Str          : QAttributes;

      function Uint8_To_Character is new Ada.Unchecked_Conversion
        (Source => Unsigned_8, Target => Character);
      function Uint16_To_String is new Ada.Unchecked_Conversion
        (Source => Unsigned_16, Target => QAttributes);
      -- Actually does the dirty work of creating a question
      function Create_QName_Record
        (Domain_Section : String)
         return String
      is
         Label : String (1 .. Domain_Section'Length + 1);
      begin
         if Domain_Section /= "."
         then
            Label (1) :=
              Uint8_To_Character (Unsigned_8 (Domain_Section'Length));
            Label
              (2 .. Domain_Section'Length + 1) := Domain_Section;
         else
            -- If this is a "." by itself, it's the terminator, and we need to
            -- do special handling
            declare
               Empty_Label : String (1 .. 1);
            begin
               Empty_Label (1) := Uint8_To_Character (Unsigned_8 (0));
               return Empty_Label;
            end;
         end if;

         return Label;
      end Create_QName_Record;
   begin
      -- Find each section of the DNS name and convert it to an encoded name
      loop
         -- If it's a period, process the section
         if Element (Question.QName, String_Offset) = '.'
         then
            DNS_Name_Record :=
              DNS_Name_Record &
              To_Unbounded_String
                (Create_QName_Record
                   (Slice
                      (Question.QName, Last_Section_Offset,
                       String_Offset - 1)));
            Last_Section_Offset := String_Offset + 1;
         end if;

         String_Offset := String_Offset + 1;
         -- If we've reached the end of the string, it forms the final section
         if String_Offset = Length (Question.QName) + 1
         then
            DNS_Name_Record :=
              DNS_Name_Record &
              To_Unbounded_String
                (Create_QName_Record
                   (Slice
                      (Question.QName, Last_Section_Offset,
                       String_Offset - 1)));
            exit; -- We're done
         end if;
      end loop;

      -- Append the final section with is zero
      DNS_Name_Record :=
        DNS_Name_Record & To_Unbounded_String (Create_QName_Record ("."));

      -- Append the QTYPE and QCLASS
      QType_Str (1 .. 2) :=
        Uint16_To_String (Htons (Unsigned_16 (Question.QType'Enum_Rep)));
      QClass_Str (1 .. 2) :=
        Uint16_To_String (Htons (Unsigned_16 (Question.QClass'Enum_Rep)));

      DNS_Name_Record :=
        DNS_Name_Record & To_Unbounded_String (QType_Str) &
        To_Unbounded_String (QClass_Str);

      return DNS_Name_Record;
   end Create_DNS_Packet_Name_Record;

   function Create_Packet
     (This   : in out Client;
      Config :        Configuration_Ptr)
      return Raw_Packet_Record_Ptr
   is
      DNS_Packet_Names : Unbounded_String;
      Outbound_Packet  : constant Raw_Packet_Record_Ptr :=
        new Raw_Packet_Record;

   begin
      for I of This.Questions
      loop
         DNS_Packet_Names :=
           DNS_Packet_Names & Create_DNS_Packet_Name_Record (I);
      end loop;

      -- Convert it to a Raw Packet format
      Outbound_Packet.Raw_Data.Header := This.Header;

      declare
         QData : constant String := To_String (DNS_Packet_Names);
         subtype QData_SEA is Stream_Element_Array (1 .. QData'Length);
         function String_To_Packet is new Ada.Unchecked_Conversion
           (Source => String, Target => QData_SEA);

      begin
         Outbound_Packet.Raw_Data.Data :=
           new Stream_Element_Array (1 .. QData'Length);
         Outbound_Packet.Raw_Data.Data.all := String_To_Packet (QData);
         Outbound_Packet.Raw_Data_Length   :=
           DNS_PACKET_HEADER_SIZE + QData'Length;
      end;

      -- Set our sender information
      Outbound_Packet.From_Address := To_Unbounded_String ("127.0.0.1");
      Outbound_Packet.From_Port    := 53;
      Outbound_Packet.To_Address   := Config.Upstream_DNS_Server;
      Outbound_Packet.To_Port      := Config.Upstream_DNS_Server_Port;
      return Outbound_Packet;
   end Create_Packet;

end DNSCatcher.DNS.Client;
