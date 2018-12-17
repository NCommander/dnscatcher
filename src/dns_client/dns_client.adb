with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body DNS_Client is
   -- Creates DNS Packet Header
   procedure Create_Header (This : in out Client) is
      Generator : Random_Transaction_ID.Generator;
   begin
      -- Generate a random number and assign it
      Random_Transaction_ID.Reset (Generator);
      This.Header.Identifier := Random_Transaction_ID.Random (Generator);

      -- We're a client, set the flags
      This.Header.Query_Response_Flag := False;
      This.Header.Opcode              := 0; -- Client response
      This.Header.Truncated           := False;
      This.Header.Recursion_Desired   := True;
      This.Header.Recursion_Available := False; -- Should be false for client requests
      This.Header.Zero                := False;
      This.Header.Authenticated_Data  := True;
      This.Header.Checking_Disabled   := False; -- Recursive server should check DNSSEC for us
      This.Header.Response_Code       := 0;

      -- Zero the question counts
      This.Header.Question_Count          := 0;
      This.Header.Answer_Record_Count     := 0;
      This.Header.Authority_Record_Count  := 0;
      This.Header.Additional_Record_Count := 0;
   end Create_Header;

   -- Adds a query to the DNS Packet
   procedure Add_Query (This : in out Client; QName : Unbounded_String; QType : RR_Types;
      QClass                 :        Classes)
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
   function Create_DNS_Packet_Name_Record (Question : Parsed_DNS_Question) return Unbounded_String
   is
      DNS_Name_Record     : Unbounded_String;
      String_Offset       : Positive := 1;
      Last_Section_Offset : Integer  := 1;

      -- Actually does the dirty work of creating a question
      function Create_QName_Record (Domain_Section : String) return String is
         Label : String (1 .. Domain_Section'Length + 1);
         function Uint8_To_Character is new Ada.Unchecked_Conversion (Source => Unsigned_8,
            Target                                                           => Character);
      begin
         Label (1) := Uint8_To_Character (Unsigned_8 (Domain_Section'Length));
         Label (2 .. Domain_Section'Length + 1) := Domain_Section;
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
                   (Slice (Question.QName, Last_Section_Offset, String_Offset - 1)));
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
                   (Slice (Question.QName, Last_Section_Offset, String_Offset - 1)));
            exit; -- We're done
         end if;
      end loop;

      Put_Line ("DNS Name Record is " & To_String(DNS_Name_Record));
      return DNS_Name_Record;
   end Create_DNS_Packet_Name_Record;

   procedure Run_Query (This : in out Client) is
      DNS_Packet_Name : Unbounded_String;
   begin
      for I of This.Questions
      loop
         DNS_Packet_Name := Create_DNS_Packet_Name_Record (I);
      end loop;
   end Run_Query;

end DNS_Client;
