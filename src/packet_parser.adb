with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams;         use Ada.Streams;
with Ada.Strings;
with Ada.Unchecked_Conversion;

with DNS_Raw_Packet_Records; use DNS_Raw_Packet_Records;
with DNS_RR_Types;           use DNS_RR_Types;
with DNS_Classes;            use DNS_Classes;
with DNS_RCodes;             use DNS_RCodes;

package body Packet_Parser is

   procedure Packet_Parser (Packet : DNS_Raw_Packet_Record_Ptr) is
      Parsed_Packet  : Parsed_DNS_Packet_Ptr;
      Current_Offset : Stream_Element_Offset := 1;
   begin
      Parsed_Packet := new Parsed_DNS_Packet;

      -- Copy the header as it's already parsed
      Parsed_Packet.Header := Packet.all.Raw_Data.Header;
      New_Line;
      Put_Line ("DNS Packet Information:");
      if Parsed_Packet.Header.Query_Response_Flag
      then
         Put_Line ("  Client Requested");
      else
         Put_Line ("  Response from Resolver");
      end if;

      if Parsed_Packet.Header.Truncated
      then
         Put_Line ("  WARNING: Packet is truncated!");
      end if;

      Put ("  Response Code: ");

      declare
         Found : Boolean := False;
      begin
         for RCode in RCodes
         loop
            if Integer (Parsed_Packet.Header.Response_Code) = RCode'Enum_Rep
            then
               Put_Line (RCodes'Image (RCode));
               Found := True;
            end if;
            exit when Found;
         end loop;

         if Found /= True
         then
            raise Unknown_RCode;
         end if;
      end;

      Put_Line ("  Question Count:" & Parsed_Packet.Header.Question_Count'Image);
      Put_Line ("  Answer Count:" & Parsed_Packet.Header.Answer_Record_Count'Image);
      Put_Line ("  Authority Count:" & Parsed_Packet.Header.Authority_Record_Count'Image);
      Put_Line ("  Additional Count:" & Parsed_Packet.Header.Additional_Record_Count'Image);

      for i in 1 .. Parsed_Packet.Header.Question_Count
      loop
         Parsed_Packet.Questions.Append
           (Parse_Question_Record (Packet.Raw_Data.Data, Current_Offset));
      end loop;

      for i in 1 .. Parsed_Packet.Header.Answer_Record_Count
      loop
         Put_Line ("Parsing Answer_Record");
         Parsed_Packet.Answer.Append
           (Parse_Resource_Record_Response (Packet.Raw_Data.Data, Current_Offset));
      end loop;

      for i in 1 .. Parsed_Packet.Header.Authority_Record_Count
      loop
         Put_Line ("Would process Authority_Record");
      end loop;

      for i in 1 .. Parsed_Packet.Header.Additional_Record_Count
      loop
         Put_Line ("Would process Additional_Record");
      end loop;

   end Packet_Parser;

   function Parse_DNS_Packet_Name_Records (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Unbounded_String
   is
      Domain_Name    : Unbounded_String;
      Section_Length : Unsigned_8;
   begin
      -- This is fucked up. DNS basically defines the first octlet of the length
      -- of a level of a domain section, so we need to parse that, then build a
      -- proper domain string out of it. Also, shit can be compressed. Kill me now.
      Section_Length := Unsigned_8 (Raw_Data.all (Offset));
      Offset         := Offset + 1;

      loop
         -- Records can be compressed, denoted by Section Length of 0xff
         -- so we need to handle both cases properly

         if Section_Length /= 16#ff#
         then
            declare
               subtype Domain_Section is String (1 .. Integer (Section_Length));
               function To_Domain_Section is new Ada.Unchecked_Conversion
                 (Source => Stream_Element_Array, Target => Domain_Section);
            begin
               Domain_Name :=
                 Domain_Name &
                 To_Domain_Section
                   (Raw_Data.all (Offset .. Stream_Element_Offset (Section_Length)));
            end;
         else
            Put_Line ("Not handling compressed DNS record");
         end if;

         -- Read in the next offset
         Offset := Offset + Stream_Element_Offset (Section_Length);

         Section_Length := Unsigned_8 (Raw_Data.all (Offset));
         Offset         := Offset + 1;

         -- Tack on the . if this isn't the last iteration
         exit when Section_Length = 0;
         Domain_Name := Domain_Name & ".";
      end loop;

      Offset := Offset + 1;
      return Domain_Name;
   end Parse_DNS_Packet_Name_Records;

   function Parse_DNS_RR_Type (Raw_Data :        Raw_DNS_Packet_Data;
      Offset                            : in out Stream_Element_Offset) return RR_Types
   is
      Found_RRType : RR_Types;
   begin
      declare
         Found : Boolean := False;
      begin
         for RR_Type in RR_Types
         loop
            if Unsigned_16 (Raw_Data (Offset)) = RR_Type'Enum_Rep
            then
               Found_RRType := RR_Type;
               Found        := True;
            end if;
         end loop;

         if Found /= True
         then
            raise Unknown_RR_Type;
         end if;
      end;

      Offset := Offset + 2;
      return Found_RRType;
   end Parse_DNS_RR_Type;

   function Parse_DNS_Class (Raw_Data :        Raw_DNS_Packet_Data;
      Offset                          : in out Stream_Element_Offset) return Classes
   is
      Found_Class : Classes;
   begin
      -- The last 32-bits is the type and class
      declare
         Found : Boolean := False;
      begin
         for Class in Classes
         loop
            if Unsigned_16 (Raw_Data (Offset)) = Class'Enum_Rep
            then
               Found_Class := Class;
               Found       := True;
            end if;
         end loop;

         if Found /= True
         then
            raise Unknown_Class;
         end if;
      end;

      Offset := Offset + 2;
      return Found_Class;
   end Parse_DNS_Class;

   -- Parses the questions asked in a DNS record
   function Parse_Question_Record (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Parsed_DNS_Question
   is
      Parsed_Question : Parsed_DNS_Question;
   begin
      Put_Line ("    Starting Answer Parse");

      -- Get the QName
      Parsed_Question.QName  := Parse_DNS_Packet_Name_Records (Raw_Data, Offset);
      Parsed_Question.QType  := Parse_DNS_RR_Type (Raw_Data, Offset);
      Parsed_Question.QClass := Parse_DNS_Class (Raw_Data, Offset);
      --      Parsed_Question.QClass := DNS_Classes.DNS_Classes(Unsigned_16(Raw_Data(Offset+2..Offset+2)));

      Put_Line ("      QName is " & To_String (Parsed_Question.QName));
      Put_Line ("      QType is " & To_String (Parsed_Question.QType));
      Put_Line ("      QClass is " & To_String (Parsed_Question.QClass));

      return Parsed_Question;
   end Parse_Question_Record;

   function Parse_Resource_Record_Response (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Parsed_DNS_Resource_Record
   is
      Parsed_Response : Parsed_DNS_Resource_Record;
   begin
      Put_Line("  Starting Resource Record Parse");
      raise Unknown_Class;
      Parsed_Response.RName := Parse_DNS_Packet_Name_Records (Raw_Data, Offset);
      Put_Line("    RName is " & To_String(Parsed_Response.RName));
      return Parsed_Response;
   end Parse_Resource_Record_Response;

end Packet_Parser;
