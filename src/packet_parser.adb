with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Strings;
with Ada.Unchecked_Conversion;

with DNS_Raw_Packet_Records; use DNS_Raw_Packet_Records;
with DNS_RR_Types; use DNS_RR_Types;
with DNS_Classes; use DNS_Classes;
with DNS_RCodes; use DNS_RCodes;

package body Packet_Parser is

   procedure Packet_Parser(Packet: DNS_Raw_Packet_Record_Ptr) is
      Parsed_Packet: Parsed_DNS_Packet_Ptr;
      Current_Offset : Stream_Element_Offset := 1;
   begin
      Parsed_Packet := new Parsed_DNS_Packet;
 
      -- Copy the header as it's already parsed
      Parsed_Packet.Header := Packet.all.Raw_Data.Header;
      New_Line;
      Put_Line("DNS Packet Information:");
      if Parsed_Packet.Header.Query_Response_Flag then
         Put_Line("  Client Requested");
      else
         Put_Line("  Response from Resolver");
      end if;

      if Parsed_Packet.Header.Truncated then
         Put_Line("  WARNING: Packet is truncated!");
      end if;
 
      Put("  Response Code: ");
      
      declare
         Found : Boolean := False;
      begin   
         for RCode in RCodes loop
            if Integer(Parsed_Packet.Header.Response_Code) = RCode'Enum_Rep then
              Put_Line(RCodes'Image(RCode));
              Found := True;
            end if;
            exit when Found;
         end loop;
         
         if Found /= True then
            raise Unknown_RCode;
         end if;
      end;
      
      Put_Line("  Question Count:" & Parsed_Packet.Header.Question_Count'Image);
      Put_Line("  Answer Count:" & Parsed_Packet.Header.Answer_Record_Count'Image);
      Put_Line("  Authority Count:" & Parsed_Packet.Header.Authority_Record_Count'Image);
      Put_Line("  Additional Count:" & Parsed_Packet.Header.Additional_Record_Count'Image);

      for i in 1..Parsed_Packet.Header.Question_Count loop
         Parse_Question_Record(Packet.Raw_Data.Data, Current_Offset);
      end loop;

      for i in 1..Parsed_Packet.Header.Answer_Record_Count loop
         Put_Line("Would process Answer_Record");
      end loop;

      for i in 1..Parsed_Packet.Header.Authority_Record_Count loop
         Put_Line("Would process Authority_Record");
      end loop;

      for i in 1..Parsed_Packet.Header.Additional_Record_Count loop
         Put_Line("Would process Additional_Record");
      end loop;
      
   end Packet_Parser;
     
   procedure Parse_Question_Record(Raw_Data: Raw_DNS_Packet_Data; Offset: in out Stream_Element_Offset) is
      Parsed_Question : Parsed_DNS_Question;
      QName_Section_Length : Unsigned_8;
      QName : Unbounded_String;
      
   begin
      Put_Line("    Starting Answer Parse");

      -- This is fucked up. DNS basically defines the first octlet of the length
      -- of a level of a domain section, so we need to parse that, then build a
      -- proper domain string out of it. Also, shit can be compressed. Kill me now.
      QName_Section_Length := Unsigned_8(Raw_Data.all(Offset))+1;

      loop
         declare
            subtype Domain_Section is String(1..Integer(QName_Section_Length));
            function To_Domain_Section is new Ada.Unchecked_Conversion
              (Source => Stream_Element_Array, Target => Domain_Section);
         begin
            QName := QName & To_Domain_Section(Raw_Data.all(
                                               Offset..Stream_Element_Offset(QName_Section_Length)));
         end;
         
         -- Read in the next offset
         Offset := Offset + Stream_Element_Offset(QName_Section_Length)+1;
         QName_Section_Length := Unsigned_8(Raw_Data.all(Offset-1));
         
         -- Tack on the . if this isn't the last iteration
         exit when QName_Section_Length = 0;
         QName := QName & ".";
      end loop;
      
      Parsed_Question.QName := QName;
      Offset := Offset + 1;
      
      -- The last 32-bits is the type and class
      declare
         Found : Boolean := False;
      begin
         for RR_Type in RR_Types loop
            if Unsigned_16(Raw_Data(Offset)) = RR_Type'Enum_Rep then
               Parsed_Question.QType := RR_Type;
               Found := True;
            end if;
         end loop;
         
         if Found /= True then
            raise Unknown_RR_Type;
         end if;
      end;

      Offset := Offset + 2;
      
      -- The last 32-bits is the type and class
      declare
         Found : Boolean := False;
      begin
         for Class in Classes loop
            if Unsigned_16(Raw_Data(Offset)) = Class'Enum_Rep then
               Parsed_Question.QClass := Class;
               Found := True;
            end if;
         end loop;
         
         if Found /= True then
            raise Unknown_Class;
         end if;
      end;
      
      --      Parsed_Question.QClass := DNS_Classes.DNS_Classes(Unsigned_16(Raw_Data(Offset+2..Offset+2)));
      Offset := Offset + 4;
      
      Put_Line("    QName is " & To_String(QName));
      Put_Line("    QType is " & To_String(Parsed_Question.QType));
      Put_Line("    QClass is " & To_String(Parsed_Question.QClass));
   end Parse_Question_Record;
   
end Packet_Parser;
