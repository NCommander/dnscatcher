with System;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams;         use Ada.Streams;
with Ada.Strings;
with Ada.Unchecked_Conversion;

with DNS_Core_Constructs.Raw_Packet_Records; use DNS_Core_Constructs.Raw_Packet_Records;
with DNS_Core_Constructs;                    use DNS_Core_Constructs;

package body DNS_Packet_Processor is

   procedure Packet_Parser (Packet : Raw_Packet_Record_Ptr) is
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
         Put_Line ("Parsing Question");
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
         Put_Line ("Parsing Authority_Record");
         Parsed_Packet.Authority.Append
           (Parse_Resource_Record_Response (Packet.Raw_Data.Data, Current_Offset));
      end loop;

      for i in 1 .. Parsed_Packet.Header.Additional_Record_Count
      loop
         Put_Line ("Parsing Additional_Record");
         Parsed_Packet.Additional.Append
           (Parse_Resource_Record_Response (Packet.Raw_Data.Data, Current_Offset));
      end loop;

   end Packet_Parser;

   -- I apologize in advance, this function is a real mindfuck.
   function Parse_DNS_Packet_Name_Records (Raw_Data :        Raw_DNS_Packet_Data;
      Offset : in out Stream_Element_Offset) return Unbounded_String
   is
      Domain_Name    : Unbounded_String;
      Section_Length : Unsigned_8;
      type Packer_Pointer is record
         Packet_Offset : Unsigned_16;
      end record;
      for Packer_Pointer'Bit_Order use System.High_Order_First;
      for Packer_Pointer'Scalar_Storage_Order use System.High_Order_First;
      pragma Pack (Packer_Pointer);
      Packet_Ptr : Packer_Pointer;
   begin
      -- This is fucked up. DNS basically defines the first octlet of the length
      -- of a level of a domain section, so we need to parse that, then build a
      -- proper domain string out of it. Also, shit can be compressed. Kill me now.
      Section_Length := Unsigned_8 (Raw_Data.all (Offset));
      if Section_Length = 0
      then
         -- It is possible for the section length to be zero for some
         -- record requests or dealing with the root. Thus just return
         -- blank
         Offset := Offset + 1;
         return To_Unbounded_String ("");
      end if;

      loop
         -- Records can be compressed, denoted by Section Length of leading binary bytes 11
         -- so we need to handle both cases properly
         --
         -- This is flipping bullshit, and I dunno if I should be pissed at the Ada guys
         -- or the DNS ones. Basically, the top two bits of the offset represent if its
         -- compressed or not, with the rest being used as an offset. Normally, you can
         -- handle that via byte order and shit, but Ada gets confused when doing this
         -- because you basically have you most significant bytes at the other end.
         --
         -- Given we're already in the correct memory format, we'll just use bitwise ops
         -- because I've already raged hard enough here

         if (Section_Length and 2#11#) /= 0 -- Not compressed
         then
            Offset := Offset + 1; -- Move past the length

            declare
               subtype Domain_Section is String (1 .. Integer (Section_Length));
               function To_Domain_Section is new Ada.Unchecked_Conversion
                 (Source => Stream_Element_Array, Target => Domain_Section);
            begin
               Domain_Name :=
                 Domain_Name &
                 To_Domain_Section
                   (Raw_Data.all (Offset .. Stream_Element_Offset (Section_Length)));

               Offset := Offset + Stream_Element_Offset (Section_Length);
            end;

         elsif (Section_Length and 2#11#) = 0
         then
            -- Standard compression is nuts. We have a pointer within the packet that basically
            -- contains a link to the string segment we need next. Let's see if we can grab it and
            -- decode it

            declare
               Old_Section_Length         : Unsigned_8;
               Decompressed_Domain_String : Unbounded_String;
               function Get_Byte_Fixed_Header is new Ada.Unchecked_Conversion
                 (Source => Stream_Element_Array, Target => Packer_Pointer);

               subtype Domain_Section is String (1 .. Integer (Section_Length));
               function To_Domain_Section is new Ada.Unchecked_Conversion
                 (Source => Stream_Element_Array, Target => Domain_Section);
            begin
               -- Oh, and for more fuckery, the pointer is 16-bit ...
               Packet_Ptr :=
                 Get_Byte_Fixed_Header
                   (Raw_Data.all (Offset - 1 .. Offset)); -- Make the top bytes vanish

               -- We subtract 12-1 for the packet header
               Packet_Ptr.Packet_Offset := (Packet_Ptr.Packet_Offset and 16#3fff#) - 11;
               Old_Section_Length       :=
                 Unsigned_8 (Raw_Data.all (Stream_Element_Offset (Packet_Ptr.Packet_Offset)));

               -- Sanity check ourselves
               if (Section_Length and 2#11#) /= 0
               then
                  -- Should never happen but you never know ...
                  raise Unknown_Compression_Method;
               end if;

               -- Now we need to decode the whole old string ... ugh
               Decompressed_Domain_String :=
                 Parse_DNS_Packet_Name_Records
                   (Raw_Data, Stream_Element_Offset (Packet_Ptr.Packet_Offset));
               Offset := Offset + 2;
               return Decompressed_Domain_String;
            end;
         else
            -- Welp, unknown compression, bail out
            raise Unknown_Compression_Method;
         end if;

         Section_Length := Unsigned_8 (Raw_Data.all (Offset));
         --Put_Line(To_String(Domain_Name));
         exit when Section_Length = 0;

         -- Tack on the . if this isn't the last iteration
         Domain_Name := Domain_Name & ".";

      end loop;

      Offset := Offset + 2;
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
      RData_Length    : Unsigned_16;
   begin
      Put_Line ("  Starting Resource Record Parse");
      Parsed_Response.RName  := Parse_DNS_Packet_Name_Records (Raw_Data, Offset);
      Parsed_Response.RType  := Parse_DNS_RR_Type (Raw_Data, Offset);
      Parsed_Response.RClass := Unsigned_16 (Raw_Data (Offset));
      Offset                 := Offset + 2;

      Parsed_Response.TTL := Unsigned_32 (Raw_Data (Offset));
      Offset              := Offset + 4;

      -- What follows is the length as a 16 bit integer, then the RData which needs an
      -- unchecked conversion
      RData_Length := Unsigned_16 (Raw_Data (Offset));
      Offset       := Offset + 2;

      Put_Line ("    RName is " & To_String (Parsed_Response.RName));
      Put_Line ("    RType is " & To_String (Parsed_Response.RType));
      Put_Line ("    RClass is" & Parsed_Response.RClass'Image);
      Put_Line ("    TTL is" & Parsed_Response.TTL'Image);
      Put_Line ("    RDLength is" & RData_Length'Image);

      declare
         subtype RData is String (1 .. Integer (RData_Length));
         function To_RData is new Ada.Unchecked_Conversion (Source => Stream_Element_Array,
            Target                                                 => RData);
      begin
         Parsed_Response.RData :=
           To_Unbounded_String
             (To_RData (Raw_Data (Offset .. Stream_Element_Offset (RData_Length))));
         Offset := Offset + Stream_Element_Offset (RData_Length);
      end;

      Put_Line ("    RData is " & To_String (Parsed_Response.RData));
      return Parsed_Response;
   end Parse_Resource_Record_Response;

end DNS_Packet_Processor;
