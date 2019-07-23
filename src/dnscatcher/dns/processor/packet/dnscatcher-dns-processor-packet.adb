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

with Ada.Strings;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with DNSCatcher.Utils; use DNSCatcher.Utils;

with System;

package body DNSCatcher.DNS.Processor.Packet is

   function Packet_Parser
     (Logger : Logger_Message_Packet_Ptr;
      Packet : Raw_Packet_Record_Ptr)
      return Parsed_DNS_Packet_Ptr
   is
      Parsed_Packet  : Parsed_DNS_Packet_Ptr;
      Current_Offset : Stream_Element_Offset := 1;
   begin
      Parsed_Packet := new Parsed_DNS_Packet;

      Logger.Push_Component ("Packet Parser");
      -- Copy the header as it's already parsed
      Parsed_Packet.Header := Packet.all.Raw_Data.Header;

      if Parsed_Packet.Header.Query_Response_Flag
      then
         Logger.Log_Message (DEBUG, "Client Requested DNS Packet");
      else
         Logger.Log_Message (DEBUG, "Server Response DNS Packet");
      end if;

      if Parsed_Packet.Header.Truncated
      then
         Logger.Log_Message (DEBUG, "Packet is truncated!");
      end if;

      declare
         Found : Boolean := False;
      begin
         for RCode in RCodes
         loop
            if Integer (Parsed_Packet.Header.Response_Code) = RCode'Enum_Rep
            then
               Logger.Log_Message
                 (DEBUG, "Response code: " & RCodes'Image (RCode));
               Found := True;
            end if;
            exit when Found;
         end loop;

         if not Found
         then
            raise Unknown_RCode;
         end if;
      end;

      Logger.Log_Message
        (DEBUG, "Question Count:" & Parsed_Packet.Header.Question_Count'Image);
      Logger.Log_Message
        (DEBUG,
         "Answer Count:" & Parsed_Packet.Header.Answer_Record_Count'Image);
      Logger.Log_Message
        (DEBUG,
         "Authority Count:" &
         Parsed_Packet.Header.Authority_Record_Count'Image);
      Logger.Log_Message
        (DEBUG,
         "Additional Count:" &
         Parsed_Packet.Header.Additional_Record_Count'Image);

      for i in 1 .. Parsed_Packet.Header.Question_Count
      loop
         Parsed_Packet.Questions.Append
           (Parse_Question_Record
              (Logger, Packet.Raw_Data.Data, Current_Offset));
      end loop;

      for i in 1 .. Parsed_Packet.Header.Answer_Record_Count
      loop
         Parsed_Packet.Answer.Append
           (Parse_Resource_Record_Response
              (Logger, Packet.Raw_Data, Current_Offset));
      end loop;

      for i in 1 .. Parsed_Packet.Header.Authority_Record_Count
      loop
         Parsed_Packet.Authority.Append
           (Parse_Resource_Record_Response
              (Logger, Packet.Raw_Data, Current_Offset));
      end loop;

      for i in 1 .. Parsed_Packet.Header.Additional_Record_Count
      loop
         Parsed_Packet.Additional.Append
           (Parse_Resource_Record_Response
              (Logger, Packet.Raw_Data, Current_Offset));
      end loop;

      Logger.Pop_Component;

      return Parsed_Packet;
   end Packet_Parser;

   -- I apologize in advance, this function is a real mindfuck.
   function Parse_DNS_Packet_Name_Records
     (Raw_Data :        Raw_DNS_Packet_Data_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Unbounded_String
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
      -- This is fucked up. DNS basically defines the first octlet of the
      -- length of a level of a domain section, so we need to parse that, then
      -- build a proper domain string out of it. Also, shit can be compressed.
      -- Kill me now.
      Section_Length := Unsigned_8 (Raw_Data.all (Offset));
      if Section_Length = 0
      then
         -- It is possible for the section length to be zero for some record
         -- requests or dealing with the root. Thus just return blank
         Offset := Offset + 1;
         return To_Unbounded_String ("");
      end if;

      loop
         -- Records can be compressed, denoted by Section Length of leading
         -- binary bytes 11 so we need to handle both cases properly
         --
         -- This is flipping bullshit, and I dunno if I should be pissed at the
         -- Ada guys or the DNS ones. Basically, the top two bits of the offset
         -- represent if its compressed or not, with the rest being used as an
         -- offset. Normally, you can handle that via byte order and shit, but
         -- Ada gets confused when doing this because you basically have you
         -- most significant bytes at the other end.
         --
         -- Given we're already in the correct memory format, we'll just use
         -- bitwise ops because I've already raged hard enough here

         if (Section_Length and 16#c0#) = 0 -- Not compressed
         then
            Offset := Offset + 1; -- Move past the length

            declare
               subtype Domain_Section is
                 String (1 .. Integer (Section_Length));
               function To_Domain_Section is new Ada.Unchecked_Conversion
                 (Source => Stream_Element_Array, Target => Domain_Section);
            begin
               Domain_Name := Domain_Name & To_Domain_Section (Raw_Data.all
                      (Offset .. Stream_Element_Offset (Section_Length)));

               Offset := Offset + Stream_Element_Offset (Section_Length);
            end;

         elsif (Section_Length and 16#c0#) /= 0
         then
            -- Standard compression is nuts. We have a pointer within the
            -- packet that basically contains a link to the string segment
            -- we need next. Let's see if we can grab it and decode it

            declare
               Decompressed_Domain_String : Unbounded_String;
               function Get_Byte_Fixed_Header is new Ada.Unchecked_Conversion
                 (Source => Stream_Element_Array, Target => Packer_Pointer);
            begin
               -- Oh, and for more fuckery, the pointer is 16-bit ...
               Packet_Ptr := Get_Byte_Fixed_Header (Raw_Data.all
                      (Offset .. Offset + 1)); -- Make the top bytes vanish

               -- We subtract 12-1 for the packet header
               Packet_Ptr.Packet_Offset :=
                 (Packet_Ptr.Packet_Offset and 16#3fff#) - 11;

               -- Sanity check ourselves
               if (Section_Length and 2#11#) /= 0
               then
                  -- Should never happen but you never know ...
                  raise Unknown_Compression_Method;
               end if;

               -- Now we need to decode the whole old string ... ugh
               Decompressed_Domain_String :=
                 Parse_DNS_Packet_Name_Records
                   (Raw_Data,
                    Stream_Element_Offset (Packet_Ptr.Packet_Offset));
               Offset := Offset + 2;
               return Domain_Name & Decompressed_Domain_String;
            end;
         else
            -- Welp, unknown compression, bail out
            raise Unknown_Compression_Method;
         end if;

         Section_Length := Unsigned_8 (Raw_Data.all (Offset));
         exit when Section_Length = 0;

         -- Tack on the . if this isn't the last iteration
         Domain_Name := Domain_Name & ".";

      end loop;

      Offset := Offset + 1;
      return Domain_Name;
   end Parse_DNS_Packet_Name_Records;

   function Parse_DNS_RR_Type
     (Raw_Data :        Raw_DNS_Packet_Data_Ptr;
      Offset   : in out Stream_Element_Offset)
      return RR_Types
   is
      Found_RRType : RR_Types;
      RR_Type_Raw  : Unsigned_16;
   begin
      declare
         Found : Boolean := False;
      begin
         RR_Type_Raw := Read_Unsigned_16 (Raw_Data, Offset);
         for RR_Type in RR_Types
         loop
            if RR_Type_Raw = RR_Type'Enum_Rep
            then
               Found_RRType := RR_Type;
               Found        := True;
            end if;
         end loop;

         if not Found
         then
            raise Unknown_RR_Type;
         end if;
      end;

      return Found_RRType;
   end Parse_DNS_RR_Type;

   function Parse_DNS_Class
     (Raw_Data :        Raw_DNS_Packet_Data_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Classes
   is
      Found_Class : Classes;
      Raw_Class   : Unsigned_16;
   begin
      -- The last 32-bits is the type and class
      declare
         Found : Boolean := False;
      begin
         Raw_Class := Read_Unsigned_16 (Raw_Data, Offset);

         for Class in Classes
         loop
            if Raw_Class = Class'Enum_Rep
            then
               Found_Class := Class;
               Found       := True;
            end if;
         end loop;

         if not Found
         then
            raise Unknown_Class;
         end if;
      end;
      return Found_Class;
   end Parse_DNS_Class;

   -- Parses the questions asked in a DNS record
   function Parse_Question_Record
     (Logger   :        Logger_Message_Packet_Ptr;
      Raw_Data :        Raw_DNS_Packet_Data_Ptr;
      Offset   : in out Stream_Element_Offset)
      return Parsed_DNS_Question
   is
      Parsed_Question : Parsed_DNS_Question;
   begin
      Logger.Push_Component ("Question Parser");

      -- Get the QName
      Parsed_Question.QName :=
        Parse_DNS_Packet_Name_Records (Raw_Data, Offset);
      Parsed_Question.QType  := Parse_DNS_RR_Type (Raw_Data, Offset);
      Parsed_Question.QClass := Parse_DNS_Class (Raw_Data, Offset);
      --      Parsed_Question.QClass := DNS_Classes.DNS_Classes(Unsigned_16(Raw_Data(Offset+2..Offset+2)));

      Logger.Log_Message
        (DEBUG, "QName is " & To_String (Parsed_Question.QName));
      Logger.Log_Message
        (DEBUG, "QType is " & To_String (Parsed_Question.QType));
      Logger.Log_Message
        (DEBUG, "QClass is " & To_String (Parsed_Question.QClass));

      Logger.Pop_Component;

      return Parsed_Question;
   end Parse_Question_Record;

   function Parse_Resource_Record_Response
     (Logger :        Logger_Message_Packet_Ptr;
      Packet :        Raw_DNS_Packet;
      Offset : in out Stream_Element_Offset)
      return Parsed_RData_Access
   is
      Parsed_Response : Parsed_DNS_Resource_Record;
      Raw_Data        : Raw_DNS_Packet_Data_Ptr;

      RData_Length          : Unsigned_16;
      Parsed_RData_Response : Parsed_RData_Access;
   begin
      -- Create direct reference to the raw data
      Raw_Data := Packet.Data;

      Logger.Push_Component ("RRecord Parser");
      Parsed_Response.RName :=
        Parse_DNS_Packet_Name_Records (Raw_Data, Offset);
      Parsed_Response.RType  := Parse_DNS_RR_Type (Raw_Data, Offset);
      Parsed_Response.RClass := Read_Unsigned_16 (Raw_Data, Offset);
      Parsed_Response.TTL    := Read_Unsigned_32 (Raw_Data, Offset);

      -- What follows is the length as a 16 bit integer, then the RData which
      -- needs an unchecked conversion
      RData_Length := Read_Unsigned_16 (Raw_Data, Offset);

      Logger.Log_Message
        (DEBUG, "RName is " & To_String (Parsed_Response.RName));
      Logger.Log_Message
        (DEBUG, "RType is " & To_String (Parsed_Response.RType));
      Logger.Log_Message (DEBUG, "RClass is" & Parsed_Response.RClass'Image);
      Logger.Log_Message (DEBUG, "TTL is" & Parsed_Response.TTL'Image);
      Logger.Log_Message (DEBUG, "RDLength is" & RData_Length'Image);

      -- RData parsing often needs the whole packet if DNS Compression is used,
      -- so give it a copy, and set the offset for them.
      Parsed_Response.Raw_Packet :=
        new Stream_Element_Array (1 .. Raw_Data'Last);
      Parsed_Response.Raw_Packet.all := Raw_Data.all;
      Parsed_Response.RData_Offset   := Offset;

      declare
         subtype RData is String (1 .. Integer (RData_Length));
         function To_RData is new Ada.Unchecked_Conversion
           (Source => Stream_Element_Array, Target => RData);
      begin
         Parsed_Response.RData := To_Unbounded_String (To_RData (Raw_Data
                   (Offset .. Stream_Element_Offset (RData_Length))));
         Offset := Offset + Stream_Element_Offset (RData_Length);
      end;

      Parsed_RData_Response :=
        To_Parsed_RData (Packet.Header, Parsed_Response);
      Logger.Log_Message
        (DEBUG, "RData is " & Parsed_RData_Response.RData_To_String);
      Logger.Pop_Component;

      return Parsed_RData_Response;
   end Parse_Resource_Record_Response;

   procedure Free_Parsed_DNS_Packet (Packet : in out Parsed_DNS_Packet_Ptr) is
      procedure Free_Ptr is new Ada.Unchecked_Deallocation
        (Object => Parsed_DNS_Packet, Name => Parsed_DNS_Packet_Ptr);
   begin
      Packet.Questions.Clear;

      for I of Packet.Answer
      loop
         I.Delete;
      end loop;

      for I of Packet.Authority
      loop
         I.Delete;
      end loop;

      for I of Packet.Additional
      loop
         I.Delete;
      end loop;

      Packet.Answer.Clear;
      Packet.Authority.Clear;
      Packet.Additional.Clear;

      Free_Ptr (Packet);
   end Free_Parsed_DNS_Packet;

end DNSCatcher.DNS.Processor.Packet;
