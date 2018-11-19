pragma Ada_2012;

with AUnit.Assertions;

with Ada.Directories;       use Ada.Directories;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with DNS_Core_Constructs;  use DNS_Core_Constructs;
with DNS_Core_Constructs.Utils; use DNS_Core_Constructs.Utils;

with DNS_Core_Constructs.Raw_Packet_Records; use DNS_Core_Constructs.Raw_Packet_Records;
with DNS_Packet_Processor; use DNS_Packet_Processor;

with DNS_RData_Processor; use DNS_RData_Processor;

with Interfaces.C.Extensions; use Interfaces.C.Extensions;

package body Test_Packet_Parser is
   procedure Free_DNSCatacher_Config is new Ada.Unchecked_Deallocation
     (Object => DNS_Common.Config.Configuration, Name => DNS_Common.Config.Configuration_Ptr);

   procedure Set_Up_Case (T : in out Packet_Parser_Test) is
   begin
      T.Capture_Config                          := new DNS_Common.Config.Configuration;
      T.Capture_Config.Local_Listen_Port        := 53;
      T.Capture_Config.Upstream_DNS_Server      := To_Unbounded_String ("4.2.2.2");
      T.Capture_Config.Upstream_DNS_Server_Port := 53;

      -- Configure the logger
      T.Capture_Config.Logger_Config.Log_Level := DEBUG;
      T.Capture_Config.Logger_Config.Use_Color := True;

      T.Logger_Task.Initialize (T.Capture_Config.Logger_Config);
      T.Logger_Task.Start;
   end Set_Up_Case;

   procedure Tear_Down_Case (T : in out Packet_Parser_Test) is
   begin
      T.Logger_Task.Stop;
      Free_DNSCatacher_Config (T.Capture_Config);
   end Tear_Down_Case;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Packet_Parser_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parse_A_Record'Access, "Test Parse A Record");
      Register_Routine (T, Test_2'Access, "Example2");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   pragma Warnings (Off, "formal parameter ""T"" is not referenced");
   function Name (T : Packet_Parser_Test) return Message_String is
   begin
      return Format ("Packet Parser Test");
   end Name;
   pragma Warnings (On, "formal parameter ""T"" is not referenced");

   ---------------------
   -- Test_Simple_Add --
   ---------------------

   function Load_Binary_DNS_Dump(File: String) return Raw_Packet_Record_Ptr is
      Input_File         : Stream_IO.File_Type;
      Input_Stream       : Stream_Access;
      Packet_Size        : Stream_Element_Offset;
      Read_Packet        : Stream_Element_Offset;
      Full_Packet        : Stream_Element_Array_Ptr;
      Inbound_Packet     : constant Raw_Packet_Record_Ptr := new Raw_Packet_Record;
   begin
      Ada.Streams.Stream_IO.Open (Input_File, In_File, File);

      -- We need to load and convert the header seperately because Ada sizing
      -- doesn't account for Pack correct.
      Input_Stream := Ada.Streams.Stream_IO.Stream (Input_File);

      -- Load in the A record and try to parse it
      Packet_Size := Stream_Element_Offset(Size(File));
      Full_Packet := new Stream_Element_Array(1..Packet_Size);
      Input_Stream.Read (Full_Packet.all, Read_Packet);

      Inbound_Packet.Raw_Data.Header :=
        SEA_To_DNS_Packet_Header
          (Full_Packet (Full_Packet'First .. DNS_PACKET_HEADER_SIZE));

      Inbound_Packet.Raw_Data.Data := new Stream_Element_Array(1..Packet_Size-DNS_PACKET_HEADER_SIZE);
      Inbound_Packet.Raw_Data.Data.all := Full_Packet(DNS_PACKET_HEADER_SIZE+1..Packet_Size);

      Free_Stream_Element_Array_Ptr(Full_Packet);
      Close(Input_File);

      return Inbound_Packet;
   end;

   procedure Test_Parse_A_Record (T : in out Test_Cases.Test_Case'Class) is
      Logger_Packet      : DNS_Common.Logger.Logger_Message_Packet_Ptr;
      Parsed_Packet      : Parsed_DNS_Packet_Ptr;
      Inbound_Packet     : Raw_Packet_Record_Ptr := new Raw_Packet_Record;
      Question           : Parsed_DNS_Question;
      Answer             : Parsed_RData_Access;
   begin
      Logger_Packet := new Logger_Message_Packet;

      Inbound_Packet := Load_Binary_DNS_Dump("./tests/data/udp_a_record.bin");
      Parsed_Packet := Packet_Parser (Logger_Packet, Inbound_Packet);
      Logger_Queue.Add_Packet (Logger_Packet);

      -- Verify the data we got from the packet matches what we expect
      AUnit.Assertions.Assert((Integer(Parsed_Packet.Header.Question_Count) = Integer(Parsed_Packet.Questions.Length)),
                              "Question Count Mismatch!");

      -- Verify the question section
      Question := Parsed_Packet.Questions(1);
      AUnit.Assertions.Assert(To_String(Question.QName), "apple.com", "Incorrect QNAME on decode!");
      AUnit.Assertions.Assert((Question.QType = DNS_Core_Constructs.A), "Incorrect QTYPE on decode!");
      AUnit.Assertions.Assert((Question.QClass = DNS_Core_Constructs.INternet), "Incorrect QCLASS on decode!");

      -- Verify the response section
      AUnit.Assertions.Assert((Integer(Parsed_Packet.Header.Answer_Record_Count) = Integer(Parsed_Packet.Answer.Length)),
                              "Answer Count Mismatch!");

      Answer := Parsed_Packet.Answer.Element(1);
      AUnit.Assertions.Assert(To_String(Answer.RName), "apple.com", "Incoorect RName!");
      AUnit.Assertions.Assert((Answer.RType = DNS_Core_Constructs.A), "Incoorect RName!");
      AUnit.Assertions.Assert((Answer.TTL = 3190), "Incorrect TTL!");
      AUnit.Assertions.Assert(Answer.RData_To_String, "17.172.224.47", "RData is incorrect");

      Answer := Parsed_Packet.Answer.Element(2);
      AUnit.Assertions.Assert(To_String(Answer.RName), "apple.com", "Incoorect RName!");
      AUnit.Assertions.Assert((Answer.RType = DNS_Core_Constructs.A), "Incoorect RName!");
      AUnit.Assertions.Assert((Answer.TTL = 3190), "Incorrect TTL!");
      AUnit.Assertions.Assert(Answer.RData_To_String, "17.178.96.59", "RData is incorrect");

      Answer := Parsed_Packet.Answer.Element(3);
      AUnit.Assertions.Assert(To_String(Answer.RName), "apple.com", "Incoorect RName!");
      AUnit.Assertions.Assert((Answer.RType = DNS_Core_Constructs.A), "Incoorect RName!");
      AUnit.Assertions.Assert((Answer.TTL = 3190), "Incorrect TTL!");
      AUnit.Assertions.Assert(Answer.RData_To_String, "17.142.160.59", "RData is incorrect");

   end Test_Parse_A_Record;

   procedure Test_2 (T : in out Test_Cases.Test_Case'Class) is
   begin
      null;
   end Test_2;

end Test_Packet_Parser;
