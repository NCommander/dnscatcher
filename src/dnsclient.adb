with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;
with DNS_Client;
with DNS_Common.Config;
with DNS_Common.Logger;                      use DNS_Common.Logger;
with DNS_Core_Constructs;                    use DNS_Core_Constructs;
with DNS_Core_Constructs.Utils;              use DNS_Core_Constructs.Utils;
with DNS_Core_Constructs.Raw_Packet_Records; use DNS_Core_Constructs.Raw_Packet_Records;
with DNS_Packet_Processor;                   use DNS_Packet_Processor;
with DNS_Packet_Processor.Utils; use DNS_Packet_Processor.Utils;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

procedure DNSClient is
   DClient         : DNS_Client.Client;
   Capture_Config  : DNS_Common.Config.Configuration_Ptr;
   Logger_Task     : DNS_Common.Logger.Logger;
   Parsed_Packet   : Parsed_DNS_Packet_Ptr;
   Outbound_Packet : Raw_Packet_Record_Ptr;
   Logger_Packet      : DNS_Common.Logger.Logger_Message_Packet_Ptr;

   procedure Free_DNSCatacher_Config is new Ada.Unchecked_Deallocation
     (Object => DNS_Common.Config.Configuration, Name => DNS_Common.Config.Configuration_Ptr);

begin
   Capture_Config                          := new DNS_Common.Config.Configuration;
   Capture_Config.Local_Listen_Port        := 53;
   Capture_Config.Upstream_DNS_Server      := To_Unbounded_String ("4.2.2.2");
   Capture_Config.Upstream_DNS_Server_Port := 53;

   -- Configure the logger
   Capture_Config.Logger_Config.Log_Level := DEBUG;
   Capture_Config.Logger_Config.Use_Color := True;

   Logger_Task.Initialize (Capture_Config.Logger_Config);
   Logger_Task.Start;
   Logger_Packet := new Logger_Message_Packet;

   DClient.Create_Header;
   DClient.Add_Query (To_Unbounded_String ("testest.casadevall.pro"), A, INternet);

   Outbound_Packet := DClient.Create_Packet;
   Parsed_Packet := Packet_Parser (Logger_Packet, Outbound_Packet);
   Logger_Queue.Add_Packet (Logger_Packet);

   Free_Raw_Packet_Record_Ptr(Outbound_Packet);
   Free_Parsed_DNS_Packet(Parsed_Packet);

   Logger_Task.Stop;
   Free_DNSCatacher_Config (Capture_Config);
exception
   when Error : others =>
      begin
         Put (Standard_Error, "Unknown error: ");
         Put_Line (Standard_Error, Exception_Information (Error));
         Logger_Task.Stop;
      end;

end DNSClient;
