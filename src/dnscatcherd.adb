with Ada.Text_IO; use Ada.Text_IO;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;

with GNAT.Ctrl_C; use GNAT.Ctrl_C;

with Packet_Catcher;
with Signal_Handlers;

with DNS_Common.Config;
procedure DNSCatcherD is
begin
   Install_Handler (Handler => Signal_Handlers.SIGINT_Handler'Access);
   Packet_Catcher.Run_Catcher;
exception
   when Error : Ada.IO_Exceptions.Name_Error =>
      begin
         Put (Standard_Error, "FATAL: Unable to open config file: ");
         Put_Line (Standard_Error, Exception_Message (Error));
         Packet_Catcher.Stop_Catcher;
      end;
   when Error : DNS_Common.Config.Missing_Mandatory_Config_Option =>
      begin
         Put (Standard_Error, "FATAL: Missing mandatory config line. ");
         Put_Line (Standard_Error, Exception_Message (Error));
         Packet_Catcher.Stop_Catcher;
      end;
   when Error : DNS_Common.Config.Malformed_Line =>
      begin
         Put (Standard_Error, "FATAL: Unable to parse config file. ");
         Put_Line (Standard_Error, Exception_Message (Error));
         Packet_Catcher.Stop_Catcher;
      end;
   when Error : others =>
      begin
         Put (Standard_Error, "FATAL: Unknown error: ");
         Put_Line (Standard_Error, Exception_Information (Error));
         Packet_Catcher.Stop_Catcher;
      end;
end DNSCatcherD;
