with Ada.Text_IO; use Ada.Text_IO;

with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Ctrl_C; use GNAT.Ctrl_C;

with Packet_Catcher;
with Signal_Handlers;

procedure Main is
begin
   Install_Handler (Handler => Signal_Handlers.SIGINT_Handler'Access);
   Packet_Catcher.Run_Catcher;
exception
   when Error : others =>
      begin
         Put (Standard_Error, "Unknown error: ");
         Put_Line (Standard_Error, Exception_Information (Error));
         Packet_Catcher.Stop_Catcher;
      end;
end Main;
