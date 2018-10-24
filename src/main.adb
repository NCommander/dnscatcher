with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Exceptions;
use Ada.Exceptions;

with GNAT.Sockets;

with Packet_Catcher;

procedure Main is

begin
   Packet_Catcher.Run_Catcher;
exception
   when Error : GNAT.Sockets.Socket_Error =>
      begin
         Put(Standard_Error, "Failed to bind socket: ");
         Put_Line(Standard_Error, Exception_Information(Error));
         Packet_Catcher.Stop_Catcher;
      end;
   when Error: others =>
      begin
         Put(Standard_Error, "Unknown error: ");
         Put_Line(Standard_Error, Exception_Information(Error));
         Packet_Catcher.Stop_Catcher;
      end;
end Main;
