with Ada.Text_IO; use Ada.Text_IO;

with Packet_Catcher;

package body Signal_Handlers is
   procedure SIGINT_Handler is
   begin
      Put_Line ("Beginning Clean Shutdown");
      Packet_Catcher.Stop_Catcher;
   end SIGINT_Handler;
end Signal_Handlers;
