with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets;          use GNAT.Sockets;

package Packet_Catcher is
   UDP_MAX_SIZE : constant Integer := 65535;

   procedure Run_Catcher;
   procedure Stop_Catcher;

   -- Protected Elements
end Packet_Catcher;
