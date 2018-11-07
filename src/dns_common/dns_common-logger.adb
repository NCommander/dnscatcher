package body DNS_Common.Logger is
   task body Logger is
      Keep_Running : Boolean := False;
   begin
      -- Processing loop
      while Keep_Running
      loop
         select
            accept Start do
               null;
            end Start;
         or
            accept Stop do
               Keep_Running := False;
            end Stop;
         or
            accept Accept_Log_Packet do
               null;
            end Accept_Log_Packet;
         or
            accept Log_Immediately (Log_Level : in Log_Levels; Component : in Unbounded_String;
               Message                        : in Unbounded_String) do
               null;
            end Log_Immediately;
         end select;
      end loop;

      -- Idling loop ready for shutdown
      while Keep_Running = False
      loop
         select
            accept Start do
               null;
            end Start;
         end select;
      end loop;
   end Logger;
end DNS_Common.Logger;
