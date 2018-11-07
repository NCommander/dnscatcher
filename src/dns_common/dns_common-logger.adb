package body DNS_Common.Logger is
   task body Logger is
      Keep_Running : Boolean := False;
   begin
      -- Processing loop
      while Keep_Running loop
         select
            accept Start do
               Keep_Running := False;
            end Start;
         end select;
      end loop;
   
      -- Idling loop ready for shutdown
      while Keep_Running = False loop
         select
            accept Start do
               null;
            end Start;
         end select;   
      end loop;
   end Logger;
end DNS_Common.Logger;
