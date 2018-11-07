with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package DNS_Common.Logger is
   -- Handles logging operations in a sane and consistent way within
   -- the Catcher
   
   type Log_Levels is (EMERGERENCY,
                       ALERT,
                       CRITICAL,
                       ERROR,
                       WARNING,
                       NOTICE,
                       INFO,
                       DEBUG);
   for Log_Levels use (EMERGERENCY => 0,
                       ALERT => 1,
                       CRITICAL => 2,
                       ERROR => 3,
                       WARNING => 4,
                       NOTICE => 5,
                       INFO => 6,
                       DEBUG => 7);

   type Logger_Configuration is record
      -- Configures the logger settings
      Log_Level: Log_Levels; -- Sets the message types to be shown
      Use_ANSI_Color: Boolean;
   end record;

   -- Contains a message to log
   type Log_Message is record
      Log_Level: Log_Levels;
      Component: Unbounded_String;
      Message: Unbounded_String;
   end record;

   task type Logger is
      entry Start;
      -- Starts the logger thread
      entry Stop;
      -- Stops the logger thread

      entry Accept_Log_Packet;
      -- Accepts a Log Packet for logging

      -- Processes a logger packet if FIFO queue
      entry Log_Immediately(Log_Level: Log_Levels; Component : in Unbounded_String; Message: Unbounded_String);
      -- Logs a message immediately
   end Logger;
   
   

end DNS_Common.Logger;
