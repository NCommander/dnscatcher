with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;

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
      Use_Color: Boolean;
   end record;

   -- Contains a message to log
   package Component_Vector is new Vectors (Natural, Unbounded_String);
   
   type Log_Message_Record is record
      Log_Level: Log_Levels;
      Component: Component_Vector.Vector;
      Message: Unbounded_String;
   end record;
   type Log_Message_Record_Ptr is access Log_Message_Record;
   
   -- Logger Message Queue is used by a task to ensure mesages are delivered
   -- in order. One queue exists per task.
   package Log_Message_Vector is new Vectors (Natural, Log_Message_Record);
   
   protected type Logger_Message_Packet is
      entry Push_Component(Component: String);
      entry Pop_Component;
      entry Log_Message (Level: Log_Levels; Msg : in String);
      entry Get (Msg : out Log_Message_Record);
      entry Count (Count : out Integer);
      entry Empty;
   private
      Current_Component: Component_Vector.Vector;
      Logged_Msgs : Log_Message_Vector.Vector;
   end Logger_Message_Packet;
   type Logger_Message_Packet_Ptr is access Logger_Message_Packet;

   package Logger_Message_Packet_Vector is new Vectors(Natural, Logger_Message_Packet_Ptr);
   protected type Logger_Queue_Type is
      entry Add_Packet(Queue: Logger_Message_Packet_Ptr);
      entry Get(Queue: out Logger_Message_Packet_Ptr);
      entry Count(Count: out Integer);
      entry Empty;
   private
      Queued_Packets : Logger_Message_Packet_Vector.Vector;
   end Logger_Queue_Type;
   
   -- Global for logging queues
   Logger_Queue : Logger_Queue_Type;

   task type Logger is
      entry Initialize(Cfg: Logger_Configuration);
      entry Start;
      -- Starts the logger thread
      entry Stop;
      -- Stops the logger thread

   end Logger;
   
   

end DNS_Common.Logger;
