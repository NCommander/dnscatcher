-- Copyright 2019 Michael Casadevall <michael@casadevall.pro>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

with Ada.Containers.Vectors; use Ada.Containers;

-- @summary
-- Handles logging functionality for DNSCatcher. This represents a common
-- class of functionality, with individual subclasses defining logging to
-- null, stdout, file, and syslog
--
-- @description
-- DNSCatcher's Logger is designed to work on the basis of message compliation
-- and dispatching. Throughout a given operation, data may be passed through
-- multiple processors, modules, and other components, with other data being
-- handled simulatiously. As such Log messages are contained in a specific
-- queue which is then gathered up, and dispatched as one giant chunk of logs.
--
-- Given that many different component can be used for logging such as syslog
-- on POSIX systems, or perhaps Logstash, this is designed to be abstracted
-- away quickly and easily
--
package DNSCatcher.Utils.Logger is
   -- Handles logging operations in a sane and consistent way within the
   -- Catcher

   -- Represents the type and severity of a log message. This type is directly
   -- based on syslog message symatics.
   type Log_Levels is
     (EMERGERENCY, -- Emergency message; reserved for emergency bail outs
      ALERT, -- Alert condition, system admin intervention required
      CRITICAL, -- Critical message, something has gone wrong in a component
      ERROR, -- Error message, may be fatal or non-fatal
      WARNING, -- Warning
      NOTICE, -- Information notice
      INFO, -- Basic information
      DEBUG -- Debug information
      );

   --!pp off
   for Log_Levels use (EMERGERENCY => 0,
                       ALERT => 1,
                       CRITICAL => 2,
                       ERROR => 3,
                       WARNING => 4,
                       NOTICE => 5,
                       INFO => 6,
                       DEBUG => 7);
   --!pp on

   -- Logger configuration object
   --
   -- @value Log_Level
   -- Sets the filter level for messages being sent
   --
   -- @value Use_Color
   -- If supported, use color within the log message
   type Logger_Configuration is record
      Log_Level : Log_Levels;
      Use_Color : Boolean;
   end record;

   -- Component_Vector is a list of Unbounded_Strings that make up the
   -- component part of a given log message, creating a hierarchy of log
   -- messages based on component and calling path
   package Component_Vector is new Vectors (Natural, Unbounded_String);

   -- Individual log message, which contains the component
   --
   -- @value Log_Level
   -- The log level of a given message
   --
   -- @value Component
   -- The vector containing the component levels
   --
   -- @value Message
   -- The actual log message as an unbounded string
   type Log_Message_Record is record
      Log_Level : Log_Levels;
      Component : Component_Vector.Vector;
      Message   : Unbounded_String;
   end record;
   type Log_Message_Record_Ptr is access Log_Message_Record;

   -- Logger Message Queue is used by a task to ensure mesages are delivered in
   -- order. One queue exists per task.
   package Log_Message_Vector is new Vectors (Natural, Log_Message_Record);

   -- Logger Message Component
   --
   -- This is a protected type that handles all log messages within a given
   -- component, and is pushed into the global packet vector
   protected type Logger_Message_Packet is
      -- Pushes a component name onto the component stack
      --
      -- @value Component
      -- Name of the component
      entry Push_Component (Component : String);

      -- Pops the latest component on the stack
      entry Pop_Component;

      -- Logs a message
      --
      -- @value Level
      -- Log level of the message
      --
      -- @value Msg
      -- String of the msg to be added
      entry Log_Message
        (Level : Log_Levels;
         Msg   : String);

         -- Pops the top message of the internal message stack
         --
         -- @value Msg
         -- Output for Log_Message_Record
      entry Get (Msg : out Log_Message_Record);

      -- Gets all messages from this component queue, and clears it
      --
      -- @value Queue
      -- Returns a Log_Message_Vector with all the component objects
      entry Get_All_And_Empty (Queue : out Log_Message_Vector.Vector);

      -- Gets count of all messages in this queue
      --
      -- @value Count
      -- Integer to return the count to
      entry Count (Count : out Integer);

      -- Empties queue of all messages
      entry Empty;
   private
      Current_Component : Component_Vector.Vector;
      Logged_Msgs       : Log_Message_Vector.Vector;
   end Logger_Message_Packet;
   type Logger_Message_Packet_Ptr is access Logger_Message_Packet;

   -- Vector containing sets of log messages
   package Logger_Message_Packet_Vector is new Vectors (Natural,
      Logger_Message_Packet_Ptr);

   -- Implements the global logger queue message type; this is used as a global
   -- object for taking logger packets and dispatching them to whatever end
   -- point is configured by the logger
   --
   protected type Logger_Queue_Type is
      -- Adds a logger message packet to the queue
      --
      -- @value Queue
      -- Pointer to the logger message to add
      entry Add_Packet (Queue : Logger_Message_Packet_Ptr);

      -- Gets the top of the queue
      --
      -- @value Queue
      -- Variable to write the pointer to
      entry Get (Queue : out Logger_Message_Packet_Ptr);

      -- Gets a count of the number of logger packets in the queue
      --
      -- @value Count
      -- Count of all message
      --
      entry Count (Count : out Integer);

      -- Empties the logger queue
      entry Empty;
   private
      Queued_Packets : Logger_Message_Packet_Vector.Vector;
   end Logger_Queue_Type;

   -- Global for logging queues
   Logger_Queue : Logger_Queue_Type;

   -- Task for handling logger functionality
   task type Logger is
      -- Initializes the logger task
      --
      -- @value Cfg
      -- Configuration object
      entry Initialize (Cfg : Logger_Configuration);

      -- Starts the logger thread
      entry Start;

      -- Stops the logger thread
      entry Stop;

   end Logger;

private
   -- ANSI Color Codes, and Reset message for STDOUT printing on Linux
   ANSI_Default       : constant String := ASCII.ESC & "[39m";
   ANSI_Black         : constant String := ASCII.ESC & "[30m";
   ANSI_Red           : constant String := ASCII.ESC & "[31m";
   ANSI_Green         : constant String := ASCII.ESC & "[32m";
   ANSI_Yellow        : constant String := ASCII.ESC & "[33m";
   ANSI_Blue          : constant String := ASCII.ESC & "[34m";
   ANSI_Magenta       : constant String := ASCII.ESC & "[35m";
   ANSI_Cyan          : constant String := ASCII.ESC & "[36m";
   ANSI_Light_Gray    : constant String := ASCII.ESC & "[37m";
   ANSI_Dark_Gray     : constant String := ASCII.ESC & "[90m";
   ANSI_Light_Red     : constant String := ASCII.ESC & "[91m";
   ANSI_Light_Green   : constant String := ASCII.ESC & "[92m";
   ANSI_Light_Yellow  : constant String := ASCII.ESC & "[93m";
   ANSI_Light_Blue    : constant String := ASCII.ESC & "[94m";
   ANSI_Light_Magenta : constant String := ASCII.ESC & "[95m";
   ANSI_Light_Cyan    : constant String := ASCII.ESC & "[96m";

   ANSI_Reset : constant String := ASCII.ESC & "[0m";

end DNSCatcher.Utils.Logger;
