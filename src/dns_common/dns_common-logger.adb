with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body DNS_Common.Logger is

   procedure Free_Logger_Msg_Ptr is new Ada.Unchecked_Deallocation
     (Object => Logger_Message_Packet, Name => Logger_Message_Packet_Ptr);

   function Format_Log_Level
     (Use_Color : Boolean;
      Log_Level : Log_Levels)
      return Unbounded_String
   is
      Color_Prefix  : Unbounded_String;
      Log_Level_Str : Unbounded_String;
      Now           : constant Time := Clock;
   begin

      -- Add ANSI color codes if we're using color on Linux
      if Use_Color
      then
         case Log_Level is
            when EMERGERENCY =>
               Color_Prefix := To_Unbounded_String (ANSI_Light_Red);
            when ALERT =>
               Color_Prefix := To_Unbounded_String (ANSI_Light_Red);
            when CRITICAL =>
               Color_Prefix := To_Unbounded_String (ANSI_Light_Red);
            when ERROR =>
               Color_Prefix := To_Unbounded_String (ANSI_Red);
            when WARNING =>
               Color_Prefix := To_Unbounded_String (ANSI_Light_Yellow);
            when NOTICE =>
               Color_Prefix := To_Unbounded_String (ANSI_Yellow);
            when INFO =>
               Color_Prefix := To_Unbounded_String (ANSI_Green);
            when DEBUG =>
               Color_Prefix := To_Unbounded_String (ANSI_Light_Blue);
         end case;

         Log_Level_Str :=
           Color_Prefix & To_Unbounded_String (Log_Level'Image) & ANSI_Reset;
      else
         Log_Level_Str := To_Unbounded_String (Log_Level'Image);
      end if;

      return To_Unbounded_String
          (Image (Date => Now) & " [" & To_String (Log_Level_Str) & "]");
   end Format_Log_Level;

   function Create_String_From_Components
     (Components : Component_Vector.Vector)
      return Unbounded_String
   is
      Components_String : Unbounded_String;
      procedure Component_To_String (c : Component_Vector.Cursor) is
         Scratch_String : Unbounded_String;
      begin
         Scratch_String    := Components (c);
         Components_String := Components_String & "[" & Scratch_String & "]";
      end Component_To_String;
   begin
      -- If there's no component, just leave it blank
      if Components.Length = 0
      then
         return To_Unbounded_String ("");
      end if;

      Components.Iterate (Component_To_String'Access);
      return Components_String;
   end Create_String_From_Components;

   protected body Logger_Queue_Type is
      -- Handles queue of packets for the logger thread
      entry Add_Packet (Queue : Logger_Message_Packet_Ptr) when True is
      begin
         Queued_Packets.Append (Queue);
      end Add_Packet;

      entry Get (Queue : out Logger_Message_Packet_Ptr)
        when Queued_Packets.Length > 0 is
      begin
         Queue := Queued_Packets.First_Element;
         Queued_Packets.Delete_First;
      end Get;

      entry Count (Count : out Integer) when True is
      begin
         Count := Integer (Queued_Packets.Length);
      end Count;

      entry Empty when True is
      begin
         declare
            procedure Dump_Vector_Data
              (c : Logger_Message_Packet_Vector.Cursor)
            is
            begin
               Free_Logger_Msg_Ptr (Queued_Packets (c));
            end Dump_Vector_Data;
         begin
            Queued_Packets.Iterate (Dump_Vector_Data'access);
         end;
      end Empty;
   end Logger_Queue_Type;

   protected body Logger_Message_Packet is
      entry Push_Component (Component : String) when True is
      begin
         Current_Component.Append (To_Unbounded_String (Component));
      end Push_Component;

      entry Pop_Component when True is
      begin
         Current_Component.Delete_Last;
      end Pop_Component;

      entry Log_Message
        (Level :    Log_Levels;
         Msg   : in String) when True is
         Msg_Record : Log_Message_Record;
      begin
         Msg_Record.Log_Level := Level;
         Msg_Record.Component := Current_Component.Copy;
         Msg_Record.Message   := To_Unbounded_String (Msg);

         Logged_Msgs.Append (Msg_Record);
      end Log_Message;

      entry Get (Msg : out Log_Message_Record) when Logged_Msgs.Length > 0 is
      begin
         Msg := Logged_Msgs.First_Element;
         Logged_Msgs.Delete_First;
      end Get;

      entry Get_All_And_Empty (Queue : out Log_Message_Vector.Vector) when True
        is
      begin
         Queue := Logged_Msgs.Copy;
         Logged_Msgs.Clear;
      end Get_All_And_Empty;

      entry Count (Count : out Integer) when True is
      begin
         Count := Integer (Logged_Msgs.Length);
      end Count;

      entry Empty when True is
      begin
         Logged_Msgs.Clear;
      end Empty;

   end Logger_Message_Packet;

   task body Logger is
      -- Running task for the logger processing the global message queue
      Logger_Cfg        : Logger_Configuration;
      Keep_Running      : Boolean := False;
      Current_Queue     : Logger_Message_Packet_Ptr;
      Msg_Packets       : Log_Message_Vector.Vector;
      Msg_String        : Unbounded_String;
      Queues_To_Process : Integer;
      procedure Process_Queue is

         procedure Print_Messages (c : Log_Message_Vector.Cursor) is
            Current_Msg : constant Log_Message_Record :=
              Log_Message_Vector.Element (c);
         begin
            -- This is so ugly :(
            if Log_Levels'Enum_Rep (Logger_Cfg.Log_Level) >=
              Log_Levels'Enum_Rep (Current_Msg.Log_Level)
            then
               Msg_String :=
                 Format_Log_Level
                   (Logger_Cfg.Use_Color, Current_Msg.Log_Level);
               Msg_String :=
                 Msg_String &
                 Create_String_From_Components (Current_Msg.Component);
               Msg_String := Msg_String & " " & Current_Msg.Message;
               Put_Line (To_String (Msg_String));
            end if;
         end Print_Messages;

      begin
         Logger_Queue.Count (Queues_To_Process);
         while Queues_To_Process > 0
         loop
            Logger_Queue.Get (Current_Queue);

            -- Get a local copy and then empty it; we don't care past that
            -- point
            if Current_Queue /= null
            then
               Current_Queue.Get_All_And_Empty (Msg_Packets);
               Msg_Packets.Iterate (Print_Messages'Access);
               Free_Logger_Msg_Ptr (Current_Queue);
            end if;

            Logger_Queue.Count (Queues_To_Process);
         end loop;

      exception
         -- Not sure if there's a better way to do this, but avoids a race
         -- condition
         when Constraint_Error =>
            begin
               null;
            end;
      end Process_Queue;

   begin
      -- Set some sane defaults for Logger config if not initialized
      Logger_Cfg.Log_Level := NOTICE;
      Logger_Cfg.Use_Color := False;

      loop
         -- Processing loop
         while Keep_Running
         loop
            select
               accept Start do
                  null;
               end Start;
            or
               accept Stop do
                  -- Flush the pending queue
                  Keep_Running := False;
                  Process_Queue;
               end Stop;
            else
               Process_Queue;
               delay 0.1;
            end select;
         end loop;

         -- Idling loop ready for shutdown
         while Keep_Running = False
         loop
            select
               accept Initialize (Cfg : Logger_Configuration) do
                  Logger_Cfg := Cfg;
               end Initialize;
            or
               accept Start do
                  Keep_Running := True;
               end Start;
            or
               accept Stop do
                  null;
               end Stop;
            or
               terminate;
            end select;
         end loop;
      end loop;
   end Logger;
end DNS_Common.Logger;
