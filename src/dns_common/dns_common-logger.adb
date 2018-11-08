with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body DNS_Common.Logger is

   procedure Free_Logger_Msg_Ptr is new Ada.Unchecked_Deallocation
     (Object => Logger_Message_Packet, Name => Logger_Message_Packet_Ptr);

   function Format_Log_Level (Use_Color : Boolean; Log_Level : Log_Levels) return Unbounded_String
   is
   begin
      -- We'll do color later
      return To_Unbounded_String ("[" & Log_Level'Image & "]");
   end Format_Log_Level;

   function Create_String_From_Components
     (Components : Component_Vector.Vector) return Unbounded_String
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

      entry Get (Queue : out Logger_Message_Packet_Ptr) when Queued_Packets.Length > 0 is
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
            procedure Dump_Vector_Data (c : Logger_Message_Packet_Vector.Cursor) is
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
         Current_Component.Append (To_Unbounded_String(Component));
      end Push_Component;

      entry Pop_Component when True is
      begin
         Current_Component.Delete_Last;
      end Pop_Component;

      entry Log_Message (Level : Log_Levels; Msg : in String) when True is
         Msg_Record : Log_Message_Record;
      begin
         Msg_Record.Log_Level := Level;
         Msg_Record.Component := Current_Component.Copy;
         Msg_Record.Message   := To_Unbounded_String(Msg);

         Logged_Msgs.Append (Msg_Record);
      end Log_Message;

      entry Get (Msg : out Log_Message_Record) when Logged_Msgs.Length > 0 is
      begin
         Msg := Logged_Msgs.First_Element;
         Logged_Msgs.Delete_First;
      end Get;

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
      Logger_Cfg    : Logger_Configuration;
      Keep_Running  : Boolean := False;
      Current_Queue : Logger_Message_Packet_Ptr;
      Queue_Count   : Integer := 0;
      Msg_String    : Unbounded_String;
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
                  Logger_Queue.Empty;
                  Keep_Running := False;
               end Stop;
            else
               Logger_Queue.Count (Queue_Count);
               if Queue_Count > 0
               then
                  Logger_Queue.Get (Current_Queue);

                  -- Process the queue and deliver all msgs
                  declare
                     Queue_Msg_Count : Integer;
                     Current_Msg     : Log_Message_Record;
                  begin
                     Current_Queue.Count (Queue_Msg_Count);

                     while Queue_Msg_Count /= 0
                     loop
                        Current_Queue.Get (Current_Msg);

                        -- This is so ugly :(
                        if Log_Levels'Enum_Rep(Logger_Cfg.Log_Level) >= Log_Levels'Enum_Rep (Current_Msg.Log_Level)
                        then
                           Msg_String :=
                             Format_Log_Level (Logger_Cfg.Use_Color, Current_Msg.Log_Level);
                           Msg_String :=
                             Msg_String & Create_String_From_Components (Current_Msg.Component);
                           Msg_String := Msg_String & " " & Current_Msg.Message;
                           Put_Line (To_String (Msg_String));
                        end if;
                        Current_Queue.Count (Queue_Msg_Count);
                     end loop;
                     delay 0.1;
                  end;

                  Free_Logger_Msg_Ptr (Current_Queue);
               end if;
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
               terminate;
            end select;
         end loop;
      end loop;
   end Logger;
end DNS_Common.Logger;
