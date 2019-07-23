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

with Ada.Characters.Latin_1;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body DNSCatcher.Config is
   procedure Initialize_Config_Parse is
   begin
      -- Load String to Type Mapping
      GCP_Map.Insert ("LOCAL_LISTEN_PORT", Parse_Local_Listen_Port'Access);
      GCP_Map.Insert ("UPSTREAM_DNS_SERVER", Parse_Upstream_DNS_Server'Access);
      GCP_Map.Insert
        ("UPSTREAM_DNS_SERVER_PORT", Parse_Upstream_DNS_Server_Port'Access);
   end Initialize_Config_Parse;

   procedure Parse_Local_Listen_Port
     (Config    : Configuration_Ptr;
      Value_Str : String)
   is
   begin
      Config.Local_Listen_Port := Port_Type'Value (Value_Str);
   exception
      when Constraint_Error =>
         raise Malformed_Line with "Local_Listen_Port not a valid port number";
   end Parse_Local_Listen_Port;

   procedure Parse_Upstream_DNS_Server
     (Config    : Configuration_Ptr;
      Value_Str : String)
   is
   begin
      Config.Upstream_DNS_Server := To_Unbounded_String (Value_Str);
   exception
      when Constraint_Error =>
         raise Malformed_Line with "Invalid upstrema DNS Server";
   end Parse_Upstream_DNS_Server;

   procedure Parse_Upstream_DNS_Server_Port
     (Config    : Configuration_Ptr;
      Value_Str : String)
   is
   begin
      Config.Upstream_DNS_Server_Port := Port_Type'Value (Value_Str);
   exception
      when Constraint_Error =>
         raise Malformed_Line
           with "Upstream_DNS_Server_Port not a valid port number";
   end Parse_Upstream_DNS_Server_Port;

   function Parse_Config_File
     (Config_File_Path : String)
     return Configuration_Ptr
   is
      Parsed_Config     : Configuration_Ptr;
      Config_File       : Ada.Text_IO.File_Type;
      Line_Count        : Integer := 1;
      Exception_Message : Unbounded_String;
      use GCP_Management;
   begin
      Parsed_Config := new Configuration;

      Initialize_Config_Parse;

      -- Set sane defaults
      Parsed_Config.Local_Listen_Port        := 53;
      Parsed_Config.Upstream_DNS_Server_Port := 53;

      -- Try to open the configuration file
      Open
        (File => Config_File,
         Mode => Ada.Text_IO.In_File,
         Name => Config_File_Path);

      while not End_Of_File (Config_File)
      loop
         declare
            Current_Line  : constant String := Get_Line (Config_File);
            Key_End_Loc   : Integer         := 0;
            Equals_Loc    : Integer         := 0;
            Value_Loc     : Integer         := 0;
            Is_Whitespace : Boolean         := True;
         begin
            -- Skip lines starting with a comment or blank
            if Current_Line = ""
            then
               goto Config_Parse_Continue;
            end if;

            -- Has to be done seperately or it blows an index check
            if Current_Line (1) = '#'
            then
               goto Config_Parse_Continue;
            end if;

            -- Skip line if its all whitespace
            for I in Current_Line'range
            loop
               if Current_Line (I) /= ' ' and
                 Current_Line (I) /= Ada.Characters.Latin_1.HT
               then
                  Is_Whitespace := False;
               end if;

               if Current_Line (I) = '='
               then
                  Equals_Loc := I;
               end if;

               -- Determine length of the key
               --
               -- This is a little non-obvious at first glance. We subtract 2
               -- here to remove the character we want, and the previous char
               -- because a 17 char string will be 1..18 in the array.

               if (Is_Whitespace or Current_Line (I) = '=') and Key_End_Loc = 0
               then
                  Key_End_Loc := I - 2;
               end if;

               exit when Is_Whitespace and Equals_Loc /= 0;
               -- We also want to confirm there's a = in there somewhere
            end loop;

            -- It's all whitespace, skip it
            if Is_Whitespace
            then
               goto Config_Parse_Continue;
            end if;

            if Equals_Loc = 0
            then
               Exception_Message :=
                 To_Unbounded_String ("Malformed line (no = found) at");
               Append (Exception_Message, Line_Count'Image);
               Append (Exception_Message, ": ");
               Append (Exception_Message, Current_Line);

               raise Malformed_Line with To_String (Exception_Message);
            end if;

            -- Read in the essential values
            for C in GCP_Map.Iterate
            loop
               -- Slightly annoying, but need to handle not reading past the
               -- end of Current_Line. We also need to check that the next char
               -- is a space or = so we don't match substrings by accident.
               if Key_End_Loc = Key (C)'Length
               then
                  if Key (C) = To_Upper (Current_Line
                         (1 .. Key (C)'Length))
                  then
                     -- Determine the starting character of the value
                     for I in Current_Line
                       (Equals_Loc + 1 .. Current_Line'Length)'range
                     loop
                        if Current_Line (I) /= ' ' and
                          Current_Line (I) /= Ada.Characters.Latin_1.HT
                        then
                           Value_Loc := I;
                           exit;
                        end if;
                     end loop;

                     -- If Value_Loc is zero, pass an empty string in
                     if Value_Loc = 0
                     then
                        Element (C).all (Parsed_Config, "");
                     else
                        Element (C).all (Parsed_Config, Current_Line
                             (Value_Loc .. Current_Line'Length));
                     end if;
                  end if;
               end if;
            end loop;

            <<Config_Parse_Continue>>
            Line_Count := Line_Count + 1;
         end;
      end loop;

      -- Clean up
      Close (Config_File);

      -- Confirm mandatory values are set
      if Parsed_Config.Upstream_DNS_Server = ""
      then
         raise Missing_Mandatory_Config_Option
           with "Upstream_DNS_Server must be set";
      end if;

      return Parsed_Config;
   end Parse_Config_File;

end DNSCatcher.Config;
