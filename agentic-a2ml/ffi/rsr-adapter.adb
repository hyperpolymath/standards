-- SPDX-License-Identifier: AGPL-3.0-or-later
-- (MPL-2.0 is automatic legal fallback until PMPL is formally recognised)
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- RSR_Adapter body - Repository customization implementation
--
-- Implements safe file operations for customizing repositories.
-- All file operations validate paths to prevent traversal attacks.

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with Ada.IO_Exceptions;

package body RSR_Adapter is

   -- Character set for path validation
   Safe_Path_Chars : constant Character_Set :=
     To_Set ("abcdefghijklmnopqrstuvwxyz" &
             "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
             "0123456789" &
             "-_./");

   ---------------------------------------------------------------------------
   -- Is_Safe_Path
   ---------------------------------------------------------------------------
   function Is_Safe_Path (Path : String) return Boolean is
   begin
      -- Reject empty paths
      if Path'Length = 0 then
         return False;
      end if;

      -- Reject paths with directory traversal
      if Index (Path, "..") > 0 then
         return False;
      end if;

      -- Reject absolute paths (must be relative)
      if Path (Path'First) = '/' then
         return False;
      end if;

      -- Verify all characters are safe
      for I in Path'Range loop
         if not Is_In (Path (I), Safe_Path_Chars) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Safe_Path;

   ---------------------------------------------------------------------------
   -- Get_Input
   ---------------------------------------------------------------------------
   function Get_Input (Prompt : String) return Unbounded_String is
      Line : String (1 .. Max_Input_Length);
      Last : Natural;
   begin
      Put (Prompt & " ");
      Get_Line (Line, Last);

      if Last > 0 then
         return To_Unbounded_String (Line (1 .. Last));
      else
         return Null_Unbounded_String;
      end if;
   exception
      when Ada.IO_Exceptions.End_Error =>
         return Null_Unbounded_String;
   end Get_Input;

   ---------------------------------------------------------------------------
   -- Get_AI_Target_Choice
   ---------------------------------------------------------------------------
   function Get_AI_Target_Choice return AI_Target is
      Choice : Unbounded_String;
   begin
      Put_Line ("Select AI Target:");
      Put_Line ("  1. Claude");
      Put_Line ("  2. Copilot");
      Put_Line ("  3. None");
      Choice := Get_Input ("Enter choice (1-3):");

      declare
         S : constant String := To_String (Choice);
      begin
         if S = "1" or else S = "claude" or else S = "Claude" then
            return Claude;
         elsif S = "2" or else S = "copilot" or else S = "Copilot" then
            return Copilot;
         elsif S = "3" or else S = "none" or else S = "None" then
            return None;
         else
            Put_Line ("Invalid selection, defaulting to None");
            return None;
         end if;
      end;
   end Get_AI_Target_Choice;

   ---------------------------------------------------------------------------
   -- Update_README
   ---------------------------------------------------------------------------
   function Update_README
     (Project_Name : Unbounded_String;
      Author       : Unbounded_String) return Boolean
   is
      Readme_Path : constant String := "README.adoc";
      File        : File_Type;
      Template    : constant String :=
        "// SPDX-License-Identifier: AGPL-3.0-or-later" & ASCII.LF &
        "// SPDX-FileCopyrightText: 2026 " & To_String (Author) & ASCII.LF &
        "= " & To_String (Project_Name) & ASCII.LF &
        ASCII.LF &
        "== Overview" & ASCII.LF &
        ASCII.LF &
        "This project uses the agentic-scm template." & ASCII.LF;
   begin
      -- Check if README exists
      if not Exists (Readme_Path) then
         Put_Line ("Warning: README.adoc not found, creating new file");
      end if;

      -- Write the updated README
      Create (File, Out_File, Readme_Path);
      Put (File, Template);
      Close (File);

      Put_Line ("Updated " & Readme_Path);
      return True;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line ("Error: Cannot create " & Readme_Path);
         return False;
      when Ada.IO_Exceptions.Use_Error =>
         Put_Line ("Error: Cannot write to " & Readme_Path);
         return False;
   end Update_README;

   ---------------------------------------------------------------------------
   -- Blank_File
   ---------------------------------------------------------------------------
   function Blank_File (Path : String) return Boolean is
      File : File_Type;
   begin
      -- Validate path safety
      if not Is_Safe_Path (Path) then
         Put_Line ("Error: Unsafe path rejected: " & Path);
         return False;
      end if;

      -- Check if file exists
      if not Exists (Path) then
         Put_Line ("Warning: File not found: " & Path);
         return False;
      end if;

      -- Overwrite with empty content
      Create (File, Out_File, Path);
      Close (File);

      Put_Line ("Blanked: " & Path);
      return True;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line ("Error: Cannot access " & Path);
         return False;
      when Ada.IO_Exceptions.Use_Error =>
         Put_Line ("Error: Cannot write to " & Path);
         return False;
   end Blank_File;

   ---------------------------------------------------------------------------
   -- Delete_File
   ---------------------------------------------------------------------------
   function Delete_File (Path : String) return Boolean is
   begin
      -- Validate path safety
      if not Is_Safe_Path (Path) then
         Put_Line ("Error: Unsafe path rejected: " & Path);
         return False;
      end if;

      -- Check if file exists
      if not Exists (Path) then
         Put_Line ("Warning: File not found: " & Path);
         return False;
      end if;

      -- Delete the file
      Ada.Directories.Delete_File (Path);
      Put_Line ("Deleted: " & Path);
      return True;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line ("Error: Cannot access " & Path);
         return False;
      when Ada.IO_Exceptions.Use_Error =>
         Put_Line ("Error: Cannot delete " & Path);
         return False;
   end Delete_File;

   ---------------------------------------------------------------------------
   -- Customize_Repo
   ---------------------------------------------------------------------------
   procedure Customize_Repo is
      Config         : Repo_Config;
      AI_Context_Path : constant String := ".rhodium/ai-context.json";
      Success        : Boolean;
   begin
      Put_Line ("=== Repository Customization ===");
      Put_Line ("");

      -- Gather configuration
      Config.Project_Name := Get_Input ("Project Name?");
      Config.Author := Get_Input ("Your Name?");
      Config.AI_Tool := Get_AI_Target_Choice;

      Put_Line ("");
      Put_Line ("Configuration:");
      Put_Line ("  Project: " & To_String (Config.Project_Name));
      Put_Line ("  Author:  " & To_String (Config.Author));
      Put_Line ("  AI Tool: " & AI_Target'Image (Config.AI_Tool));
      Put_Line ("");

      -- Update README with project info
      Success := Update_README (Config.Project_Name, Config.Author);
      if not Success then
         Put_Line ("Warning: README update failed");
      end if;

      -- Handle AI context file based on selection
      case Config.AI_Tool is
         when Claude =>
            Put_Line ("Keeping AI context for Claude");

         when Copilot =>
            Put_Line ("Keeping AI context for Copilot");
            -- Could customize for Copilot-specific format here

         when None =>
            -- Blank the AI context file if no AI tool selected
            Success := Blank_File (AI_Context_Path);
            if not Success then
               Put_Line ("Note: AI context file not found or not writable");
            end if;
      end case;

      Put_Line ("");
      Put_Line ("Repository customization complete.");

   end Customize_Repo;

end RSR_Adapter;
