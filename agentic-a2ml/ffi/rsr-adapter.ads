-- SPDX-License-Identifier: MPL-2.0
-- (MPL-2.0 is automatic legal fallback until PMPL is formally recognised)
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- RSR_Adapter - Repository customization for agentic-scm
--
-- This package provides safe file operations for customizing
-- repositories based on user input. Uses SPARK subset for
-- memory safety guarantees.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package RSR_Adapter is

   -- AI tool target selection
   type AI_Target is (Claude, Copilot, None);

   -- User choice for file handling
   type File_Choice is (Keep, Blank, Delete);

   -- Maximum lengths for bounded strings
   Max_Input_Length : constant := 256;

   -- Repository configuration record
   type Repo_Config is record
      Project_Name : Unbounded_String;
      Author       : Unbounded_String;
      AI_Tool      : AI_Target;
   end record;

   -- Get input from user with prompt
   -- Returns empty string on EOF or error
   function Get_Input (Prompt : String) return Unbounded_String;

   -- Display menu and get AI target choice
   -- Returns None if invalid selection
   function Get_AI_Target_Choice return AI_Target;

   -- Update README with project information
   -- Returns True on success, False on failure
   function Update_README
     (Project_Name : Unbounded_String;
      Author       : Unbounded_String) return Boolean;

   -- Blank out a file (replace with empty content)
   -- Returns True on success, False if file not found
   function Blank_File (Path : String) return Boolean;

   -- Delete a file
   -- Returns True on success, False if file not found
   function Delete_File (Path : String) return Boolean;

   -- Main customization procedure
   -- Orchestrates the repository setup process
   procedure Customize_Repo;

   -- Validate that a path is safe (no traversal attacks)
   function Is_Safe_Path (Path : String) return Boolean;

end RSR_Adapter;
