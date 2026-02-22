with Ada.Text_IO;
with Ada.Strings.Fixed;
with GNAT.OS_Lib;
with A2ML_TUI.Core;

procedure Main is
   pragma SPARK_Mode (Off);

   use Ada.Text_IO;
   use Ada.Strings.Fixed;
   use A2ML_TUI.Core;

   function Read_Line_Trim return String is
      Buffer : String (1 .. 1024);
      Last   : Natural;
   begin
      Get_Line (Buffer, Last);
      return Trim (Buffer (1 .. Last), Ada.Strings.Both);
   end Read_Line_Trim;

   procedure Run_Command (Cmd : String) is
      Args   : GNAT.OS_Lib.Argument_List_Access;
      Result : Integer;
   begin
      Args := GNAT.OS_Lib.Argument_String_To_List (Cmd);
      Result := GNAT.OS_Lib.Spawn (Args (Args'First), Args);
      GNAT.OS_Lib.Free (Args);
      Put_Line ("Exit code: " & Integer'Image (Result));
   end Run_Command;

   Choice : String := "";
   Input  : String := "";
   Outp   : String := "";
   St     : State;
begin
   Put_Line ("A2ML Ada TUI (prototype)");
   Put_Line ("------------------------");
   loop
      Put_Line ("");
      Put_Line ("1) Render HTML");
      Put_Line ("2) Validate (checked)");
      Put_Line ("3) Dump AST (JSON)");
      Put_Line ("4) Toggle mode (lax/checked) [current: " & (if St.Mode = Checked then "checked" else "lax") & "]");
      Put_Line ("5) Toggle concat [current: " & (if St.Concat then "on" else "off") & "]");
      Put_Line ("6) Set output path [current: " & (if St.Outp = " " then "stdout" else St.Outp) & "]");
      Put_Line ("7) Quit");
      Put ("> ");
      Choice := Read_Line_Trim;

      if Choice = "7" then
         exit;
      elsif Choice = "4" then
         St := Toggle_Mode (St);
      elsif Choice = "5" then
         St := Toggle_Concat (St);
      elsif Choice = "6" then
         Put ("Output path (empty for stdout): ");
         Outp := Read_Line_Trim;
         St := Set_Out (St, Outp);
      elsif Choice = "1" or else Choice = "2" or else Choice = "3" then
         Put ("Input file (or '-' for stdin): ");
         Input := Read_Line_Trim;
         if Input = "" then
            Put_Line ("No input provided.");
         else
            declare
               Cmd : String :=
                 (if Choice = "1" then Build_Command ("render", Input, St.Mode, St.Outp, St.Concat)
                  elsif Choice = "2" then Build_Command ("validate", Input, St.Mode, St.Outp, St.Concat)
                  else Build_Command ("ast", Input, St.Mode, St.Outp, St.Concat));
            begin
               Put_Line ("Running: " & Cmd);
               Run_Command (Cmd);
            end;
         end if;
      else
         Put_Line ("Unknown choice.");
      end if;
   end loop;
end Main;
