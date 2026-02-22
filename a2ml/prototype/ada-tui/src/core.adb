with Ada.Strings.Fixed;

package body A2ML_TUI.Core with SPARK_Mode is
   use Ada.Strings.Fixed;

   function Toggle_Mode (S : State) return State is
      R : State := S;
   begin
      if S.Mode = Checked then
         R.Mode := Lax;
      else
         R.Mode := Checked;
      end if;
      return R;
   end Toggle_Mode;

   function Toggle_Concat (S : State) return State is
      R : State := S;
   begin
      R.Concat := not S.Concat;
      return R;
   end Toggle_Concat;

   function Set_Out (S : State; Path : String) return State is
      R : State := S;
   begin
      if Path'Length = 0 then
         R.Outp := " ";
      else
         R.Outp := Path;
      end if;
      return R;
   end Set_Out;

   function Mode_Str (M : Mode_Type) return String is
   begin
      return (if M = Checked then "checked" else "lax");
   end Mode_Str;

   function Build_Command
     (Command : String;
      Input   : String;
      Mode    : Mode_Type;
      Outp    : String;
      Concat  : Boolean) return String is
      Cmd : String := "just cli " & Command & " " & Input;
   begin
      Cmd := Cmd & " --mode " & Mode_Str (Mode);
      if Outp /= " " then
         Cmd := Cmd & " --out " & Outp;
      end if;
      if Concat then
         Cmd := Cmd & " --concat";
      end if;
      return Cmd;
   end Build_Command;
end A2ML_TUI.Core;
