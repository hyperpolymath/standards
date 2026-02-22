package A2ML_TUI.Core with SPARK_Mode is
   type Mode_Type is (Lax, Checked);

   type State is record
      Mode   : Mode_Type := Checked;
      Concat : Boolean := False;
      Outp   : String (1 .. 1) := " "; -- empty sentinel
   end record;

   function Toggle_Mode (S : State) return State;
   function Toggle_Concat (S : State) return State;
   function Set_Out (S : State; Path : String) return State;

   function Build_Command
     (Command : String;
      Input   : String;
      Mode    : Mode_Type;
      Outp    : String;
      Concat  : Boolean) return String;
end A2ML_TUI.Core;
