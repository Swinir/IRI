--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interpretor;

procedure Main is
   Interpreteur : Interpretor.T_Interpretor;
begin
   Put_Line("Hello, World!");
   Put_Line("Please input the full path to the file you want to evaluate");
   --Get_Line(Path);
   Interpretor.Init("tests/test_interpretor.txt", Interpreteur);
   Interpretor.Interpret_Single_Instruction(Interpreteur);
   Interpretor.Display_Single_Info(Interpreteur);
      Interpretor.Interpret_Single_Instruction(Interpreteur);
   Interpretor.Display_Single_Info(Interpreteur);
      Interpretor.Interpret_Single_Instruction(Interpreteur);
   Interpretor.Display_Single_Info(Interpreteur);
      Interpretor.Interpret_Single_Instruction(Interpreteur);
   Interpretor.Display_Single_Info(Interpreteur);


      Interpretor.Interpret_Single_Instruction(Interpreteur);
   Interpretor.Display_Single_Info(Interpreteur);
      Interpretor.Interpret_Single_Instruction(Interpreteur);
   Interpretor.Display_Single_Info(Interpreteur);
      Interpretor.Interpret_Single_Instruction(Interpreteur);
   Interpretor.Display_Single_Info(Interpreteur);
      Interpretor.Interpret_Single_Instruction(Interpreteur);
   Interpretor.Display_Single_Info(Interpreteur);
   
end Main;