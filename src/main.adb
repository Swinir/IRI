with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interpretor;
with Evaluator;
with Memory;

procedure Main is
   type Mode is (Normal, Debugger);
   Interpreteur : Interpretor.T_Interpretor;
   Current_Mode : Mode := Normal;
   User_Input : Unbounded_String;
begin
   Put_Line("Hello, World!");
   Put_Line("Please input the full path to the file you want to evaluate");
   --Get_Line(Path); -- TODO : ENABLE THIS AND USE PATH
   Interpretor.Init("tests/test_2.txt", Interpreteur);

   Put_Line("Please enter the running mode (normal or debugger):");
   User_Input := To_Unbounded_String(Get_Line);
   if To_Unbounded_String("normal") = User_Input then Current_Mode := Normal;
   elsif To_Unbounded_String("debugger") = User_Input then Current_Mode := Debugger;
   else Put_Line("Invalid mode. Defaulting to Normal mode."); Current_Mode := Normal;
   end if;

   if Current_Mode = Normal then
      Interpretor.Interpret_All(Interpreteur);
   elsif Current_Mode = Debugger then
      Put_Line("");
      Put_Line("");
      Put_Line("--------------------------------- Memory Content ---------------------------------");
      Put("-- Memory index value :" & Integer'Image(1) & "  |  ");
      Memory.Put(Memory.Get_Data(Interpretor.Get_Memory(Interpreteur), 1));
      for I in 2..Memory.Length(Interpretor.Get_Memory(Interpreteur)) loop
         Put_Line("");
         Put_Line("------------------------------------------------");
         Put("-- Memory index value :" & Integer'Image(I) & "  |  ");
         Memory.Put(Memory.Get_Data(Interpretor.Get_Memory(Interpreteur), I));
      end loop;
      Put_Line("");
      Put_Line("----------------------------------------------------------------------------------");
      Put_Line("");
      Put_Line("");
      Put_Line("");
      Interpretor.Display_Single_Info(Interpreteur);
      while (Interpretor.Get_PC(Interpreteur) <= Memory.Length(Interpretor.Get_Memory(Interpreteur)) and not Evaluator.Is_End_Of_Program(Interpretor.Get_IR(Interpreteur))) loop
         Interpretor.Interpret_Single_Instruction(Interpreteur);
         Interpretor.Display_Infos(Interpreteur);
         Interpretor.Display_Single_Info(Interpreteur);
         if User_Input /= To_Unbounded_String("exec") then
            Put_Line("");
            Put_Line("Do you want to continue execution without breaks (write : exec) or execute only the next instruction (Press enter) ?");
            User_Input := To_Unbounded_String(Get_Line);
         end if;
      end loop;
   end if;
end Main;