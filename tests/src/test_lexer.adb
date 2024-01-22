with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Lexer; use Lexer;
with Memory; use Memory;
with Common_Types; use Common_Types;

procedure Test_Lexer is
   Lignes : String_List;
   Memoire : T_Memory;
   Instructions : Memory.T_Instructions;
   Instruction_Test : Memory.T_Instructions;
   Mots : T_Words_List;
   Index : Integer;
begin
   Common_Types.Init(Lignes);
   Memory.Init(Memoire);
   Common_Types.Append(Lignes, To_Unbounded_String("Programme toto"));
   Common_Types.Append(Lignes, To_Unbounded_String("N <- 2"));
   Common_Types.Append(Lignes, To_Unbounded_String("T3 <- T1 OR T2"));
   Common_Types.Append(Lignes, To_Unbounded_String("GOTO L1"));
   pragma Assert(Common_Types.Get_Data(Lignes, 1) = To_Unbounded_String("Programme toto"));
   pragma Assert(Common_Types.Get_Data(Lignes, 2) = To_Unbounded_String("N <- 2"));
   pragma Assert(Common_Types.Get_Data(Lignes, 3) = To_Unbounded_String("T3 <- T1 OR T2"));
   pragma Assert(Common_Types.Get_Data(Lignes, 4) = To_Unbounded_String("GOTO L1"));
   Analyser_Lignes(Lignes, Memoire);
   Memory.Put(Memory.Get_Data(Memoire, 1));
   Memory.Put(Memory.Get_Data(Memoire, 2));
   Memory.Put(Memory.Get_Data(Memoire, 3));
   Memory.Put(Memory.Get_Data(Memoire, 4));
   
   Instruction_Test.Token1 := To_Unbounded_String("PROGRAM");
   pragma Assert(Memory.Get_Data(Memoire, 1) = Instruction_Test);
   Instruction_Test.Token1 := To_Unbounded_String("N");
   Instruction_Test.Token2 := To_Unbounded_String("2");
   pragma Assert(Memory.Get_Data(Memoire, 2) = Instruction_Test);
   Instruction_Test.Token1 := To_Unbounded_String("T3");
   Instruction_Test.Token2 := To_Unbounded_String("T1");
   Instruction_Test.Token3 := To_Unbounded_String("OR");
   Instruction_Test.Token4 := To_Unbounded_String("T2");
   pragma Assert(Memory.Get_Data(Memoire, 3) = Instruction_Test);
   Instruction_Test.Token1 := To_Unbounded_String("GOTO");
   Instruction_Test.Token2 := To_Unbounded_String("L1");
   Instruction_Test.Token3 := To_Unbounded_String("");
   Instruction_Test.Token4 := To_Unbounded_String("");
   pragma Assert(Memory.Get_Data(Memoire, 4) = Instruction_Test);
   Put_Line("Analyser_Lignes test passed");

   -- Test for Extraire_Mots
   Mots := Extraire_Mots(To_Unbounded_String("T3 <- T1 OR T2"));
   Put_Line("Extraire_Mots test passed");

   -- Test for Process_Keywords
   Index := 1;
   Process_Keywords(Mots, Index, Instructions);
   Put_Line("Process_Keywords test passed");

   -- Test for Process_Function
   Common_Types.Clear(Mots);
   Common_Types.Append(Mots, To_Unbounded_String("Programme"));
   Process_Function(Mots, Instructions);
   Put_Line("Process_Function test passed");

   -- Test for Process_Var_Init
   Common_Types.Clear(Mots);
   Memory.Clear(Memoire);
   Memory.Init(Memoire);
   Common_Types.Init(Mots);
   Common_Types.Append(Mots, To_Unbounded_String("N,"));
   Common_Types.Append(Mots, To_Unbounded_String("a,"));
   Common_Types.Append(Mots, To_Unbounded_String("d,"));
   Common_Types.Append(Mots, To_Unbounded_String("fdjbhfqjfoifjojfdslkmjfklqjml"));
   Common_Types.Append(Mots, To_Unbounded_String(":"));
   Common_Types.Append(Mots, To_Unbounded_String("Entier"));
   Process_Var_Init(Mots, Memoire);
   Memory.Put(Memory.Get_Data(Memoire, 1));
   Memory.Put(Memory.Get_Data(Memoire, 2));
   Memory.Put(Memory.Get_Data(Memoire, 3));
   Memory.Put(Memory.Get_Data(Memoire, 4));
   Instruction_Test.Token1 := To_Unbounded_String("INIT");
   Instruction_Test.Token2 := To_Unbounded_String("N");
   Instruction_Test.Token3 := To_Unbounded_String("Entier");
   Instruction_Test.Token4 := To_Unbounded_String("");
   pragma Assert(Memory.Get_Data(Memoire, 1) = Instruction_Test);
   Instruction_Test.Token1 := To_Unbounded_String("INIT");
   Instruction_Test.Token2 := To_Unbounded_String("a");
   Instruction_Test.Token3 := To_Unbounded_String("Entier");
   pragma Assert(Memory.Get_Data(Memoire, 2) = Instruction_Test);
   Instruction_Test.Token1 := To_Unbounded_String("INIT");
   Instruction_Test.Token2 := To_Unbounded_String("d");
   Instruction_Test.Token3 := To_Unbounded_String("Entier");
   pragma Assert(Memory.Get_Data(Memoire, 3) = Instruction_Test);
   Instruction_Test.Token1 := To_Unbounded_String("INIT");
   Instruction_Test.Token2 := To_Unbounded_String("fdjbhfqjfoifjojfdslkmjfklqjml");
   Instruction_Test.Token3 := To_Unbounded_String("Entier");
   pragma Assert(Memory.Get_Data(Memoire, 4) = Instruction_Test);
   Put_Line("Process_Var_Init test passed");

   -- Test for Process_Goto
   Common_Types.Clear(Mots);
   Common_Types.Init(Mots);
   Common_Types.Append(Mots, To_Unbounded_String("GOTO"));
   Common_Types.Append(Mots, To_Unbounded_String("L1"));
   Process_Goto(Mots, Instructions);
   pragma Assert(Instructions.Token1 = To_Unbounded_String("GOTO") AND Instructions.Token2 = To_Unbounded_String("L1"));
   Put_Line("Process_Goto test passed");

   -- Test for Process_If
   Common_Types.Clear(Mots);
   Common_Types.Init(Mots);
   Common_Types.Append(Mots, To_Unbounded_String("IF"));
   Common_Types.Append(Mots, To_Unbounded_String("T1"));
   Common_Types.Append(Mots, To_Unbounded_String("GOTO"));
   Common_Types.Append(Mots, To_Unbounded_String("L1"));
   Process_If(Mots, Instructions);
   pragma Assert(Instructions.Token1 = To_Unbounded_String("IF") AND Instructions.Token2 = To_Unbounded_String("T1") AND Instructions.Token3 = To_Unbounded_String("GOTO") AND Instructions.Token4 = To_Unbounded_String("L1"));
   Put_Line("Process_If test passed");

   -- Test for Process_Label
   Common_Types.Clear(Mots);
   Common_Types.Init(Mots);
   Common_Types.Append(Mots, To_Unbounded_String("L1"));
   Common_Types.Append(Mots, To_Unbounded_String("RANDOM"));
   Common_Types.Append(Mots, To_Unbounded_String("TESTESTESTESTESTEST"));
   Process_Label(Mots, Instructions);
   pragma Assert(Instructions.Token1 = To_Unbounded_String("LABEL") AND Instructions.Token2 = To_Unbounded_String("L1"));
   Put_Line("Process_Label test passed");

   -- Test for Process_Value_Var
   Common_Types.Clear(Mots);
   Common_Types.Init(Mots);
   Common_Types.Append(Mots, To_Unbounded_String("T1"));
   Common_Types.Append(Mots, To_Unbounded_String("<-"));
   Common_Types.Append(Mots, To_Unbounded_String("25"));
   Process_Value_Variable(Mots, Instructions);
   pragma Assert(Instructions.Token1 = To_Unbounded_String("T1") AND Instructions.Token2 = To_Unbounded_String("25"));
   Put_Line("Process_Value_Var test passed");

   -- Test for Process_Value_Var with logical operator
   Common_Types.Clear(Mots);
   Common_Types.Init(Mots);
   Common_Types.Append(Mots, To_Unbounded_String("T1"));
   Common_Types.Append(Mots, To_Unbounded_String("<-"));
   Common_Types.Append(Mots, To_Unbounded_String("T2"));
   Common_Types.Append(Mots, To_Unbounded_String("AND"));
   Common_Types.Append(Mots, To_Unbounded_String("T3"));
   Process_Value_Variable(Mots, Instructions);
   pragma Assert(Instructions.Token1 = To_Unbounded_String("T1") AND Instructions.Token2 = To_Unbounded_String("T2") AND Instructions.Token3 = To_Unbounded_String("AND") AND Instructions.Token4 = To_Unbounded_String("T3"));
   Put_Line("Process_Value_Var with logical operator test passed");
end Test_Lexer;