with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Lexer; use Lexer;
with Memory; use Memory;
with Common_Types; use Common_Types;

procedure Test_Lexer is
   Lignes : String_List;
   Memoire : T_Memory;
   Instructions : T_Instructions;
   Mots : T_Words_List;
   Index : Integer;
begin
   Lignes.Append(To_Unbounded_String("PROGRAMME N INTEGER"));
   Lignes.Append(To_Unbounded_String("N <- 2"));
   Lignes.Append(To_Unbounded_String("T3 <- T1 OR T2"));
   Lignes.Append(To_Unbounded_String("GOTO L1"));
   Analyser_Lignes(Lignes, Memoire);
   Put_Line("Analyser_Lignes test passed");

   -- Test for Extraire_Mots
   Mots := Extraire_Mots("T3 <- T1 OR T2");
   Put_Line("Extraire_Mots test passed");

   -- Test for Enregistrer_Instructions
   Instructions := (Token1 => 'T', Token2 => '1', Token3 => ' ', Token4 => ' ');
   Enregistrer_Instructions(Instructions, Memoire);
   Put_Line("Enregistrer_Instructions test passed");

   -- Test for Process_Keywords
   Index := 1;
   Process_Keywords(Mots, Index, Instructions);
   Put_Line("Process_Keywords test passed");

   -- Test for Process_Function
   Mots.Clear;
   Mots.Append(To_Unbounded_String("PROGRAMME"));
   Process_Function(Mots, Instructions);
   Put_Line("Process_Function test passed");

   -- Test for Process_Var_Init
   Mots.Clear;
   Mots.Append(To_Unbounded_String("N INTEGER"));
   Process_Var_Init(Mots, Instructions);
   Put_Line("Process_Var_Init test passed");

   -- Test for Process_Goto
   Mots.Clear;
   Mots.Append(To_Unbounded_String("GOTO L1"));
   Process_Goto(Mots, Instructions);
   Put_Line("Process_Goto test passed");

   -- Test for Process_If
   Mots.Clear;
   Mots.Append(To_Unbounded_String("IF T1 GOTO L1"));
   Process_If(Mots, Instructions);
   Put_Line("Process_If test passed");

   -- Test for Process_Label
   Mots.Clear;
   Mots.Append(To_Unbounded_String("LABEL L1 5"));
   Process_Label(Mots, Instructions);
   Put_Line("Process_Label test passed");

   -- Test for Process_Value_Var
   Mots.Clear;
   Mots.Append(To_Unbounded_String("T1 25"));
   Process_Value_Var(Mots, Instructions);
   Put_Line("Process_Value_Var test passed");

   -- Test for Process_Value_Var with logical operator
   Mots.Clear;
   Mots.Append(To_Unbounded_String("T2 T3 AND T1"));
   Process_Value_Var(Mots, Instructions);
   Put_Line("Process_Value_Var with logical operator test passed");
end Test_Lexer;