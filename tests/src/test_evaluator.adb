with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Evaluator;
with Memory;
with Register;

procedure Test_Evaluator is

    Memoire : Memory.T_Memory;
    Registre : Register.Register_Type;
    Memory.Init(Memoire);
    Memory.Append(Memoire, (Token1 => "PROGRAM", Token2 => "", Token3 => "", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "INIT", Token2 => "T1", Token3 => "INTEGER", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "INIT", Token2 => "T2", Token3 => "INTEGER", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "INIT", Token2 => "T3", Token3 => "INTEGER", Token4 => ""))
    Memory.Append(Memoire, (Token1 => "INIT", Token2 => "T4", Token3 => "BOOLEAN", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "BEGIN", Token2 => "", Token3 => "", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "LABEL", Token2 => "L1", Token3 => "4", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "T1", Token2 => "2", Token3 => "", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "T2", Token2 => "4", Token3 => "", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "T3", Token2 => "T1", Token3 => "+", Token4 => "T2"));
    Memory.Append(Memoire, (Token1 => "T4", Token2 => "T3", Token3 => "=", Token4 => "6"));
    Memory.Append(Memoire, (Token1 => "IF", Token2 => "T4", Token3 => "GOTO", Token4 => "L1"));
    Memory.Append(Memoire, (Token1 => "GOTO", Token2 => "L1", Token3 => "", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "LABEL", Token2 => "L2", Token3 => "10", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "NULL", Token2 => "", Token3 => "", Token4 => ""));
    Memory.Append(Memoire, (Token1 => "END", Token2 => "", Token3 => "", Token4 => ""));

    Registre.Add_Variable(Registre,"L1","LABEL","");
    Registre.Add_Variable(Registre,"L2","LABEL","");
    Registre.Add_Variable(Registre,"T1","INTEGER","");
    Registre.Add_Variable(Registre,"T2","INTEGER","");
    Registre.Add_Variable(Registre,"T3","INTEGER","");
    Registre.Add_Variable(Registre,"T4","BOOLEAN","");

    procedure Test_Assign_Value is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
    begin
        Register.Add_Variable(Registre,"T1","INTEGER","")
        IR := (Token1 => "T1", Token2 => "2", Token3 => "", Token4 => "");
        Evaluator.Assign_Value(IR, Memoire, Registre);
        Rec := Register.Get_Variable(Registre,"T1");
        pragma Assert(Rec.Value = 2);
    end

    procedure Test_Assign_With_Operation is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
    begin
        Register.Add_Variable(Registre,"T1","INTEGER","2");
        Register.Add_Variable(Registre,"T2","INTEGER","4");
        Register.Add_Variable(Registre,"T3","INTEGER","");
        IR := (Token1 => "T3", Token2 => "T1", Token3 => "+", Token4 => "T2");
        Evaluator.Assign_With_Operation(IR, Memoire, Registre);
        Rec := Register.Get_Variable(Registre,"T3");
        pragma Assert(Rec.Value = 6);
    end

    procedure Test_Conditional_Branch is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        PC : Integer;
    begin
        PC := 8;
        Register.Add_Variable(Registre,"L1","INTEGER","4");
        Register.Add_Variable(Registre,"T4","BOOLEAN","1");
        IR := (Token1 => "IF", Token2 => "T4", Token3 => "GOTO", Token4 => "L1");
        Evaluator.Conditional_Branch(IR, Memoire, Registre, PC);
        pragma Assert(PC = 4);
    end

    procedure Test_Unconditional_Branch is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        PC : Integer;
    begin
        PC := 6;
        Register.Add_Variable(Registre,"L1","INTEGER","10");
        IR := (Token1 => "GOTO", Token2 => "L1", Token3 => "", Token4 => "");
        Evaluator.Unconditional_Branch(IR, Memoire, Registre, PC);
        pragma Assert(PC = 10);
    end

    procedure Test_Read_Variable is
        IR : Memory.T_Instructions;
        Rec : Register.Variable_Record;
        Registre : Register.Register_Type;
    begin
        Register.Add_Variable(Registre,"T1","INTEGER","");
        IR := (Token1 => "READ", Token2 => "T1", Token3 => "");
        Put_Line("Ecrire = 10 pour voir si test correct");
        Evaluator.Read_Variable(IR, Memoire, Registre);
        Rec := Register.Get_Variable(Registre,"T1");
        pragma Assert(Rec.Value = 10);
    end

    procedure Test_Write_Variable is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
    begin
        Register.Add_Variable(Registre,"T1","INTEGER","5");
        IR := (Token1 => "WRITE", Token2 => "T1", Token3 => "", Token4 => "");
        Evaluator.Write_Variable(IR, Memoire, Registre);
        Put_Line("Valeur afficher = 5 si test correct");
    end

    procedure Test_Null_Operation is
        IR : Memory.T_Instructions;
    begin
        IR := (Token1 => "NULL", Token2 => "", Token3 => "", Token4 => "");
        Evaluator.Null_Operation(IR);
        pragma Assert(IR);
    end

begin

end Test_Evaluator;