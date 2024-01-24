with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;
with Evaluator;
with Memory;
with Register;

procedure Test_Evaluator is

    Memoire : Memory.T_Memory;
    Registre : Register.Register_Type;

    function S(Source : String) return Unbounded_String renames To_Unbounded_String;

    procedure Inits is
    begin
        Memory.Init(Memoire);
        Memory.Append(Memoire, (Token1 => S("PROGRAM"), Token2 => S(""), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("INIT"), Token2 => S("T1"), Token3 => S("INTEGER"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("INIT"), Token2 => S("T2"), Token3 => S("INTEGER"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("INIT"), Token2 => S("T3"), Token3 => S("INTEGER"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("INIT"), Token2 => S("T4"), Token3 => S("BOOLEAN"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("BEGIN"), Token2 => S(""), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("LABEL"), Token2 => S("L1"), Token3 => S("4"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("T1"), Token2 => S("2"), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("T2"), Token2 => S("4"), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("+"), Token4 => S("T2")));
        Memory.Append(Memoire, (Token1 => S("T4"), Token2 => S("T3"), Token3 => S("="), Token4 => S("6")));
        Memory.Append(Memoire, (Token1 => S("IF"), Token2 => S("T4"), Token3 => S("GOTO"), Token4 => S("L1")));
        Memory.Append(Memoire, (Token1 => S("GOTO"), Token2 => S("L1"), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("LABEL"), Token2 => S("L2"), Token3 => S("10"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("NULL"), Token2 => S(""), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("END"), Token2 => S(""), Token3 => S(""), Token4 => S("")));
    end Inits;

    --Registre.Add_Variable(Registre,"L1","LABEL","");
    --Registre.Add_Variable(Registre,"L2","LABEL","");
    --Registre.Add_Variable(Registre,"T1","INTEGER","");
    --Registre.Add_Variable(Registre,"T2","INTEGER","");
    --Registre.Add_Variable(Registre,"T3","INTEGER","");
    --Registre.Add_Variable(Registre,"T4","BOOLEAN","");

    procedure Test_Init_Variable is

    begin

    end Test_Init_Variable;

    procedure Init_Label is
        
    begin

    end Init_Label;

    procedure Test_Assign_Value is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
    begin
        Register.Init(Registre);
        Register.Add_Variable(Registre,S("T1"),Register.T_Entier,S(""));
        IR := (Token1 => S("T1"), Token2 => S("2"), Token3 => S(""), Token4 => S(""));
        Evaluator.Assign_Value(IR, Memoire, Registre);
        Rec := Register.Get_Variable(Registre,S("T1"));
        pragma Assert(Rec.Value = "2");
        Put("hfgd");
    end Test_Assign_Value;

    procedure Test_Assign_With_Operation is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
    begin
        Register.Init(Registre);
        Register.Add_Variable(Registre,S("T1"),Register.T_Entier,S("2"));
        Register.Add_Variable(Registre,S("T2"),Register.T_Entier,S("4"));
        Register.Add_Variable(Registre,S("T3"),Register.T_Entier,S(""));
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("+"), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Memoire, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = "6");
    end Test_Assign_With_Operation;

    procedure Test_Conditional_Branch is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        PC : Integer;
    begin
        Register.Init(Registre);
        PC := 8;
        Register.Add_Variable(Registre,S("L1"),Register.T_Entier,S("4"));
        Register.Add_Variable(Registre,S("T4"),Register.T_Booleen,S("1"));
        IR := (Token1 => S("IF"), Token2 => S("T4"), Token3 => S("GOTO"), Token4 => S("L1"));
        Evaluator.Conditional_Branch(IR, Memoire, Registre, PC);
        pragma Assert(PC = 4);
    end Test_Conditional_Branch;

    procedure Test_Unconditional_Branch is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        PC : Integer;
    begin
        Register.Init(Registre);
        PC := 6;
        Register.Add_Variable(Registre,S("L1"),Register.T_Entier,S("10"));
        IR := (Token1 => S("GOTO"), Token2 => S("L1"), Token3 => S(""), Token4 => S(""));
        Evaluator.Unconditional_Branch(IR, Memoire, Registre, PC);
        pragma Assert(PC = 10);
    end Test_Unconditional_Branch;

    procedure Test_Read_Variable is
        IR : Memory.T_Instructions;
        Rec : Register.Variable_Record;
        Registre : Register.Register_Type;
    begin
        Register.Init(Registre);
        Register.Add_Variable(Registre,S("T1"),Register.T_Entier,S(""));
        IR := (Token1 => S("READ"), Token2 => S("T1"), Token3 => S(""), Token4 => S(""));
        Put_Line("Ecrire = 10 pour voir si test correct");
        Evaluator.Read_Variable(IR, Memoire, Registre);
        Rec := Register.Get_Variable(Registre,S("T1"));
        pragma Assert(Rec.Value = S("10"));
    end Test_Read_Variable;

    procedure Test_Write_Variable is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
    begin
        Register.Init(Registre);
        Register.Add_Variable(Registre,S("T1"),Register.T_Entier,S("5"));
        IR := (Token1 => S("WRITE"), Token2 => S("T1"), Token3 => S(""), Token4 => S(""));
        Evaluator.Write_Variable(IR, Memoire, Registre);
        Put_Line("Valeur afficher = 5 si test correct");
    end Test_Write_Variable;

    procedure Test_Null_Operation is
        IR : Memory.T_Instructions;
    begin
        Register.Init(Registre);
        IR := (Token1 => S("NULL"), Token2 => S(""), Token3 => S(""), Token4 => S(""));
        Evaluator.Null_Operation(IR);
        pragma Assert(IR.Token1 = S("NULL"));
    end Test_Null_Operation;

    procedure Test_Evaluate_And_Execute is
    
    begin

    end Test_Evaluate_And_Execute;

begin
    Inits;
    Put("passed fd");
    Test_Assign_Value;
    Put("passed 2");
    Test_Assign_With_Operation;
    Put("passed 3");
    Test_Conditional_Branch;
    Put("passed 4");
    Test_Unconditional_Branch;
    Put("passed 5");
    Test_Read_Variable;
    Put("passed 6");
    Test_Write_Variable;
    Put("passed 7");
    Test_Null_Operation;
    Test_Evaluate_And_Execute;

end Test_Evaluator;