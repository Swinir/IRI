with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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
        Memory.Append(Memoire, (Token1 => S("INIT"), Token2 => S("T1"), Token3 => S("Entier"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("INIT"), Token2 => S("T2"), Token3 => S("Entier"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("INIT"), Token2 => S("T3"), Token3 => S("Entier"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("BEGIN"), Token2 => S(""), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("LABEL"), Token2 => S("L1"), Token3 => S("4"), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("T1"), Token2 => S("2"), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("T2"), Token2 => S("3"), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("="), Token4 => S("2")));
        Memory.Append(Memoire, (Token1 => S("IF"), Token2 => S("T3"), Token3 => S("GOTO"), Token4 => S("L1")));
        Memory.Append(Memoire, (Token1 => S("NULL"), Token2 => S(""), Token3 => S(""), Token4 => S("")));
        Memory.Append(Memoire, (Token1 => S("END"), Token2 => S(""), Token3 => S(""), Token4 => S("")));
    end Inits;

    procedure Test_Init_Variable is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
    begin
        Register.Init(Registre);
        pragma Assert(Register.Length(Registre) = 0);
        IR := (Token1 => S("INIT"), Token2 => S("T1"), Token3 => S("Entier"), Token4 => S(""));
        Evaluator.Init_Variable(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T1"));
        pragma Assert(Register.Length(Registre) = 1);
        pragma Assert(Rec.Value = S(""));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Entier));
        IR := (Token1 => S("INIT"), Token2 => S("T2"), Token3 => S("Caractere"), Token4 => S(""));
        Evaluator.Init_Variable(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T2"));
        pragma Assert(Register.Length(Registre) = 2);
        pragma Assert(Rec.Value = S(""));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Caractere));
        IR := (Token1 => S("INIT"), Token2 => S("T3"), Token3 => S("Booleen"), Token4 => S(""));
        Evaluator.Init_Variable(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Register.Length(Registre) = 3);
        pragma Assert(Rec.Value = S(""));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Booleen));
        IR := (Token1 => S("INIT"), Token2 => S("T4"), Token3 => S("Entier"), Token4 => S(""));
        Evaluator.Init_Variable(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T4"));
        pragma Assert(Register.Length(Registre) = 4);
        pragma Assert(Rec.Value = S(""));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Entier));
    end Test_Init_Variable;

    procedure Test_Init_Label is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
    begin
        Register.Init(Registre);
        pragma Assert(Register.Length(Registre) = 0);
        IR := (Token1 => S("LABEL"), Token2 => S("L1"), Token3 => S("5"), Token4 => S(""));
        Evaluator.Init_Label(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("L1"));
        pragma Assert(Register.Length(Registre) = 1);
        pragma Assert(Rec.Value = S("5"));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Label));
        IR := (Token1 => S("LABEL"), Token2 => S("L2"), Token3 => S("10"), Token4 => S(""));
        Evaluator.Init_Label(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("L2"));
        pragma Assert(Register.Length(Registre) = 2);
        pragma Assert(Rec.Value = S("10"));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Label));
        IR := (Token1 => S("LABEL"), Token2 => S("L3"), Token3 => S("2"), Token4 => S(""));
        Evaluator.Init_Label(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("L3"));
        pragma Assert(Register.Length(Registre) = 3);
        pragma Assert(Rec.Value = S("2"));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Label));
    end Test_Init_Label;

    procedure Test_Assign_Value is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
    begin
        Register.Init(Registre);
        Register.Add_Variable(Registre,S("T1"), Register.T_Entier,S(""));
        IR := (Token1 => S("T1"), Token2 => S("2"), Token3 => S(""), Token4 => S(""));
        Evaluator.Assign_Value(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T1"));
        pragma Assert(Rec.Value = S("2"));
    end Test_Assign_Value;

    procedure Test_Assign_With_Operation is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
    begin
        Register.Init(Registre);
        Register.Add_Variable(Registre,S("T1"), Register.T_Entier, S("2"));
        Register.Add_Variable(Registre,S("T2"), Register.T_Entier, S("4"));
        Register.Add_Variable(Registre,S("T3"), Register.T_Entier, S(""));
        Register.Add_Variable(Registre,S("T4"), Register.T_Entier, S("6"));
        Register.Add_Variable(Registre,S("T5"), Register.T_Entier, S("12"));
        -- +
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("+"), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("6"));
        -- -
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("-"), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("-2"));
        -- /
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("/"), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("0"));
        -- *
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("*"), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("8"));
        -- AND
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("AND"), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("0"));

        IR := (Token1 => S("T3"), Token2 => S("T4"), Token3 => S("AND"), Token4 => S("T5"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("4"));
        -- OR
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("OR"), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("6"));

        IR := (Token1 => S("T3"), Token2 => S("T4"), Token3 => S("OR"), Token4 => S("T5"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("14"));
        -- =
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("="), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("0"));

        Register.Add_Variable(Registre,S("T6"),Register.T_Entier,S("2"));
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("="), Token4 => S("T6"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("1"));
        -- >
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S(">"), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("0"));

        IR := (Token1 => S("T3"), Token2 => S("T2"), Token3 => S(">"), Token4 => S("T1"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("1"));
        -- <
        IR := (Token1 => S("T3"), Token2 => S("T1"), Token3 => S("<"), Token4 => S("T2"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("1"));

        IR := (Token1 => S("T3"), Token2 => S("T2"), Token3 => S("<"), Token4 => S("T1"));
        Evaluator.Assign_With_Operation(IR, Registre);
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("0"));
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
        Evaluator.Conditional_Branch(IR, Registre, PC);
        pragma Assert(PC = 3);
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
        Evaluator.Unconditional_Branch(IR, Registre, PC);
        pragma Assert(PC = 9);
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
        Evaluator.Read_Variable(IR, Registre);
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
        Evaluator.Write_Variable(IR, Registre);
        Put_Line("Valeur afficher = 5 si test correct");
    end Test_Write_Variable;

    procedure Test_Null_Operation is
        IR : Memory.T_Instructions;
    begin
        Register.Init(Registre);
        IR := (Token1 => S("NULL"), Token2 => S(""), Token3 => S(""), Token4 => S(""));
        Evaluator.Null_Operation;
        pragma Assert(IR.Token1 = S("NULL"));
    end Test_Null_Operation;

    procedure Test_Evaluate_And_Execute is
        IR : Memory.T_Instructions;
        PC : Integer;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
    begin
        Register.Init(Registre);
        PC := 0;
        for I in 1..Memory.Length(Memoire) loop
            IR := Memory.Get_Data(Memoire, I);
            Evaluator.Evaluate_And_Execute(IR, Registre, PC);
        end loop;
        Rec := Register.Get_Variable(Registre,S("T1"));
        pragma Assert(Rec.Value = S("2"));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Entier));
        Rec := Register.Get_Variable(Registre,S("T2"));
        pragma Assert(Rec.Value = S("3"));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Entier));
        Rec := Register.Get_Variable(Registre,S("T3"));
        pragma Assert(Rec.Value = S("1"));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Entier));
        Rec := Register.Get_Variable(Registre,S("L1"));
        pragma Assert(Rec.Value = S("4"));
        pragma Assert(Register.T_Types'Pos(Rec.T_Type) = Register.T_Types'Pos(Register.T_Label));

        pragma Assert(PC = 3);
    end Test_Evaluate_And_Execute;

    procedure Test_Array is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
        PC : Integer;
    begin
        PC := 1;
        Register.Init(Registre);
        IR := (Token1 => S("INIT"), Token2 => S("T1"), Token3 => S("Entier"), Token4 => S("TAB:3"));
        Evaluator.Evaluate_And_Execute(IR, Registre, PC);
        Rec := Register.Get_Variable(Registre,S("T1(1)"));
        pragma Assert(Rec.Value = S(""));
        Rec := Register.Get_Variable(Registre,S("T1(2)"));
        pragma Assert(Rec.Value = S(""));
        Rec := Register.Get_Variable(Registre,S("T1(3)"));
        pragma Assert(Rec.Value = S(""));
        IR := (Token1 => S("T1(2)"), Token2 => S("2"), Token3 => S(""), Token4 => S(""));
        Evaluator.Evaluate_And_Execute(IR, Registre, PC);
        Rec := Register.Get_Variable(Registre,S("T1(2)"));
        pragma Assert(Rec.Value = S("2"));
        Register.Add_Variable(Registre,S("I"),Register.T_Entier,S("1"));
        IR := (Token1 => S("T1(I)"), Token2 => S("4"), Token3 => S(""), Token4 => S(""));
        Evaluator.Evaluate_And_Execute(IR, Registre, PC);
        Rec := Register.Get_Variable(Registre,S("T1(1)"));
        pragma Assert(Rec.Value = S("4"));
        IR := (Token1 => S("T1(3)"), Token2 => S("T1(1)"), Token3 => S("+"), Token4 => S("T1(2)"));
        Evaluator.Evaluate_And_Execute(IR, Registre, PC);
        Rec := Register.Get_Variable(Registre,S("T1(3)"));
        pragma Assert(Rec.Value = S("6"));
        Register.Add_Variable(Registre,S("T9"),Register.T_Entier,S(""));
        Register.Add_Variable(Registre,S("T8"),Register.T_Entier,S("2"));
        Register.Add_Variable(Registre,S("J"),Register.T_Entier,S("3"));
        IR := (Token1 => S("T9"), Token2 => S("T1(J)"), Token3 => S(">"), Token4 => S("T1(T8)"));
        Evaluator.Evaluate_And_Execute(IR, Registre, PC);
        Rec := Register.Get_Variable(Registre,S("T9"));
        pragma Assert(Rec.Value = S("1"));
    end Test_Array;

    procedure Test_Character is
        IR : Memory.T_Instructions;
        Registre : Register.Register_Type;
        Rec : Register.Variable_Record;
        PC : Integer;
    begin
        PC := 1;
        Register.Init(Registre);
        IR := (Token1 => S("INIT"), Token2 => S("T1"), Token3 => S("Caractere"), Token4 => S(""));
        Evaluator.Evaluate_And_Execute(IR, Registre, PC);
        Rec := Register.Get_Variable(Registre,S("T1"));
        pragma Assert(Rec.Value = S(""));
        IR := (Token1 => S("T1"), Token2 => S("'2'"), Token3 => S(""), Token4 => S(""));
        Evaluator.Evaluate_And_Execute(IR, Registre, PC);
        Rec := Register.Get_Variable(Registre,S("T1"));
        pragma Assert(Rec.Value = S("50"));
        IR := (Token1 => S("T1"), Token2 => S("'B'"), Token3 => S(""), Token4 => S(""));
        Evaluator.Evaluate_And_Execute(IR, Registre, PC);
        Rec := Register.Get_Variable(Registre,S("T1"));
        pragma Assert(Rec.Value = S("66"));

        for Index in 1..Register.Length(Registre) loop
            Put_Line("------------------------------------------------");
            Put("-- Registry index value :" & Integer'Image(Index) & "  |  ");
            Register.Put(Register.Variable_List.Get_Data(Registre, Index));
            Put_Line("");
        end loop;
    end Test_Character;

begin
    Inits;
    Test_Init_Variable;
    Test_Init_Label;
    Test_Assign_Value;
    Test_Assign_With_Operation;
    Test_Conditional_Branch;
    Test_Unconditional_Branch;
    Test_Read_Variable;
    Test_Write_Variable;
    Test_Null_Operation;
    Test_Evaluate_And_Execute;
    Test_Array;
    Test_Character;

end Test_Evaluator;