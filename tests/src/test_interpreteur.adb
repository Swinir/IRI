with Ada.Text_IO; use Ada.Text_IO;
with Interpretor; use Interpretor;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Memory;
with Register; use Register;
with Common_Types; use Common_Types;

procedure Test_Interpreteur is

    procedure Test_Interpretor_Single is
        Interpreteur : T_Interpretor;
        IR : Memory.T_Instructions;
    begin
        Init("tests/test_interpretor.txt", Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 1);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("LABEL"));
        pragma Assert(IR.Token2 = S("L2"));
        pragma Assert(IR.Token3 = S("26"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 2);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("LABEL"));
        pragma Assert(IR.Token2 = S("L3"));
        pragma Assert(IR.Token3 = S("20"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 3);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("LABEL"));
        pragma Assert(IR.Token2 = S("L1"));
        pragma Assert(IR.Token3 = S("18"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 4);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("PROGRAM"));
        pragma Assert(IR.Token2 = S(""));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 5);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("n"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 6);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("i"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 7);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("Fact"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 8);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("T1"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 9);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("T2"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 10);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("T3"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));

        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 11);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("BEGIN"));
        pragma Assert(IR.Token2 = S(""));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 12);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("n"));
        pragma Assert(IR.Token2 = S("3"));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 13);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("i"));
        pragma Assert(IR.Token2 = S("1"));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 14);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("Fact"));
        pragma Assert(IR.Token2 = S("1"));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 15);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("T1"));
        pragma Assert(IR.Token2 = S("i"));
        pragma Assert(IR.Token3 = S("<"));
        pragma Assert(IR.Token4 = S("n"));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 16);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("T2"));
        pragma Assert(IR.Token2 = S("i"));
        pragma Assert(IR.Token3 = S("="));
        pragma Assert(IR.Token4 = S("n"));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 17);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("T3"));
        pragma Assert(IR.Token2 = S("T1"));
        pragma Assert(IR.Token3 = S("OR"));
        pragma Assert(IR.Token4 = S("T2"));

        for I in 1..3 loop
            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 18);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("IF"));
            pragma Assert(IR.Token2 = S("T3"));
            pragma Assert(IR.Token3 = S("GOTO"));
            pragma Assert(IR.Token4 = S("L3"));


            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 20);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("Fact"));
            pragma Assert(IR.Token2 = S("Fact"));
            pragma Assert(IR.Token3 = S("*"));
            pragma Assert(IR.Token4 = S("i"));


            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 21);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("i"));
            pragma Assert(IR.Token2 = S("i"));
            pragma Assert(IR.Token3 = S("+"));
            pragma Assert(IR.Token4 = S("1"));


            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 22);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("T1"));
            pragma Assert(IR.Token2 = S("i"));
            pragma Assert(IR.Token3 = S("<"));
            pragma Assert(IR.Token4 = S("n"));


            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 23);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("T2"));
            pragma Assert(IR.Token2 = S("i"));
            pragma Assert(IR.Token3 = S("="));
            pragma Assert(IR.Token4 = S("n"));

            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 24);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("T3"));
            pragma Assert(IR.Token2 = S("T1"));
            pragma Assert(IR.Token3 = S("OR"));
            pragma Assert(IR.Token4 = S("T2"));

            
            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 25);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("GOTO"));
            pragma Assert(IR.Token2 = S("L1"));
            pragma Assert(IR.Token3 = S(""));
            pragma Assert(IR.Token4 = S(""));
        end loop;


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 18);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("IF"));
        pragma Assert(IR.Token2 = S("T3"));
        pragma Assert(IR.Token3 = S("GOTO"));
        pragma Assert(IR.Token4 = S("L3"));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 19);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("GOTO"));
        pragma Assert(IR.Token2 = S("L2"));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 26);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("NULL"));
        pragma Assert(IR.Token2 = S(""));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 27);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("END"));
        pragma Assert(IR.Token2 = S(""));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));

    end Test_Interpretor_Single;

    procedure Test_Interpretor_All is
        Interpreteur : T_Interpretor;
        IR : Memory.T_Instructions;
    begin
        Init("tests/test_interpretor.txt", Interpreteur);
        Interpretor.Interpret_All(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 27);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("END"));
        pragma Assert(IR.Token2 = S(""));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));
    end Test_Interpretor_All;

begin
    Test_Interpretor_Single;
    Test_Interpretor_All;
end Test_Interpreteur;