with Ada.Text_IO; use Ada.Text_IO;
with Interpretor; use Interpretor;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Memory;
with Register; use Register;
with Common_Types; use Common_Types;

procedure Test_Interpreteur is

    --  procedure Test_Init is
    --      type Instruction is record
    --          Token1 : String(1..50);
    --          Token2 : String(1..50);
    --          Token3 : String(1..50);
    --          Token4 : String(1..50);
    --      end record;
    --  begin
    --      Init(PC, IR, Registers, Memory);
    --      Put_Line("Init done");
    --  end Test_Init;

    --  procedure Test_Get_PC(PC : T_PC) is
    --  begin
    --      Put("PC: "); Put(Get_PC(PC)); New_Line;
    --  end Test_Get_PC;

    --  procedure Test_Increment_PC(PC : in out T_PC) is
    --  begin
    --      Increment_PC(PC);
    --      Put("Incremented PC: "); Put(Get_PC(PC)); New_Line;
    --  end Test_Increment_PC;

    --  procedure Test_Display_Info(Memory : T_Memory; Registers : T_Register; PC : T_PC) is
    --  begin
    --      Display_Info(Memory, Registers, PC);
    --  end Test_Display_Info;

    --  procedure Test_Interpret_Single_Instruction(IR : in out T_IR; Memory : in out T_Memory; Registers : in out T_Register; PC : in out T_PC) is
    --  begin
    --      Interpret_Single_Instruction(IR, Memory, Registers, PC);
    --      Put("Interpret_Single_Instruction done");
    --  end Test_Interpret_Single_Instruction;

    --  procedure Test_Multiple_Interpret_Single_Instruction(IR : in out T_IR; Memory : in out T_Memory; Registers : in out T_Register; PC : in out T_PC) is
    --  begin
    --      for I in 1 .. 5 loop
    --          Interpret_Single_Instruction(IR, Memory, Registers, PC);
    --          Put("After Interpret_Single_Instruction, PC: "); Put(Get_PC(PC)); New_Line;
    --          Put("IR: "); Put(Get_IR_Value(IR)); New_Line;
    --          pragma Assert(Get_PC(PC) = I, "PC should increment");
    --          pragma Assert(Get_IR_Value(IR) /= "", "IR should not be empty");
    --      end loop;
    --  end Test_Multiple_Interpret_Single_Instruction;

    --  procedure Test_Read_File_Simulation(IR : in out T_IR; Memory : in out T_Memory; Registers : in out T_Register; PC : in out T_PC) is
    --      Memoire : Memory.T_Memory;
    --      Instr : Instruction;
    --  begin
    --      Memory.Init(Memoire);
    --      Instr := (Token1 => "INIT", Token2 => "n", Token3 => "5", Token4 => "");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "INIT", Token2 => "i", Token3 => "1", Token4 => "");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "INIT", Token2 => "Fact", Token3 => "1", Token4 => "");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "T1", Token2 => "i", Token3 => "<", Token4 => "n");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "T2", Token2 => "i", Token3 => "=", Token4 => "n");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "L1", Token2 => "7", Token3 => "=", Token4 => "n");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "IF", Token2 => "T3", Token3 => "GOTO", Token4 => "L3");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "GOTO", Token2 => "L2", Token3 => "", Token4 => "");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "L3", Token2 => "9", Token3 => "=", Token4 => "n");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "Fact", Token2 => "Fact", Token3 => "*", Token4 => "i");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "i", Token2 => "i", Token3 => "+", Token4 => "1");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "T1", Token2 => "i", Token3 => "<", Token4 => "n");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "T1", Token2 => "i", Token3 => "=", Token4 => "n");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "T3", Token2 => "T1", Token3 => "OR", Token4 => "T2");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "GOTO", Token2 => "L1", Token3 => "", Token4 => "");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "L2", Token2 => "15", Token3 => "=", Token4 => "n");
    --      Memory.Append(Memoire, Instr);
    --      Instr := (Token1 => "NULL", Token2 => "", Token3 => "", Token4 => "");
    --      Memory.Append(Memoire, Instr);

    --      Interpret_Single_Instruction(IR, Memoire, Registers, PC);

    --      Put("After Interpret_Single_Instruction, PC: "); Put(Get_PC(PC)); New_Line;
    --      Put("IR: "); Put(Get_IR_Value(IR)); New_Line;
    --      pragma Assert(Get_PC(PC) = 1, "PC should be equal to 1");
    --      pragma Assert(Get_IR_Value(IR) /= "", "IR should not be empty");
    --  end Test_Read_File_Simulation;


    procedure Test_Interpretor is
        Interpreteur : T_Interpretor;
        IR : Memory.T_Instructions;
    begin
        Init("tests/test_interpretor.txt", Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 1);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S(""));
        pragma Assert(IR.Token2 = S(""));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 2);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("PROGRAM"));
        pragma Assert(IR.Token2 = S(""));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 3);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("n"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 4);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("i"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 5);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("Fact"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 6);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("T1"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 7);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("T2"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 8);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("INIT"));
        pragma Assert(IR.Token2 = S("T3"));
        pragma Assert(IR.Token3 = S("Entier"));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 9);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("N"));
        pragma Assert(IR.Token2 = S("5"));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 10);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("i"));
        pragma Assert(IR.Token2 = S("1"));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 11);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("Fact"));
        pragma Assert(IR.Token2 = S("1"));
        pragma Assert(IR.Token3 = S(""));
        pragma Assert(IR.Token4 = S(""));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 12);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("T1"));
        pragma Assert(IR.Token2 = S("i"));
        pragma Assert(IR.Token3 = S("<"));
        pragma Assert(IR.Token4 = S("n"));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 13);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("T2"));
        pragma Assert(IR.Token2 = S("i"));
        pragma Assert(IR.Token3 = S("="));
        pragma Assert(IR.Token4 = S("n"));


        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 14);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("T3"));
        pragma Assert(IR.Token2 = S("T1"));
        pragma Assert(IR.Token3 = S("OR"));
        pragma Assert(IR.Token4 = S("T2"));

        for I in 1..3 loop
            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 15);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("IF"));
            pragma Assert(IR.Token2 = S("T3"));
            pragma Assert(IR.Token3 = S("GOTO"));
            pragma Assert(IR.Token4 = S("L3"));


            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 17);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("Fact"));
            pragma Assert(IR.Token2 = S("Fact"));
            pragma Assert(IR.Token3 = S("*"));
            pragma Assert(IR.Token4 = S("i"));


            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 18);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("i"));
            pragma Assert(IR.Token2 = S("i"));
            pragma Assert(IR.Token3 = S("+"));
            pragma Assert(IR.Token4 = S("1"));


            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 19);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("T1"));
            pragma Assert(IR.Token2 = S("i"));
            pragma Assert(IR.Token3 = S("<"));
            pragma Assert(IR.Token4 = S("n"));


            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 20);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("T2"));
            pragma Assert(IR.Token2 = S("i"));
            pragma Assert(IR.Token3 = S("="));
            pragma Assert(IR.Token4 = S("n"));

            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 21);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("T3"));
            pragma Assert(IR.Token2 = S("T1"));
            pragma Assert(IR.Token3 = S("OR"));
            pragma Assert(IR.Token4 = S("T2"));

            
            Interpretor.Interpret_Single_Instruction(Interpreteur);
            pragma Assert(Get_PC(Interpreteur) = 21);
            IR := Get_IR(Interpreteur);
            pragma Assert(IR.Token1 = S("GOTO"));
            pragma Assert(IR.Token2 = S("L1"));
            pragma Assert(IR.Token3 = S(""));
            pragma Assert(IR.Token4 = S(""));
        end loop;

        Interpretor.Interpret_Single_Instruction(Interpreteur);
        pragma Assert(Get_PC(Interpreteur) = 21);
        IR := Get_IR(Interpreteur);
        pragma Assert(IR.Token1 = S("IF"));
        pragma Assert(IR.Token2 = S("T3"));
        pragma Assert(IR.Token3 = S("GOTO"));
        pragma Assert(IR.Token4 = S("L3"));

    end Test_Interpretor;


begin
    --  Test_Init;
    --  Test_Get_PC(PC);
    --  Test_Increment_PC(PC);
    --  Test_Display_Info(Memory, Registers, PC);
    --  Test_Interpret_Single_Instruction(IR, Memory, Registers, PC);
    --  Test_Multiple_Interpret_Single_Instruction(IR, Memory, Registers, PC);
    Test_Interpretor;
end Test_Interpreteur;