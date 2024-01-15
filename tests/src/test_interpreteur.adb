with Ada.Text_IO; use Ada.Text_IO;
with Interpreteur; use Interpreteur;
with Ada.Assertions; use Ada.Assertions;
with Memory;
with Register; use Register;

procedure Test_Interpreteur is
    PC : T_PC;
    IR : T_IR;
    Registers : T_Register;
    Memory : Memory.T_Memory;

    procedure Test_Init is
        type Instruction is record
            Token1 : String(1..50);
            Token2 : String(1..50);
            Token3 : String(1..50);
            Token4 : String(1..50);
        end record;
    begin
        Init(PC, IR, Registers, Memory);
        Put_Line("Init done");
    end Test_Init;

    procedure Test_Get_PC(PC : T_PC) is
    begin
        Put("PC: "); Put(Get_PC(PC)); New_Line;
    end Test_Get_PC;

    procedure Test_Increment_PC(PC : in out T_PC) is
    begin
        Increment_PC(PC);
        Put("Incremented PC: "); Put(Get_PC(PC)); New_Line;
    end Test_Increment_PC;

    procedure Test_Display_Info(Memory : T_Memory; Registers : T_Register; PC : T_PC) is
    begin
        Display_Info(Memory, Registers, PC);
    end Test_Display_Info;

    procedure Test_Interpret_Single_Instruction(IR : in out T_IR; Memory : in out T_Memory; Registers : in out T_Register; PC : in out T_PC) is
    begin
        Interpret_Single_Instruction(IR, Memory, Registers, PC);
        Put("Interpret_Single_Instruction done");
    end Test_Interpret_Single_Instruction;

    procedure Test_Multiple_Interpret_Single_Instruction(IR : in out T_IR; Memory : in out T_Memory; Registers : in out T_Register; PC : in out T_PC) is
    begin
        for I in 1 .. 5 loop
            Interpret_Single_Instruction(IR, Memory, Registers, PC);
            Put("After Interpret_Single_Instruction, PC: "); Put(Get_PC(PC)); New_Line;
            Put("IR: "); Put(Get_IR_Value(IR)); New_Line;
            pragma Assert(Get_PC(PC) = I, "PC should increment");
            pragma Assert(Get_IR_Value(IR) /= "", "IR should not be empty");
        end loop;
    end Test_Multiple_Interpret_Single_Instruction;

    procedure Test_Read_File_Simulation(IR : in out T_IR; Memory : in out T_Memory; Registers : in out T_Register; PC : in out T_PC) is
        Memoire : Memory.T_Memory;
        Instr : Instruction;
    begin
        Memory.Init(Memoire);
        Instr := (Token1 => "INIT", Token2 => "n", Token3 => "5", Token4 => "");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "INIT", Token2 => "i", Token3 => "1", Token4 => "");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "INIT", Token2 => "Fact", Token3 => "1", Token4 => "");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "T1", Token2 => "i", Token3 => "<", Token4 => "n");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "T2", Token2 => "i", Token3 => "=", Token4 => "n");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "L1", Token2 => "7", Token3 => "=", Token4 => "n");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "IF", Token2 => "T3", Token3 => "GOTO", Token4 => "L3");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "GOTO", Token2 => "L2", Token3 => "", Token4 => "");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "L3", Token2 => "9", Token3 => "=", Token4 => "n");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "Fact", Token2 => "Fact", Token3 => "*", Token4 => "i");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "i", Token2 => "i", Token3 => "+", Token4 => "1");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "T1", Token2 => "i", Token3 => "<", Token4 => "n");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "T1", Token2 => "i", Token3 => "=", Token4 => "n");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "T3", Token2 => "T1", Token3 => "OR", Token4 => "T2");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "GOTO", Token2 => "L1", Token3 => "", Token4 => "");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "L2", Token2 => "15", Token3 => "=", Token4 => "n");
        Memory.Append(Memoire, Instr);
        Instr := (Token1 => "NULL", Token2 => "", Token3 => "", Token4 => "");
        Memory.Append(Memoire, Instr);

        Interpret_Single_Instruction(IR, Memoire, Registers, PC);

        Put("After Interpret_Single_Instruction, PC: "); Put(Get_PC(PC)); New_Line;
        Put("IR: "); Put(Get_IR_Value(IR)); New_Line;
        pragma Assert(Get_PC(PC) = 1, "PC should be equal to 1");
        pragma Assert(Get_IR_Value(IR) /= "", "IR should not be empty");
    end Test_Read_File_Simulation;

    procedure Test_Interpret(IR : in out T_IR; Memory : in out T_Memory; Registers : in out T_Register; PC : in out T_PC) is
    begin
        Interpret("path_to_program", IR, Memory, Registers, PC);
        Put("Interpret done");
    end Test_Interpret;


begin
    Test_Init;
    Test_Get_PC(PC);
    Test_Increment_PC(PC);
    Test_Display_Info(Memory, Registers, PC);
    Test_Interpret_Single_Instruction(IR, Memory, Registers, PC);
    Test_Multiple_Interpret_Single_Instruction(IR, Memory, Registers, PC);
    Test_Interpret(IR, Memory, Registers, PC);
end Test_Interpreteur;