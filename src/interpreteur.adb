with Ada.Text_IO; use Ada.Text_IO;

package body interpreteur is

    procedure Init(PC : out T_PC; IR : out Memory.T_Instructions; Registers : out Register.Register_Type; Memoire : out Memory.T_Memory) is
    begin
        PC := 0;
        Register.Init(Registers);
        Memory.Init(Memoire);
    end Init;

    function Get_PC(PC : in T_PC) return Integer is
    begin
        return PC'Access;
    end Get_PC;

    function Get_IR_Value(IR : in Memory.T_Instructions) return Unbounded_String is
    begin
        Return IR.Token1 & IR.Token2 & IR.Token3 & IR.Token4;
    end Get_IR_Value;

    procedure Increment_PC(PC : in out T_PC) is
    begin
        PC := PC + 1;
    end Increment_PC;

    procedure Display_Info(Memoire : in Memory.T_Memory; Registers : in Register.Register_Type; PC : in T_PC) is
    begin
        for Index in 1..Memory.Length(Memoire) loop
            Memory.Put(Memory.Get_Data(Memoire, Index));
        end loop;
        Put("----------- PC Value : ");
        Put(Integer(Get_PC(PC)));
        Put_Line(" -------------");
        Put_Line("----------- Register Content -----------");
        for Index in 1..Register.Length(Registers) loop
            Put_Line("Register value position :" & Integer'Image(Index));
            Register.Put(Register.Get_Data(Registers, Index));
        end loop;
        Put_Line("----------------------------------------");
    end Display_Info;


    procedure Interpret_Single_Instruction(IR : in out Memory.T_Instructions; Registre : in out Register.Register_Type; PC : in out T_PC) is
    begin
        Evaluator.Evaluate_And_Execute(IR, Registre, Integer(PC));
    end Interpret_Single_Instruction;


    procedure Interpret(Path : in Unbounded_String) is
        Handle : File_Handle;
        File_Content_String : Unbounded_String;
    begin
        Init(PC, IR, Register, Memoire);
        Reader.Open_File(Path => Path, Handle => Handle);
        File_Content_String := Read_Entire_File(Handle => Handle);
        Lexer.Analyser_Lignes(File_Content_String, Memoire);
        for Index in 1..Memory.Length(Memoire) loop
            Increment_PC(PC);
            IR := Memory.Get_Data(Memoire, PC);
            Interpret_Single_Instruction(IR, Registre, PC);
        end loop;
    end Interpret;
end interpreteur;