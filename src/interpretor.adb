with Ada.Text_IO; use Ada.Text_IO;
with Common_Types;

package body Interpretor is

    procedure Init(Path : in String; Interpreteur : out T_Interpretor) is
        Memoire :  Memory.T_Memory;
        Registre : Register.Register_Type;
        IR : Memory.T_Instructions;
    begin
        Register.Init(Registre);
        Memory.Init(Memoire);
        Interpreteur.Memoire := Memoire;
        Interpreteur.Registre := Registre;
        Interpreteur.PC := 1;

        Read_File_Content(Path, Interpreteur);
        Interpreteur.IR := Memory.Get_Data(Interpreteur.Memoire, Interpreteur.PC);
    end Init;



    procedure Read_File_Content(Path : in String; Interpreteur : out T_Interpretor) is
        Handle : Reader.File_Handle;
        File_Content_String : Common_Types.String_List;
    begin
        Reader.Open_File(Path => Path, Handle => Handle);
        File_Content_String := Reader.Get_Lines(Handle => Handle);
        Reader.Close_File(Handle);
        Lexer.Analyser_Lignes(File_Content_String, Interpreteur.Memoire);
    end Read_File_Content;

    function Get_PC(Interpreteur : in T_Interpretor) return Integer is
    begin
        return Interpreteur.PC;
    end Get_PC;

    function Get_IR(Interpreteur : in T_Interpretor) return Memory.T_Instructions is
    begin
        return Interpreteur.IR;
    end Get_IR;

    function Get_Registre(Interpreteur : in T_Interpretor) return Register.Register_Type is 
    begin
        return Interpreteur.Registre;
    end Get_Registre;

    function Get_Memory(Interpreteur : in T_Interpretor) return Memory.T_Memory is 
    begin
        return Interpreteur.Memoire;
    end Get_Memory;

    function Get_IR_Value(IR : in Memory.T_Instructions) return Unbounded_String is
    begin
        Return IR.Token1 & IR.Token2 & IR.Token3 & IR.Token4;
    end Get_IR_Value;

    procedure Increment_PC(Interpreteur : in out T_Interpretor) is
    begin
        Interpreteur.PC := Interpreteur.PC + 1;
    end Increment_PC;

    procedure Display_Infos(Interpreteur : in T_Interpretor) is
    begin
        Put_Line("");
        Put_Line("--------------------------------- Register Content ---------------------------------");
        Put("-- Registry index value :" & Integer'Image(1) & "  |  ");
        Register.Put(Register.Variable_List.Get_Data(Interpreteur.Registre, 1));
        Put_Line("");
        for Index in 2..Register.Length(Interpreteur.Registre) loop
            Put_Line("------------------------------------------------");
            Put("-- Registry index value :" & Integer'Image(Index) & "  |  ");
            Register.Put(Register.Variable_List.Get_Data(Interpreteur.Registre, Index));
            Put_Line("");
        end loop;
        Put_Line("----------------------------------------------------------------------------------");
        Put_Line("");
    end Display_Infos;

    procedure Display_Single_Info(Interpreteur : in T_Interpretor) is
    begin
        Put_Line("");
        Put("--------------------------------- PC value :");Put(Integer'Image(Interpreteur.PC));Put(" ---------------------------------");
        Put_Line("");
        Put("-- IR Tokens : ");
        Memory.Put(Interpreteur.IR);
        Put_Line("");
        Put_Line("----------------------------------------------------------------------------------");
    end Display_Single_Info;


    procedure Interpret_Single_Instruction(Interpreteur : in out T_Interpretor) is
    begin
        if Interpreteur.PC <= Memory.Length(Interpreteur.Memoire) then
            Evaluator.Evaluate_And_Execute(Interpreteur.IR,Interpreteur.Registre, Interpreteur.PC);
            Increment_PC(Interpreteur);
            Interpreteur.IR := Memory.Get_Data(Interpreteur.Memoire, Interpreteur.PC);
        end if;
    end Interpret_Single_Instruction;


    procedure Interpret_All(Interpreteur : in out T_Interpretor) is
    begin
        while (Interpreteur.PC <= Memory.Length(Interpreteur.Memoire) and not Evaluator.Is_End_Of_Program(Interpreteur.IR)) loop
            Interpret_Single_Instruction(Interpreteur);
        end loop;
        Display_Infos(Interpreteur);
    end Interpret_All;
end Interpretor;