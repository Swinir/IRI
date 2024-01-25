with Ada.Text_IO; use Ada.Text_IO;
with Common_Types;

package body Interpretor is

    procedure Init(Path : in String; Intepreteur : out T_Interpretor) is
        Memoire :  Memory.T_Memory;
        Registre : Register.Register_Type;
        IR : Memory.T_Instructions;
    begin
        Register.Init(Registre);
        Memory.Init(Memoire);
        Intepreteur.Memoire := Memoire;
        Intepreteur.Registre := Registre;
        Intepreteur.PC := 1;

        Read_File_Content(Path, Intepreteur);
    end Init;

    procedure Read_File_Content(Path : in String; Intepreteur : out T_Interpretor) is
        Handle : Reader.File_Handle;
        File_Content_String : Common_Types.String_List;
    begin
        Reader.Open_File(Path => Path, Handle => Handle);
        File_Content_String := Reader.Get_Lines(Handle => Handle);
        Lexer.Analyser_Lignes(File_Content_String, Intepreteur.Memoire);
    end Read_File_Content;

    function Get_IR_Value(IR : in Memory.T_Instructions) return Unbounded_String is
    begin
        Return IR.Token1 & IR.Token2 & IR.Token3 & IR.Token4;
    end Get_IR_Value;

    procedure Increment_PC(Intepreteur : in out T_Interpretor) is
    begin
        Intepreteur.PC := Intepreteur.PC + 1;
    end Increment_PC;

    procedure Display_Infos(Intepreteur : in T_Interpretor) is
    begin
        Put_Line("----------- Memory Content -----------");
        for Index in 1..Memory.Length(Intepreteur.Memoire) loop
            Memory.Put(Memory.Get_Data(Intepreteur.Memoire, Index));
        end loop;
        Put_Line("----------------------------------------");
        Put("----------- PC Value : ");Put(Integer'Image(Intepreteur.PC));Put(" -------------");
        Put_Line("----------- Register Content -----------");
        for Index in 1..Register.Length(Intepreteur.Registre) loop
            Put_Line("Register value position :" & Integer'Image(Index));
           -- Register.Put(Memory.Get_Data(Intepreteur.Registre, Index));
        end loop;
        Put_Line("----------------------------------------");
    end Display_Infos;

    procedure Display_Single_Info(Intepreteur : in T_Interpretor) is
    begin
        Put("----------- PC Value : ");Put(Integer'Image(Intepreteur.PC));Put(" -------------");
        Put_Line("----------- IR -----------");
        Memory.Put(Intepreteur.IR);
        Put_Line("--------------------------");
    end Display_Single_Info;


    procedure Interpret_Single_Instruction(Intepreteur : in out T_Interpretor) is
    begin
        if Intepreteur.PC <= Memory.Length(Intepreteur.Memoire) then
            Evaluator.Evaluate_And_Execute(Intepreteur.IR,Intepreteur.Memoire, Intepreteur.Registre, Intepreteur.PC);
            Increment_PC(Intepreteur);
            Intepreteur.IR := Memory.Get_Data(Intepreteur.Memoire, Intepreteur.PC);
        end if;
    end Interpret_Single_Instruction;


    procedure Interpret_All(Intepreteur : in out T_Interpretor) is
    begin
        Intepreteur.IR := Memory.Get_Data(Intepreteur.Memoire, Intepreteur.PC);
        while (Intepreteur.PC <= Memory.Length(Intepreteur.Memoire) and not Evaluator.Is_End_Of_Program(Intepreteur.IR)) loop
            Interpret_Single_Instruction(Intepreteur);
        end loop;
    end Interpret_All;
end Interpretor;