with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;


package body Lexer is

    function Split(Source : in String; Separator : in Character) return Words_Array_T is
        Words : Words_Array_T;
        Word : Unbounded_String := To_Unbounded_String("");
        Index : Positive := Source'First;
        Count : Positive := 1;
    begin
        while Index <= Source'Last and Count <= 10 loop
            if Source(Index) /= Separator then
                Append(Word, Source(Index));
            else
                if Length(Word) > 0 then
                    Words(Count) := Word;
                    Word := To_Unbounded_String("");
                    Count := Count + 1;
                end if;
            end if;
            Index := Index + 1;
        end loop;
        if Length(Word) > 0 and Count <= 10 then
            Words(Count) := Word;
        end if;
        return Words;
    end Split;


    function Extraire_Mots(Ligne : in Unbounded_String) return T_Words_List is
        Words_Array : Words_Array_T;
        Words : T_Words_List;
        Index : Integer := 1;
    begin
        Common_Types.Init(Words);
        Words_Array := Split(Source => To_String(Ligne), Separator => ' ');
        for Index in Words_Array'Range loop
            if Words_Array(Index) /= To_Unbounded_String("") then
                Common_Types.Append(Words, Words_Array(Index));
            end if;
        end loop;
        return Words;
    end Extraire_Mots;


    procedure Process_Keywords(Mots : in out T_Words_List; Index : in out Integer; Instructions : out Memory.T_Instructions; Memoire : in out Memory.T_Memory; Nb_Declarations : in out Integer; Nb_Labels : in out Integer) is
        Word : Unbounded_String;
    begin
        Word := Common_Types.Get_Data(Mots, 1);
        if Element(Word, 1) = 'L' then
            Process_Label(Mots, Index, Memoire, Nb_Labels, Nb_Declarations);
            Common_Types.Pop(Mots, 1);
            Word := Common_Types.Get_Data(Mots, 1);
        end if;
        if Word = To_Unbounded_String("IF") then
            Process_If(Mots, Instructions);
        elsif Word = To_Unbounded_String("GOTO") then
            Process_Goto(Mots, Instructions);
        elsif Word = To_Unbounded_String("NULL") then
            Process_Null(Mots, Instructions);
        elsif Word = To_Unbounded_String("Ecrire") then
            Process_Write(Mots, Instructions);
        elsif Word = To_Unbounded_String("Lire") then
            Process_Read(Mots, Instructions);
        elsif Word = To_Unbounded_String("Programme") then
            Process_Function(Mots, Instructions);
        elsif Word = To_Unbounded_String("Debut") then
            Process_Begin_Variable(Mots, Instructions);
        elsif Word = To_Unbounded_String("Fin") then
            Process_End_Variable(Mots, Instructions);
        elsif Common_Types.Get_Position(Mots, To_Unbounded_String(":")) /= -1 then
            Process_Var_Init(Mots, Memoire, Nb_Declarations);
        elsif Common_Types.Length(Mots) >= 2 then
            if Common_Types.Get_Data(Mots, 2) = To_Unbounded_String("<-") then
                Process_Value_Variable(Mots, Instructions);
            end if;
        else
            Ada.Text_IO.Put_Line("Uhoh program crashed cause keyword is not reconnized. Keyword in question : ");
            Ada.Strings.Unbounded.Text_IO.Put(Word);
        end if;
    end Process_Keywords;
    

    procedure Enregistrer_Instructions(Instructions : in Memory.T_Instructions; Memoire : in out Memory.T_Memory) is
    begin
        Memory.Append(Memoire, Instructions);
    end Enregistrer_Instructions;


    procedure Process_Function(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String("PROGRAM");
    end Process_Function;


    procedure Process_Goto (Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Instructions.Token1 := To_Unbounded_String("GOTO");

        Word := Common_Types.Get_Data(Mots, 2);
        Instructions.Token2 := Word;
    end Process_Goto;

    procedure Process_If(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Word := Common_Types.Get_Data(Mots, 1);
        Instructions.Token1 := Word;

        Word := Common_Types.Get_Data(Mots, 2);
        Instructions.Token2 := Word;

        Word := Common_Types.Get_Data(Mots, 3);
        Instructions.Token3 := Word;

        Word := Common_Types.Get_Data(Mots, 4);
        Instructions.Token4 := Word;
    end Process_If;

    procedure Process_Label(Mots : in T_Words_List; Index : in Integer; Memoire : in out Memory.T_Memory; Nb_Labels : in out Integer; Nb_Declarations : in Integer) is
        Word : Unbounded_String;
        Old_Instruction : Memory.T_Instructions;
        New_Instruction : Memory.T_Instructions;
        Instruction : Memory.T_Instructions;
        Line_nb : Integer;
        Old_line_nb : Integer;
    begin
        for Index_MEM in 1..Memory.Length(Memoire) loop
            Old_Instruction := Memory.Get_Data(Memoire, Index_MEM);
            if Old_Instruction.Token1 = To_Unbounded_String("LABEL") then
                New_Instruction.Token1 := Old_Instruction.Token1;
                New_Instruction.Token2 := Old_Instruction.Token2;
                Old_line_nb := Integer'Value(To_String (Old_Instruction.Token3)) + 1;
                New_Instruction.Token3 := To_Unbounded_String(Trim(Integer'Image(Old_line_nb), Ada.Strings.Left));
                Memory.Edit_Data(Memoire, Index_MEM, New_Instruction);
            end if;
        end loop;
        Nb_Labels := Nb_Labels + 1;
        Instruction.Token1 := To_Unbounded_String("LABEL");
        Word := Common_Types.Get_Data(Mots, 1);
        Instruction.Token2 := Word;

        Line_nb := Index + Nb_Labels + (Nb_Declarations - 1);
        Instruction.Token3 := To_Unbounded_String(Trim(Integer'Image(Line_nb), Ada.Strings.Left));

        Memory.Insert_Beginning(Memoire, Instruction);

    end Process_Label;

    procedure Process_Value_Variable(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Word := Common_Types.Get_Data(Mots, 1);
        Instructions.Token1 := Word;

        Word := Common_Types.Get_Data(Mots, 3);
        Instructions.Token2 := Word;

        if Common_Types.Length(Mots) = 5 then

            Word := Common_Types.Get_Data(Mots, 4);
            Instructions.Token3 := Word;

            Word := Common_Types.Get_Data(Mots, 5);
            Instructions.Token4 := Word;
        end if;
    end Process_Value_Variable;

    procedure Process_Read(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Instructions.Token1 := To_Unbounded_String("READ");

        Word := Common_Types.Get_Data(Mots, 2);
        Instructions.Token2 := Word;
    end Process_Read;

    procedure Process_Write(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Instructions.Token1 := To_Unbounded_String("WRITE");

        Word := Common_Types.Get_Data(Mots, 2);
        Instructions.Token2 := Word;
    end Process_Write;

    procedure Process_Begin_Variable(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String("BEGIN");
    end Process_Begin_Variable;


    procedure Process_End_Variable(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String("END");
    end Process_End_Variable;

    procedure Process_Null(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String("NULL");
    end Process_Null;

    procedure Process_Var_Init(Mots : in T_Words_List; Memoire : in out Memory.T_Memory; Nb_Declarations : in out Integer) is
        Instruction_Array : Instruction_Array_T;
        Temp_Instruction : Memory.T_Instructions;
        Word : Unbounded_String := To_Unbounded_String("");
        Nb_Words : Integer;
    begin
        Nb_Words := Common_Types.Length(Mots) - 1;
        for Index in 1..Nb_Words loop
            Word := Common_Types.Get_Data(Mots, Index);

            if Word /= ":" then
                Nb_Declarations := Nb_Declarations + 1;
                Temp_Instruction.Token1 := To_Unbounded_String("INIT"); 
                Word := Common_Types.Get_Data(Mots, Index);
                if element(Word, length (Word)) = ',' then
                    Word := To_Unbounded_String(Ada.Strings.Unbounded.Slice(Word, 1, Ada.Strings.Unbounded.Length(Word) - 1));
                end if;
                Temp_Instruction.Token2 := Word;
                -- Type will be set later

                Instruction_Array(Index) := Temp_Instruction;
            else
                Word := Common_Types.Get_Data(Mots, Index + 1);
                for Index in Instruction_Array'Range loop
                    if Instruction_Array(Index).Token1 /= To_Unbounded_String("") then
                        Instruction_Array(Index).Token3 := Word;
                    end if;
                end loop;
            end if;
        end loop;
        for Index in Instruction_Array'Range loop
            if Instruction_Array(Index).Token1 /= To_Unbounded_String("") then
                Enregistrer_Instructions(Instruction_Array(Index), Memoire);
            end if;
        end loop;
    end Process_Var_Init;


    procedure Clear_Instructions(Instructions : in out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String("");
        Instructions.Token2 := To_Unbounded_String("");
        Instructions.Token3 := To_Unbounded_String("");
        Instructions.Token4 := To_Unbounded_String("");
    end Clear_Instructions;

    procedure Analyser_Lignes(Lignes : in Common_Types.String_List; Memoire : in out Memory.T_Memory) is
        Mots : T_Words_List;
        Index : Integer := 1;
        Instructions : Memory.T_Instructions;
        Nb_Declarations : Integer := 0;
        Nb_Labels : Integer := 0;
    begin
        while Index <= Common_Types.Length(Lignes) loop
            Mots := Extraire_Mots(Common_Types.Get_Data(Lignes, Index));
            Process_Keywords(Mots, Index, Instructions, Memoire, Nb_Declarations, Nb_Labels);
            if Instructions.Token1 /= To_Unbounded_String("LABEL") and Instructions.Token1 /= To_Unbounded_String("") then
                Enregistrer_Instructions(Instructions, Memoire);
            end if;
            Common_Types.Clear(Mots);
            Clear_Instructions(Instructions);
            Index := Index + 1;
        end loop;
            --          for I in 1..Memory.Length(Memoire) loop
            --          Ada.Integer_Text_IO.Put(I);
            --      Memory.Put(Memory.Get_Data(Memoire, I));
            --  end loop;
    end Analyser_Lignes;

end lexer;