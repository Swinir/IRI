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
        while Index <= Source'Last and Count <= 50 loop
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
        if Length(Word) > 0 and Count <= 50 then
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
        Debut_Instruction : Memory.T_Instructions;
    begin
        Debut_Instruction.Token1 := To_Unbounded_String("BEGIN");
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
        elsif Ada.Strings.Fixed.Index(To_String(Word), "Ecrire") > 0 then
            Process_Write(Mots, Instructions);
        elsif Ada.Strings.Fixed.Index(To_String(Word), "Lire") > 0 then
            Process_Read(Mots, Instructions);
        elsif Word = To_Unbounded_String("Programme") then
            Process_Function(Mots, Instructions);
        elsif Word = To_Unbounded_String("Debut") then
            Process_Begin_Variable(Mots, Instructions);
        elsif Word = To_Unbounded_String("Fin") then
            Process_End_Variable(Mots, Instructions);
        elsif Common_Types.Length(Mots) >= 2 and Memory.Get_Position(Memoire, Debut_Instruction) = - 1 then
            Process_Var_Init(Mots, Memoire, Nb_Declarations);
        elsif Common_Types.Length(Mots) >= 2 and Memory.Get_Position(Memoire, Debut_Instruction) /= - 1 then
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

    function Reverse_String(Str : in Unbounded_String) return Unbounded_String is
        Result : Unbounded_String := To_Unbounded_String("");
    begin
        for I in reverse 1 .. Length(Str) loop
            Result := Result & Element(Str, I);
        end loop;
        return Result;
    end Reverse_String;

    procedure Process_Read(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
        Open_Paren_Pos : Integer;
        Close_Paren_Pos : Integer;
    begin
        Instructions.Token1 := To_Unbounded_String("READ");

        Word := Common_Types.Get_Data(Mots, 1);
        Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Word), "(");
        Close_Paren_Pos := To_String(Word)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Word)), ")");

        Instructions.Token2 := To_Unbounded_String(Slice(Word, Open_Paren_Pos + 1, Close_Paren_Pos - 1));

    end Process_Read;

    procedure Process_Write(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
        Open_Paren_Pos : Integer;
        Close_Paren_Pos : Integer;
    begin
        Instructions.Token1 := To_Unbounded_String("WRITE");

        Word := Common_Types.Get_Data(Mots, 1);
        Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Word), "(");
        Close_Paren_Pos := To_String(Word)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Word)), ")");

        Instructions.Token2 := To_Unbounded_String(Slice(Word, Open_Paren_Pos + 1, Close_Paren_Pos));
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
        Open_Paren_Pos : Integer;
        Close_Paren_Pos : Integer;
        Value : Integer;
    begin
        Nb_Words := Common_Types.Length(Mots) - 1;
        if Common_Types.Get_Data(Mots, 3) = To_Unbounded_String("Tableau") then
            Temp_Instruction.Token1 := To_Unbounded_String("INIT");
            Temp_Instruction.Token2 := Common_Types.Get_Data(Mots, 1);
            Word := Common_Types.Get_Data(Mots, 4);
            Open_Paren_Pos := Index(Word, "(");
            Close_Paren_Pos := Index(Word, ")");
            Word := To_Unbounded_String(Slice(Word, Open_Paren_Pos + 1, Close_Paren_Pos - 1));
            Value := Integer'Value(To_String(Word));
            Temp_Instruction.Token3 := To_Unbounded_String("TAB:") & Trim(Integer'Image(Value), Ada.Strings.Left);
            Temp_Instruction.Token4 := Common_Types.Get_Data(Mots, 6);
            Enregistrer_Instructions(Temp_Instruction, Memoire);
        else
            for Index in 1..Nb_Words loop
                Word := Common_Types.Get_Data(Mots, Index);
                Put_Line(Word);
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
        end if;
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
        Index_without_Blanks : Integer := 1;
        Instructions : Memory.T_Instructions;
        Nb_Declarations : Integer := 0;
        Nb_Labels : Integer := 0;
    begin
        while Index <= Common_Types.Length(Lignes) loop
            if Ada.Strings.Fixed.Index(To_String(Common_Types.Get_Data(Lignes, Index)), "--") = 0 AND Common_Types.Get_Data(Lignes, Index) /= To_Unbounded_String("") then
                Put(Common_Types.Get_Data(Lignes, Index));
                Mots := Extraire_Mots(Common_Types.Get_Data(Lignes, Index));
                Process_Keywords(Mots, Index_without_Blanks, Instructions, Memoire, Nb_Declarations, Nb_Labels);
                if Instructions.Token1 /= To_Unbounded_String("LABEL") and Instructions.Token1 /= To_Unbounded_String("") then
                    Enregistrer_Instructions(Instructions, Memoire);
                end if;
                Common_Types.Clear(Mots);
                Clear_Instructions(Instructions);
                Index_without_Blanks := Index_without_Blanks + 1;
            end if;
            Index := Index + 1;
        end loop;
    end Analyser_Lignes;

end lexer;