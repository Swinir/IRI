with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

package body Lexer is

    type Words_Array_T is array (1..10) of Unbounded_String;

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


    function Extraire_Mots(Ligne : in String) return T_Words_List is
        Words_Array : Words_Array_T;
        Words : T_Words_List;
    begin
        Words_Array := Split(Source => Ligne, Separator => ' ');
        for Index in Words_Array'Range loop
            Common_Types.Append(Words, Words_Array(Index));
        end loop;
        return Words;
    end Extraire_Mots;


    procedure Process_Goto (Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Word := Common_Types.Get_Data(Mots, 0);
        Instructions.Token1 := Word;

        Word := Common_Types.Get_Data(Mots, 1);
        Instructions.Token2 := Word;
    end Process_Goto;

    procedure Process_If(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Word := Common_Types.Get_Data(Mots, 0);
        Instructions.Token1 := Word;

        Word := Common_Types.Get_Data(Mots, 1);
        Instructions.Token2 := Word;

        Word := Common_Types.Get_Data(Mots, 2);
        Instructions.Token3 := Word;

        Word := Common_Types.Get_Data(Mots, 3);
        Instructions.Token4 := Word;
    end Process_If;

    procedure Process_Label(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Instructions.Token1 := To_Unbounded_String("LABEL");

        Word := Common_Types.Get_Data(Mots, 0);
        Instructions.Token2 := Word;

        Word := To_Unbounded_String(""); -- TODO : Call the procedure to translate label name into line number
        Instructions.Token3 := Word;
    end Process_Label;

    procedure Process_Value_Variable(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Word := Common_Types.Get_Data(Mots, 0);
        Instructions.Token1 := Word;

        Word := Common_Types.Get_Data(Mots, 1);
        Instructions.Token2 := Word;

        if Common_Types.Length(Mots) = 4 then
            Word := Common_Types.Get_Data(Mots, 2);
            Instructions.Token3 := Word;

            Word := Common_Types.Get_Data(Mots, 3);
            Instructions.Token4 := Word;
        end if;
    end Process_Value_Variable;

    procedure Process_Read(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Instructions.Token1 := To_Unbounded_String("READ");

        Word := Common_Types.Get_Data(Mots, 1);
        Instructions.Token2 := Word;
    end Process_Read;

    procedure Process_Write(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String;
    begin
        Instructions.Token1 := To_Unbounded_String("WRITE");

        Word := Common_Types.Get_Data(Mots, 1);
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

end lexer;