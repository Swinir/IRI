with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;

package body Evaluator is

procedure Evaluate_And_Execute(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type; PC : in out Integer) is
begin
    if IR.Token1 = S("INIT") then
        Init_Variable(IR, Registre);
    elsif IR.Token1 = S("GOTO") then
        Unconditional_Branch(IR, Registre, PC);
    elsif IR.Token1 = S("IF") then
        Conditional_Branch(IR, Registre, PC);
    elsif IR.Token1 = S("LABEL") then
        Init_Label(IR, Registre);
    elsif IR.Token1 = S("READ") then
        Read_Variable(IR, Registre);
    elsif IR.Token1 = S("WRITE") then
        Write_Variable(IR, Registre);
    elsif IR.Token1 = S("NULL") or IR.Token1 = S("PROGRAM") or IR.Token1 = S("BEGIN") or IR.Token1 = S("END") then
        Null_Operation;
    else
        if IR.Token3 /= S("") and IR.Token3 /= S(" ") and IR.Token4 /= S("") and IR.Token4 /= S(" ") then
            Assign_With_Operation(IR, Registre);
        else
            Assign_Value(IR, Registre);
        end if;
    end if;
end;

function Reverse_String(Str : in Unbounded_String) return Unbounded_String is
    Result : Unbounded_String := To_Unbounded_String("");
begin
    for I in reverse 1 .. Length(Str) loop
        Result := Result & Element(Str, I);
    end loop;
    return Result;
end Reverse_String;

function Is_Array_Type(IR : in Memory.T_Instructions) return Boolean is
    Value : Unbounded_String;
    Close_Paren_Pos : Integer;

begin
    Close_Paren_Pos := To_String(IR.Token4)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(IR.Token4)), ":");

    Value := S(Slice(IR.Token4, 1, Close_Paren_Pos));
    return Value = S("TAB");
end Is_Array_Type;

procedure Init_Array(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type; Variable_Type  : in Register.T_Types) is
    Value : Unbounded_String;
    Array_Size : Character;
    Name : Unbounded_String;
begin
    Array_Size := Element(IR.Token4, Length(IR.Token4));
    for Num in 1..(Character'Pos(Array_Size)- Character'Pos('0')) loop
        Name := IR.Token2;
        Append(Name,S("("));
        Append(Name, To_Unbounded_String(Trim(Integer'Image(Num), Ada.Strings.Left)));
        Append(Name,S(")"));
        Register.Add_Variable(Registre, Name, Variable_Type, S(""));
    end loop;
end Init_Array;

function Get_Array_Index(Token : In Unbounded_String; Registre : in Register.Register_Type) return Unbounded_String is
    Value : Register.Variable_Record;
    Open_Paren_Pos : Integer;
    Close_Paren_Pos : Integer;
    Array_Index : Unbounded_String;
    Name : Unbounded_String;
begin
    Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Token), "(");
    Close_Paren_Pos := To_String(Token)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Token)), ")");
    
    Name := S(Slice(Token, 1, Open_Paren_Pos));

    Array_Index := S(Slice(Token, Open_Paren_Pos + 1, Close_Paren_Pos));
    If Array_Index >= S("1") and Array_Index <= S("9") then
        return Token;
    else
        Value := Register.Get_Variable(Registre,Array_Index);
        return Name & Value.Value & S(")");
    end if;
    
end Get_Array_Index;

function Is_Variable_Array(Token : in Unbounded_String) return Boolean is
    Open_Paren_Pos : Integer;
    Close_Paren_Pos : Integer;
begin
    Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Token), "(");
    Close_Paren_Pos := To_String(Token)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Token)), ")");

    return Open_Paren_Pos /= 0 and Close_Paren_Pos /= 0;
end Is_Variable_Array;

procedure Assign_Value(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is
    Current : Register.Variable_Record;
    Variable : Register.Variable_Record;
    Name : Unbounded_String;
begin
    if Is_Variable_Array(IR.Token1) then
        Name := Get_Array_Index(IR.Token1, Registre);
        Current := Register.Get_Variable(Registre, Name);
    else
        Current := Register.Get_Variable(Registre, IR.Token1);
    end if; 

    if Register.Contains_Name(Registre, IR.Token2) then
        Variable := Register.Get_Variable(Registre,IR.Token2);
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, Variable.Value);
    elsif Is_Variable_Array(IR.Token2) then
        Variable := Register.Get_Variable(Registre,Get_Array_Index(IR.Token2, Registre));
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, Variable.Value);
    elsif Register.T_Types'Pos(Current.T_Type) = Register.T_Types'Pos(Register.T_Caractere) then
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, To_Unbounded_String(Trim(Integer'Image(Character'Pos((Element(IR.Token2, Length(IR.Token2)-1)))), Ada.Strings.Left)));
    else
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, IR.Token2);

    end if;
end Assign_Value;

-- Converti une chaine de caractère en valeur entière.
function String_Hash(Str : String) return Integer is
    Sum : Integer := 0;
begin
    for I in Str'Range loop
        Sum := Sum + Character'Pos(Str(I));
    end loop;
    return Sum;
end String_Hash;

function Is_String_Type(Token : Unbounded_String) return Boolean is
    Value : Unbounded_String;
    Close_Paren_Pos : Integer;
    Open_Paren_Pos : Integer;

begin
    Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Token), """");
    Close_Paren_Pos := To_String(Token)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Token)), """");
    
    return Open_Paren_Pos /= 0;
end Is_String_Type;

procedure Assign_With_Operation(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is
    Operator : Unbounded_String;
    Current : Register.Variable_Record;
    Left : Register.Variable_Record;
    Right : Register.Variable_Record;
    Left_Value : Integer;
    Right_Value : Integer;
    Result : Integer;

begin 
    --if Left.T_Type and Right.T_Type =
    if Is_Variable_Array(IR.Token1) then
        
        Current.Name := Get_Array_Index(IR.Token1, Registre);
    end if;
    Current := Register.Get_Variable(Registre,IR.Token1);
    Operator := IR.Token3;

    -- Check if variable or value
    if Register.Contains_Name(Registre, IR.Token2) then
        Left := Register.Get_Variable(Registre,IR.Token2);
        if Register.T_Types'Pos(Left.T_Type) = Register.T_Types'Pos(Register.T_Chaine) then
            Left_Value := String_Hash(To_String(Left.Value));
        else
            Left_Value :=  Integer'Value(To_String (Left.Value));
        end if;
     -- Check if it is an array
    elsif Is_Variable_Array(IR.Token2) then
        Left := Register.Get_Variable(Registre,Get_Array_Index(IR.Token2, Registre));
        Left_Value := Integer'Value(To_String (Left.Value));
    elsif Register.T_Types'Pos(Current.T_Type) = Register.T_Types'Pos(Register.T_Caractere) then
        Left_Value := Character'Pos(Element(IR.Token2, Length(IR.Token2)-1));
    elsif Is_String_Type(IR.Token2) then
        Left_Value := String_Hash(To_String(IR.Token2));
    else
        Left_Value :=  Integer'Value(To_String (IR.Token2));
    end if;

    if Register.Contains_Name(Registre, IR.Token4) then
        Right := Register.Get_Variable(Registre,IR.Token4); 
        if Register.T_Types'Pos(Left.T_Type) = Register.T_Types'Pos(Register.T_Chaine) then
            Right_Value := String_Hash(To_String(Right.Value));
        else
            Right_Value :=  Integer'Value(To_String (Right.Value));
        end if;
    elsif Is_Variable_Array(IR.Token2) then
        Right := Register.Get_Variable(Registre,Get_Array_Index(IR.Token4, Registre)); 
        Right_Value :=  Integer'Value(To_String (Right.Value));
    elsif Register.T_Types'Pos(Current.T_Type) = Register.T_Types'Pos(Register.T_Caractere) then
        Right_Value := Character'Pos(Element(IR.Token4, Length(IR.Token4)-1));
    elsif Is_String_Type(IR.Token4) then
        Right_Value := String_Hash(To_String(IR.Token4));
    else
        Right_Value :=  Integer'Value(To_String (IR.Token4));
    end if;

    if Operator = S("AND") then
        Result := (Integer(Unsigned_Integer(Left_Value) and Unsigned_Integer(Right_Value)));
    elsif Operator = S("OR") then
        Result := Integer(Unsigned_Integer(Left_Value) or Unsigned_Integer(Right_Value));
    elsif Operator = S("=") then
        Result := (if Left_Value = Right_Value then 1 else 0);
    elsif Operator = S(">") then
        Result := (if Left_Value > Right_Value then 1 else 0);
    elsif Operator = S("<") then
        Result := (if Left_Value < Right_Value then 1 else 0);
    elsif Operator = S("+") then
        Result := Left_Value + Right_Value;
    elsif Operator = S("-") then
        Result := Left_Value - Right_Value;
    elsif Operator = S("*") then
        Result := Left_Value * Right_Value;
    elsif Operator = S("/") then
        Result := Left_Value / Right_Value;
    end if;
    Register.Edit_Variable(Registre, Current.Name, Current.T_Type, To_Unbounded_String(Trim(Integer'Image(Result), Ada.Strings.Left)));
end Assign_With_Operation;


procedure Init_Variable(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is
    Variable_Type_Name : Unbounded_String;
    Variable_Type : Register.T_Types;
begin
    Variable_Type_Name := IR.Token3;
    if Variable_Type_Name = "Entier" then
        Variable_Type := Register.T_Entier;
    elsif Variable_Type_Name = "Booleen" then
        Variable_Type := Register.T_Booleen;
    elsif Variable_Type_Name = "Caractere" then
        Variable_Type := Register.T_Caractere;
    elsif Variable_Type_Name = "Chaine" then
        Variable_Type := Register.T_Chaine;
    end if;
    if Is_Array_Type(IR) then
        Init_Array(IR,Registre,Variable_Type);
    else
        Register.Add_Variable(Registre, IR.Token2, Variable_Type, S(""));
    end if;
    
end Init_Variable;

procedure Init_Label(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is 
begin
    Register.Add_Variable(Registre, IR.Token2, Register.T_Label, IR.Token3);
end Init_Label;

procedure Conditional_Branch(IR : in Memory.T_Instructions; Registre : in Register.Register_Type; PC : in out Integer) is
    Current : Register.Variable_Record;
    Result : Register.Variable_Record;
begin
    Result := Register.Get_Variable(Registre,IR.Token2);
    Current := Register.Get_Variable(Registre,IR.Token4);
    if Integer'Value(To_String(Result.Value)) > 0 then
        PC := Integer'Value(To_String(Current.Value)) - 1;
    end if;
end Conditional_Branch;

procedure Unconditional_Branch(IR : in Memory.T_Instructions; Registre : in Register.Register_Type; PC : in out Integer) is
    Label : Register.Variable_Record;
begin
    if Register.Contains_Name(Registre, IR.Token2) then
        Label := Register.Get_Variable(Registre,IR.Token2);
        PC := Integer'Value(To_String(Label.Value)) - 1;
    else 
        PC := Integer'Value(To_String(IR.Token2)) - 1;
    end if;
end Unconditional_Branch;

procedure Read_Variable(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is
    Input_Value : Unbounded_String;
    Current : Register.Variable_Record;
begin
    if Is_Variable_Array(IR.Token2) then
        Current := Register.Get_Variable(Registre, Get_Array_Index(IR.Token2, Registre));
    else 
        Current := Register.Get_Variable(Registre, IR.Token2);
    end if;
    Get_Line(Input_Value);
    Register.Edit_Variable(Registre, Current.Name, Current.T_Type, Input_Value);
end Read_Variable; 

procedure Write_Variable(IR : in Memory.T_Instructions; Registre : in Register.Register_Type) is
    Output : Register.Variable_Record;
begin
    if Register.Contains_Name(Registre, IR.Token2) then
        Output := Register.Get_Variable(Registre,IR.Token2);
        if Register.T_Types'Pos(Output.T_Type) = Register.T_Types'Pos(Register.T_Caractere) then
            Ada.Text_IO.Put_Line(Character'Val(Integer'Value(To_String(Output.Value)))'Image);
        else
            Ada.Text_IO.Put_Line(To_String(Output.Value));
        end if;
    elsif Is_Variable_Array(IR.Token2) then
        Output := Register.Get_Variable(Registre, Get_Array_Index(IR.Token2, Registre)); 
        Ada.Text_IO.Put_Line(To_String(Output.Value));
    else 
        Ada.Text_IO.Put_Line(To_String(IR.Token2));
    end if;
end Write_Variable; 

procedure Null_Operation is
begin
    return;
end Null_Operation;

function Is_End_Of_Program(IR : in Memory.T_Instructions) return Boolean is
begin
    if IR.Token1 = S("END") then
        return True;
    end if;
    return False;
end Is_End_Of_Program;


function Is_End_Of_Programm(IR : in Memory.T_Instructions) return Boolean is
begin
    if IR.Token1 = S("END") then
        return True;
    else
        return False;
    end if;
end Is_End_Of_Programm;

end Evaluator;