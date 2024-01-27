with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
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

procedure Assign_Value(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is
    Current : Register.Variable_Record;
    Variable : Register.Variable_Record;
begin
    Current := Register.Get_Variable(Registre,IR.Token1);
    if Register.Contains_Name(Registre, IR.Token2) then
        Variable := Register.Get_Variable(Registre,IR.Token2);
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, Variable.Value);
    else 
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, IR.Token2);
    end if;

end Assign_Value;

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
    Operator := IR.Token3;
    Current := Register.Get_Variable(Registre,IR.Token1);
    -- Check if variable or value
    if Register.Contains_Name(Registre, IR.Token2) then
        Left := Register.Get_Variable(Registre,IR.Token2);
        Left_Value :=  Integer'Value(To_String (Left.Value));
    else
        Left_Value :=  Integer'Value(To_String (IR.Token2));
    end if;

    if Register.Contains_Name(Registre, IR.Token4) then
        Right := Register.Get_Variable(Registre,IR.Token4); 
        Right_Value :=  Integer'Value(To_String (Right.Value));
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
    end if;
    Register.Add_Variable(Registre, IR.Token2, Variable_Type, S(""));
    
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
    Current := Register.Get_Variable(Registre,IR.Token2);
    Get_Line(Input_Value);
    Register.Edit_Variable(Registre, Current.Name, Current.T_Type, Input_Value);
end Read_Variable; 

procedure Write_Variable(IR : in Memory.T_Instructions; Registre : in Register.Register_Type) is
    Output : Register.Variable_Record;
begin
    if Register.Contains_Name(Registre, IR.Token2) then
        Output := Register.Get_Variable(Registre,IR.Token2);
        Put_Line(Output.Value);
    else 
        Put_Line(IR.Token2);
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