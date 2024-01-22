package body Evaluator is


procedure Initialize(Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) is
begin
    Memory.Init(Memoire);
    Register.Init(Memoire);
end Initialize;

procedure Assign_Value(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) is
    Current : Register.Variable_Record;
    Variable : Register.Variable_Record;
begin
    Current := Register.Get_Variable(Registre,IR.Token1);
    if Register.Contains_Name(IR.Token2) then
        Variable := Register.Get_Variable(Registre,IR.Token2);
        Register.Edit_Variable(Current.Name, Current.T_Type, Variable.Value);
    else 
        Register.Edit_Variable(Current.Name, Current.T_Type, IR.Token2);
    end if;

end Assign_Value;

procedure Assign_With_Operation(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) is
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
    if Register.Contains_Name(IR.Token2) then
        Left := Register.Get_Variable(Registre,IR.Token2);
        Left_Value :=  Integer'Value(To_String (Left.Value));
    else
        Left_Value :=  Integer'Value(To_String (IR.Token2));
    end if;

    if Register.Contains_Name(IR.Token4) then
        Right := Register.Get_Variable(Registre,IR.Token4); 
        Right_Value :=  Integer'Value(To_String (Right.Value));
    else
        Right_Value :=  Integer'Value(To_String (IR.Token4));

    end if;

    if Operator = "AND" then
        Result := Left_Value and Right_Value;
    elsif Operator = "OR" then
        Result := Left_Value or Right_Value;
    elsif Operator = "=" then
        Result := Left_Value = Right_Value;
    elsif Operator = ">" then
        Result := Left_Value > Right_Value;
    elsif Operator = "<" then
        Result := Left_Value < Right_Value;
    elsif Operator = "+" then
        Result := Left_Value + Right_Value;
    elsif Operator = "-" then
        Result := Left_Value - Right_Value;
    elsif Operator = "*" then
        Result := Left_Value * Right_Value;
    elsif Operator = "/" then
        Result := Left_Value / Right_Value;
    end if;
    Register.Edit_Variable(Current.Name, Current.T_Type, To_Unbounded_String(Result));
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
    Register.Add_Variable(Registre, IR.Token2, Variable_Type,"");
    
end Init_Variable;

procedure Init_Label(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is 
begin
    Register.Add_Variable(Registre, IR.Token2, Register.T_Label, IR.Token3);
end Init_Label;

procedure Conditional_Branch(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type; PC : in out Integer) is
    Current : Register.Variable_Record;
    Result : Register.Variable_Record;
begin
    Result := Register.Get_Variable(Registre,IR.Token2);
    Current := Register.Get_Variable(Registre,IR.Token4);
    if Integer'Value(To_String(Result.Value)) >= 0 then
        PC := Integer'Value(To_String(Current.Value));
    end if;
end Conditional_Branch;

procedure Unconditional_Branch(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type; PC : in out Integer) is
    Label : Register.Variable_Record;
begin
    if Register.Contains_Name(IR.Token2) then
        Label := Register.Get_Variable(Registre,IR.Token2);
        PC := Integer'Value(To_String(Label.Value));
    else 
        PC := Integer'Value(To_String(IR.Token2));
    end if;
end Unconditional_Branch;

procedure Read_Variable(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) is
    Input_Value : Unbounded_String;
    Current : Register.Variable_Record;
begin
    Current := Register.Get_Variable(Registre,IR.Token2);
    Get_Line(Input_Value);
    Register.Edit_Variable(Current.Name, Current.T_Type, To_Unbounded_String(Input_Value));
end Read_Variable; 

procedure Write_Variable(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) is
    Output : Register.Variable_Record;
begin
    if Register.Contains_Name(IR.Token2) then
        Output := Register.Get_Variable(Registre,IR.Token2);
        Put_Line(Output.Value);
    else 
        Put_Line(IR.Token2);
    end if;
end Write_Variable; 



end Evaluator;