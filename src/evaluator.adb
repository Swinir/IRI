with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;

package body Evaluator is

-- Procédure pour évaluer et exécuter une instruction
procedure Evaluate_And_Execute(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type; PC : in out Integer) is
begin
    -- Selon le premier token de l'instruction, on appelle la procédure correspondante
    if IR.Token1 = S("INIT") then
        Init_Variable(IR, Registre); -- Initialisation d'une variable
    elsif IR.Token1 = S("GOTO") then
        Unconditional_Branch(IR, Registre, PC); -- Branchement inconditionnel
    elsif IR.Token1 = S("IF") then
        Conditional_Branch(IR, Registre, PC); -- Branchement conditionnel
    elsif IR.Token1 = S("LABEL") then
        Init_Label(IR, Registre); -- Initialisation d'un label
    elsif IR.Token1 = S("READ") then
        Read_Variable(IR, Registre); -- Lecture d'une variable
    elsif IR.Token1 = S("WRITE") then
        Write_Variable(IR, Registre); -- Écriture d'une variable
    elsif IR.Token1 = S("NULL") or IR.Token1 = S("PROGRAM") or IR.Token1 = S("BEGIN") or IR.Token1 = S("END") then
        Null_Operation; -- Opération nulle
    else
        -- Si les tokens 3 et 4 ne sont pas vides, on effectue une opération d'assignation avec opération
        if IR.Token3 /= S("") and IR.Token3 /= S(" ") and IR.Token4 /= S("") and IR.Token4 /= S(" ") then
            Assign_With_Operation(IR, Registre);
        else
            Assign_Value(IR, Registre); -- Sinon, on effectue une simple assignation de valeur
        end if;
    end if;
end;

-- Fonction pour inverser une chaîne de caractères
function Reverse_String(Str : in Unbounded_String) return Unbounded_String is
    Result : Unbounded_String := To_Unbounded_String(""); -- Initialise le résultat comme une chaîne vide
begin
    for I in reverse 1 .. Length(Str) loop -- Parcourt la chaîne en sens inverse
        Result := Result & Element(Str, I); -- Ajoute chaque caractère à la fin du résultat
    end loop;
    return Result;
end Reverse_String;

-- Fonction pour vérifier si le type d'initialisation est un tableau
function Is_Array_Init_Type(IR : in Memory.T_Instructions) return Boolean is
    Value : Unbounded_String;
    Close_Paren_Pos : Integer;

begin
    Close_Paren_Pos := To_String(IR.Token4)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(IR.Token4)), ":"); -- Trouve la position de la parenthèse fermante dans le token 4

    Value := S(Slice(IR.Token4, 1, Close_Paren_Pos)); -- Extrait la valeur du token 4
    return Value = S("TAB"); -- Retourne vrai si la valeur est égale à "TAB", faux sinon
end Is_Array_Init_Type; 


-- Procédure pour initialiser un tableau
procedure Init_Array(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type; Variable_Type  : in Register.T_Types) is
    Value : Unbounded_String;
    Array_Size : Character;
    Name : Unbounded_String;
begin
    Array_Size := Element(IR.Token4, Length(IR.Token4)); -- Extrait la taille du tableau du token 4
    for Num in 1..(Character'Pos(Array_Size)- Character'Pos('0')) loop -- Pour chaque élément du tableau
        Name := IR.Token2;
        Append(Name,S("("));
        Append(Name, To_Unbounded_String(Trim(Integer'Image(Num), Ada.Strings.Left)));
        Append(Name,S(")"));
        Register.Add_Variable(Registre, Name, Variable_Type, S("")); -- Ajoute une variable au registre pour chaque élément du tableau
    end loop;
end Init_Array;

-- Fonction pour obtenir l'index d'un tableau
function Get_Array_Index(Token : In Unbounded_String; Registre : in Register.Register_Type) return Unbounded_String is
    Value : Register.Variable_Record;
    Open_Paren_Pos : Integer;
    Close_Paren_Pos : Integer;
    Array_Index : Unbounded_String;
    Name : Unbounded_String;
begin
    -- Trouve la position de la première parenthèse ouvrante et dernière parenthèse fermante dans le token
    Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Token), "(");
    Close_Paren_Pos := To_String(Token)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Token)), ")");
    
    Name := S(Slice(Token, 1, Open_Paren_Pos)); -- Extrait le nom du tableau du token
    Array_Index := S(Slice(Token, Open_Paren_Pos + 1, Close_Paren_Pos)); -- Extrait l'index du tableau du token
    If Array_Index >= S("1") and Array_Index <= S("9") then -- Si l'index est un nombre entre 1 et 9, retourne le token
        return Token;
    else
        Value := Register.Get_Variable(Registre,Array_Index); -- Sinon, obtient la valeur de la variable correspondant à l'index et la retourne
        return Name & Value.Value & S(")");
    end if;
    
end Get_Array_Index;

-- Fonction pour vérifier si une variable est un tableau
function Is_Variable_Array(Token : in Unbounded_String) return Boolean is
    Open_Paren_Pos : Integer;
    Close_Paren_Pos : Integer;
begin
    -- Trouve la position de la première parenthèse ouvrante et dernière parenthèse fermante dans le token
    Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Token), "(");
    Close_Paren_Pos := To_String(Token)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Token)), ")");

    return Open_Paren_Pos /= 0 and Close_Paren_Pos /= 0; -- Retourne vrai si les parenthèses ouvrante et fermante sont présentes, faux sinon
end Is_Variable_Array;

-- Procédure pour assigner une valeur à une variable
procedure Assign_Value(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is
    Current : Register.Variable_Record;
    Variable : Register.Variable_Record;
    Name : Unbounded_String;
begin
    if Is_Variable_Array(IR.Token1) then -- Si la variable est un tableau, obtient l'index du tableau
        Name := Get_Array_Index(IR.Token1, Registre);
        Current := Register.Get_Variable(Registre, Name);
    else -- Sinon, obtient la variable directement
        Current := Register.Get_Variable(Registre, IR.Token1);
    end if; 

    if Register.Contains_Name(Registre, IR.Token2) then -- Si le token 2 est une variable, obtient sa valeur et l'assigne à la variable courante
        Variable := Register.Get_Variable(Registre,IR.Token2);
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, Variable.Value);
    elsif Is_Variable_Array(IR.Token2) then -- Si le token 2 est un tableau, obtient l'index du tableau et assigne sa valeur à la variable courante
        Variable := Register.Get_Variable(Registre,Get_Array_Index(IR.Token2, Registre));
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, Variable.Value);
    elsif Register.T_Types'Pos(Current.T_Type) = Register.T_Types'Pos(Register.T_Caractere) then -- Si la variable courante est de type caractère, convertit le token 2 en caractère et l'assigne à la variable courante
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, To_Unbounded_String(Trim(Integer'Image(Character'Pos((Element(IR.Token2, Length(IR.Token2)-1)))), Ada.Strings.Left)));
    else -- Sinon, assigne directement le token 2 à la variable courante
        Register.Edit_Variable(Registre, Current.Name, Current.T_Type, IR.Token2);

    end if;
end Assign_Value;

-- Fonction pour convertir une chaîne de caractères en valeur entière
function String_Hash(Str : String) return Integer is
    Sum : Integer := 0;
begin
    for I in Str'Range loop -- Parcourt chaque caractère de la chaîne
        Sum := Sum + Character'Pos(Str(I)); -- Ajoute la valeur ASCII du caractère à la somme
    end loop;
    return Sum;
end String_Hash;

-- Fonction pour vérifier si une variable est un string
function Is_String_Type(Token : Unbounded_String) return Boolean is
    Value : Unbounded_String;
    Close_Paren_Pos : Integer;
    Open_Paren_Pos : Integer;

begin
    Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Token), """"); -- Trouve la position du premier guillemet dans le token
    Close_Paren_Pos := To_String(Token)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Token)), """"); -- Trouve la position du dernier guillemet dans le token
    
    return Open_Paren_Pos /= 0; -- Retourne vrai si un guillemet a été trouvé, faux sinon
end Is_String_Type;

-- Procedure pour assigner une valeur à une variable avec opération
procedure Assign_With_Operation(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is
    Operator : Unbounded_String;
    Current : Register.Variable_Record;
    Left : Register.Variable_Record;
    Right : Register.Variable_Record;
    Left_Value : Integer;
    Right_Value : Integer;
    Result : Integer;

begin 
    -- Si le token 1 est un tableau, obtient l'index du tableau
    if Is_Variable_Array(IR.Token1) then 
        
        Current.Name := Get_Array_Index(IR.Token1, Registre); 
    end if;
    Current := Register.Get_Variable(Registre,IR.Token1); -- Obtient la variable courante
    Operator := IR.Token3; -- Obtient l'opérateur de l'instruction

    -- Vérifie si le token 2 est une variable ou une valeur
    if Register.Contains_Name(Registre, IR.Token2) then
        Left := Register.Get_Variable(Registre,IR.Token2); -- Si c'est une variable, obtient sa valeur
        if Register.T_Types'Pos(Left.T_Type) = Register.T_Types'Pos(Register.T_Chaine) then
            Left_Value := String_Hash(To_String(Left.Value)); -- Si la variable est de type chaîne, convertit la chaîne en valeur entière
        else
            Left_Value :=  Integer'Value(To_String (Left.Value)); -- Sinon, convertit la valeur de la variable en entier
        end if;
    -- Vérifie si le token 2 est un tableau
    elsif Is_Variable_Array(IR.Token2) then
        Left := Register.Get_Variable(Registre,Get_Array_Index(IR.Token2, Registre)); -- Si c'est un tableau, obtient l'index du tableau et convertit sa valeur en entier
        Left_Value := Integer'Value(To_String (Left.Value));
    elsif Register.T_Types'Pos(Current.T_Type) = Register.T_Types'Pos(Register.T_Caractere) then 
        Left_Value := Character'Pos(Element(IR.Token2, Length(IR.Token2)-1)); -- Si la variable courante est de type caractère, convertit le token 2 en caractère et obtient sa valeur ASCII
    elsif Is_String_Type(IR.Token2) then
        Left_Value := String_Hash(To_String(IR.Token2)); -- Si le token 2 est une chaîne, convertit la chaîne en valeur entière
    else
        Left_Value :=  Integer'Value(To_String (IR.Token2)); -- Sinon, convertit directement le token 2 en entier
    end if;

    -- Vérifie si le token 4 est une variable ou une valeur
    if Register.Contains_Name(Registre, IR.Token4) then
        Right := Register.Get_Variable(Registre,IR.Token4); -- Si c'est une variable, obtient sa valeur
        if Register.T_Types'Pos(Left.T_Type) = Register.T_Types'Pos(Register.T_Chaine) then
            Right_Value := String_Hash(To_String(Right.Value)); -- Si la variable est de type chaîne, convertit la chaîne en valeur entière
        else
            Right_Value :=  Integer'Value(To_String (Right.Value)); -- Sinon, convertit la valeur de la variable en entier
        end if;
    elsif Is_Variable_Array(IR.Token2) then -- Vérifie si le token 4 est un tableau
        Right := Register.Get_Variable(Registre,Get_Array_Index(IR.Token4, Registre)); -- Si c'est un tableau, obtient l'index du tableau et convertit sa valeur en entier
        Right_Value :=  Integer'Value(To_String (Right.Value));
    elsif Register.T_Types'Pos(Current.T_Type) = Register.T_Types'Pos(Register.T_Caractere) then
        Right_Value := Character'Pos(Element(IR.Token4, Length(IR.Token4)-1)); -- Si la variable courante est de type caractère, convertit le token 4 en caractère et obtient sa valeur ASCII
    elsif Is_String_Type(IR.Token4) then
        Right_Value := String_Hash(To_String(IR.Token4)); -- Si le token 4 est une chaîne, convertit la chaîne en valeur entière
    else
        Right_Value :=  Integer'Value(To_String (IR.Token4)); -- Sinon, convertit directement le token 4 en entier
    end if;

    -- Effectue l'opération spécifiée par l'opérateur sur les valeurs gauche et droite et stocke le résultat
    if Operator = S("AND") then
        Result := (Integer(Unsigned_Integer(Left_Value) and Unsigned_Integer(Right_Value))); -- Effectue une opération AND bit à bit
    elsif Operator = S("OR") then
        Result := Integer(Unsigned_Integer(Left_Value) or Unsigned_Integer(Right_Value)); -- Effectue une opération OR bit à bit
    elsif Operator = S("=") then
        Result := (if Left_Value = Right_Value then 1 else 0); -- Compare les valeurs gauche et droite pour l'égalité
    elsif Operator = S(">") then
        Result := (if Left_Value > Right_Value then 1 else 0); -- Vérifie si la valeur gauche est supérieure à la valeur droite
    elsif Operator = S("<") then
        Result := (if Left_Value < Right_Value then 1 else 0); -- Vérifie si la valeur gauche est inférieure à la valeur droite
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
    Variable_Type_Name := IR.Token3; -- Obtient le type de la variable à partir du token 3
    -- Détermine le type de la variable en fonction de la valeur de Variable_Type_Name
    if Variable_Type_Name = "Entier" then
        Variable_Type := Register.T_Entier;
    elsif Variable_Type_Name = "Booleen" then
        Variable_Type := Register.T_Booleen;
    elsif Variable_Type_Name = "Caractere" then
        Variable_Type := Register.T_Caractere;
    elsif Variable_Type_Name = "Chaine" then
        Variable_Type := Register.T_Chaine;
    end if;
    if Is_Array_Init_Type(IR) then -- Si le type de la variable est un tableau, initialise le tableau
        Init_Array(IR,Registre,Variable_Type);
    else -- Sinon, ajoute la variable au registre
        Register.Add_Variable(Registre, IR.Token2, Variable_Type, S(""));
    end if;
    
end Init_Variable;

-- Procédure pour initialiser un label
procedure Init_Label(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is 
begin
    Register.Add_Variable(Registre, IR.Token2, Register.T_Label, IR.Token3); -- Ajoute le label au registre
end Init_Label;

-- Procédure pour une branche conditionnelle
procedure Conditional_Branch(IR : in Memory.T_Instructions; Registre : in Register.Register_Type; PC : in out Integer) is
    Current : Register.Variable_Record;
    Result : Register.Variable_Record;
begin
    -- Obtient la valeur de la variable de résultat et de la variable courante
    Result := Register.Get_Variable(Registre,IR.Token2);
    Current := Register.Get_Variable(Registre,IR.Token4);
    if Integer'Value(To_String(Result.Value)) > 0 then -- Si la valeur de la variable de résultat est supérieure à 0, met à jour le compteur de programme (PC)
        PC := Integer'Value(To_String(Current.Value)) - 1;
    end if;
end Conditional_Branch;

-- Procédure pour une branche inconditionnelle
procedure Unconditional_Branch(IR : in Memory.T_Instructions; Registre : in Register.Register_Type; PC : in out Integer) is
    Label : Register.Variable_Record;
begin
    if Register.Contains_Name(Registre, IR.Token2) then -- Si le registre contient le label, obtient la valeur du label et met à jour le compteur de programme (PC)
        Label := Register.Get_Variable(Registre,IR.Token2);
        PC := Integer'Value(To_String(Label.Value)) - 1;
    else -- Sinon, met à jour le compteur de programme (PC) avec la valeur du token 2
        PC := Integer'Value(To_String(IR.Token2)) - 1;
    end if;
end Unconditional_Branch;

-- Procédure pour lire une variable
procedure Read_Variable(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) is
    Input_Value : Unbounded_String;
    Current : Register.Variable_Record;
begin
    if Is_Variable_Array(IR.Token2) then -- Si le token 2 est un tableau, obtient la valeur de l'index du tableau
        Current := Register.Get_Variable(Registre, Get_Array_Index(IR.Token2, Registre));
    else -- Sinon, obtient la valeur de la variable
        Current := Register.Get_Variable(Registre, IR.Token2);
    end if;
    Ada.Text_IO.Put_Line("");
    Ada.Text_IO.Put("Input :");
    Get_Line(Input_Value); -- Recupere l'entrée utilisateur
    Register.Edit_Variable(Registre, Current.Name, Current.T_Type, Input_Value);
end Read_Variable; 

-- Procédure pour écrire une variable
procedure Write_Variable(IR : in Memory.T_Instructions; Registre : in Register.Register_Type) is
    Output : Register.Variable_Record;
begin
    if Register.Contains_Name(Registre, IR.Token2) then -- Si le registre contient le nom de la variable, obtient la valeur de la variable
        Output := Register.Get_Variable(Registre,IR.Token2);
        if Register.T_Types'Pos(Output.T_Type) = Register.T_Types'Pos(Register.T_Caractere) then -- Si le type de la variable est un caractère, convertit la valeur en caractère et l'affiche
            Ada.Text_IO.Put_Line(Character'Val(Integer'Value(To_String(Output.Value)))'Image);
        else -- Sinon, affiche simplement la valeur de la variable
            Ada.Text_IO.Put_Line(To_String(Output.Value));
        end if;
    elsif Is_Variable_Array(IR.Token2) then -- Si le token 2 est un tableau, obtient la valeur de l'index du tableau et l'affiche
        Output := Register.Get_Variable(Registre, Get_Array_Index(IR.Token2, Registre)); 
        Ada.Text_IO.Put_Line(To_String(Output.Value));
    else -- Sinon, affiche simplement la valeur du token 2
        Ada.Text_IO.Put_Line(To_String(IR.Token2));
    end if;
end Write_Variable; 

-- Procédure pour une opération nulle
procedure Null_Operation is
begin
    return;
end Null_Operation;

-- Fonction pour vérifier si c'est la fin du programme
function Is_End_Of_Programm(IR : in Memory.T_Instructions) return Boolean is
begin
    if IR.Token1 = S("END") then -- Si le token 1 est "END", retourne vrai
        return True;
    else -- Sinon, retourne faux
        return False;
    end if;
end Is_End_Of_Programm;

end Evaluator;