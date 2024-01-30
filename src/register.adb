with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body register is

    -- Procédure pour afficher une variable
    procedure Put(variable : in Variable_Record) is
    begin
        Put("Variable's name : ");
        Put(variable.Name);
        Put("  ---  Value : ");
        Put(variable.Value);
        Put("  ---  Type : ");
        Put(T_Types'Image(variable.T_Type) ); -- Affiche le type de la variable
    end Put;

    -- Procédure pour initialiser le registre
    procedure Init(Register : out Register_Type) is
    begin
        Variable_List.Init(Register);
    end;


    -- Procédure pour ajouter une variable au registre
    procedure Add_Variable
      (Register : in out Register_Type;
        Name     : in     Unbounded_String;
        T_Type     : in     T_Types;
        Value    : in     Unbounded_String) is
    begin
        Variable_List.Append(Register, (Name, T_Type, Value));
    end Add_Variable;


    -- Procédure pour modifier une variable dans le registre
    procedure Edit_Variable
    (Register : in out Register_Type;
        Name     : in     Unbounded_String;
        T_Type   : in     T_Types;
        Value    : in     Unbounded_String) is
        Current_Element : Variable_Record; -- Élément courant
        Found_Element_Index : Integer; -- Index de l'élément trouvé
    begin
        Found_Element_Index := 1; 
        for I in 1 .. Variable_List.Length(Register) loop -- Boucle sur chaque variable dans le registre
            Current_Element := Variable_List.Get_Data(Register, I); -- Obtient la variable courante
            if Current_Element.Name = Name then -- Si le nom de la variable courante est égal au nom donné
                Found_Element_Index := I; -- Met à jour l'index de l'élément trouvé
            end if;
        end loop;
        Variable_List.Edit_Data(Register, Found_Element_Index, (Name, T_Type, Value)); -- Modifie la variable à l'index trouvé avec le nouveau nom, type et valeur
    end Edit_Variable;


    -- Fonction pour vérifier si un nom est contenu dans le registre
    function Contains_Name
    (Register : in Register_Type; Name : in Unbounded_String)
    return Boolean is
        Current_Element : Variable_Record; -- Élément courant
    begin
        for Index in 1 .. Variable_List.Length(Register) loop -- Boucle sur chaque variable dans le registre
            Current_Element := Variable_List.Get_Data(Register, Index); -- Obtient la variable courante
            if Current_Element.Name = Name then -- Si le nom de la variable courante est égal au nom donné
                Return True; -- Retourne vrai
            end if;
        end loop;
        return False; -- Retourne faux si le nom n'est pas trouvé
    end Contains_Name;


    -- Fonction pour obtenir une variable du registre
    function Get_Variable
      (Register : in Register_Type;
        Name     : in Unbounded_String)
        return Variable_Record is
        Current_Element : Variable_Record; -- Élément courant
        Found_Element_Index : Integer; -- Index de l'élément trouvé
    begin
        for I in 1 .. Variable_List.Length(Register) loop -- Boucle sur chaque variable dans le registre
            Current_Element := Variable_List.Get_Data(Register, I); -- Obtient la variable courante
            if Current_Element.Name = Name then -- Si le nom de la variable courante est égal au nom donné
                Found_Element_Index := I; -- Met à jour l'index de l'élément trouvé
            end if;
        end loop;
        return Variable_List.Get_Data(Register, Found_Element_Index); -- Retourne la variable à l'index trouvé
    end Get_Variable;


    -- Fonction pour obtenir la longueur du registre
    function Length(Register : in Register_Type) return Integer is
    begin
        return Variable_List.Length(Register);
    end Length;

end register;