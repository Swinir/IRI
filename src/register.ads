with Ada.Text_IO; use Ada.Text_IO;
with LinkedList;

package Register is
    -- Définition du type d'énumération T_Types
    type T_Types is (T_Entier, T_Caractere, T_Label, T_Booleen);

    -- Définition du type d'enregistrement Variable_Record
    type Variable_Record is record
        Name  : String(1..50);
        T_Type  : T_Types;
        Value : String(1..50);
    end record;

    -- La procédure Put est utilisée pour afficher une variable.
    --
    -- L'affichage sera de la forme suivante : "key" => "value"("type")
    --
    -- Paramètres :
    --     variable : la variable à afficher
    procedure Put(variable : in Variable_Record);

    -- Instanciation du package LinkedList avec le type d'élément Variable_Record
    package Variable_List is new LinkedList(Element_Type => Variable_Record, Print_Element => Put);
    subtype Register_Type is Variable_List.T_Linked_List;

    -- Procédure pour ajouter une variable dans le registre,
    --
    -- Paramètres :
    --     Register : le registre dans lequel ajouter
    --     Name : le nom de la variable
    --     T_Type : le type de la variable
    --     Value : la valeur de la variable
    --
    -- Nécessite :
    --     La variable n'existe pas déjà dans le registre
    --
    -- Assure :
    --     La variable a été ajoutée au registre
    procedure Add_Variable
      (Register : in out Register_Type;
        Name     : in     String;
        T_Type     : in     T_Types;
        Value    : in     String) with
        Pre  => not Variable_List.Contains(Register, (Name, T_Type, Value)),
        Post => Variable_List.Contains(Register, (Name, T_Type, Value));

    -- Procédure pour modifier une variable présente dans le registre
    --
    -- Paramètres :
    --     Register : le registre dans lequel modifier
    --     Name : le nom de la variable
    --     T_Type : le nouveau type de la variable
    --     Value : la nouvelle valeur de la variable
    --
    -- Nécessite :
    --     La variable existe dans le registre
    --
    -- Assure :
    --     La variable a été modifiée dans le registre
    procedure Edit_Variable
      (Register : in out Register_Type;
        Name     : in     String;
        T_Type     : in     T_Types;
        Value    : in     String) with
        Pre  => Variable_List.Contains_Name(Register, Name),
        Post => Variable_List.Contains(Register, (Name, T_Type, Value));

    -- Fonction pour obtenir la valeur d'une variable présente dans le registre en fonciton de ça c
    --
    -- Paramètres :
    --     Register : le registre à partir duquel obtenir
    --     Name : le nom de la variable
    --
    -- Renvoie :
    --     La variable demandée
    --
    -- Nécessite :
    --     La variable existe dans le registre
    --
    -- Assure :
    --     La variable retournée est celle demandée
    function Get_Variable
      (Register : in Register_Type;
        Name     : in String)
        return Variable_Record with
        Pre  => Variable_List.Contains_Name(Register, Name),
        Post => Get_Variable'Result.Name = Name;

end Register;