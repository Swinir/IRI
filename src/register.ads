with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with LinkedList;

package Register is
    -- Définition du type d'énumération T_Types
    type T_Types is (T_Entier, T_Caractere, T_Label, T_Booleen, T_Tableau, T_Chaine);

    -- Définition du type d'enregistrement Variable_Record
    type Variable_Record is record
        Name  : Unbounded_String;
        T_Type  : T_Types;
        Value : Unbounded_String;
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
    use Variable_List;

    procedure Init(Register : out Register_Type);

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
        Name     : in     Unbounded_String;
        T_Type     : in     T_Types;
        Value    : in     Unbounded_String) with
        Pre  => Variable_List.Get_Position(Register, (Name, T_Type, Value)) = -1,
        Post => Variable_List.Get_Position(Register, (Name, T_Type, Value)) /= -1;


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
        Name     : in     Unbounded_String;
        T_Type     : in     T_Types;
        Value    : in     Unbounded_String) with
        Pre  => Contains_Name(Register, Name),
        Post => Variable_List.Get_Position(Register, (Name, T_Type, Value)) /= -1;


    -- Fonction pour obtenir la valeur d'une variable présente dans le registre en fonciton de son nom
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
        Name     : in Unbounded_String)
        return Variable_Record with
        Pre  => Contains_Name(Register, Name),
        Post => Get_Variable'Result.Name = Name;


    -- Vérifie si le registre contient une variable.
    --
    -- Paramètres :
    --     Register_Type : le registre à vérifier
    --     Name : le nom de la variable à rechercher
    --
    -- Renvoie :
    --     Vrai si la variable est trouvé dans la liste chaînée, faux sinon
    function Contains_Name(Register : in Register_Type; Name : in Unbounded_String) return Boolean with
        Pre => not Variable_List.Is_Empty(Register);


    -- Function to get the length of the register
    --
    -- Parameters:
    --     Register : the register to get the length of
    --
    -- Returns:
    --     The length of the register
    function Length(Register : in Register_Type) return Integer;

end Register;