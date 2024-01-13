-- Spécification d'un module LinkedList, une liste chaînée.

generic
   type Element_Type is private;


package LinkedList is
   type T_Linked_List is private;

   -- Initialise une liste chaînée.
   --
   -- Paramètres :
   --     Linked_List : la liste chaînée à initialiser
   --
   -- Assure :
   --     Is_Empty (Linked_List)
	procedure Init(Linked_List : out T_Linked_List) with
		Post => Is_Empty (Linked_List);

   -- Ajoute un élément à la liste chaînée.
   --
   -- Paramètres :
   --     Linked_List : la liste chaînée à laquelle ajouter
   --     Data : les données à ajouter
   --
   -- Assure :
   --     La liste chaînée n'est pas vide après l'opération
   --     La longueur de la liste chaînée a augmenté de 1
   --     Le dernier élément de la liste chaînée est les données ajoutées
   procedure Append(Linked_List : in out T_Linked_List; Data : Element_Type) with
      Post => not Is_Empty (Linked_List) and then Length(Linked_List) - 1 = Length'(Old(Linked_List)) 
              and Get_Data(Linked_List, Length(Linked_List)) = Data;

   -- Retire un élément de la liste chaînée.
   --
   -- Paramètres :
   --     Linked_List : la liste chaînée à partir de laquelle retirer
   --     Data : les données à retirer
   --
   -- Nécessite :
   --     Linked_List n'est pas vide
   --
   -- Assure :
   --     La longueur de la liste chaînée a diminué de 1
   procedure Pop(Linked_List : in out T_Linked_List; Data : Element_Type) with
      Pre => not Is_Empty (Linked_List),
      Post => Length(Linked_List) = Length'(Old(Linked_List)) - 1;

   -- Vérifie si la liste chaînée est vide.
   --
   -- Paramètres :
   --     Linked_List : la liste chaînée à vérifier
   --
   -- Renvoie :
   --     Vrai si la liste chaînée est vide, faux sinon
   function Is_Empty(Linked_List : in T_Linked_List) return Boolean;

   -- Obtient la longueur de la liste chaînée.
   --
   -- Paramètres :
   --     Linked_List : la liste chaînée dont on veut obtenir la longueur
   --
   -- Renvoie :
   --     La longueur de la liste chaînée
   function Length(LinkedList : in T_Linked_List) return Integer;

   -- Efface tous les éléments de la liste chaînée.
   --
   -- Paramètres :
   --     Linked_List : la liste chaînée à effacer
   --
   -- Nécessite :
   --     Linked_List n'est pas vide
   --
   -- Assure :
   --     Is_Empty (Linked_List)
   procedure Clear(Linked_List : in out T_Linked_List) with
      Pre => not Is_Empty (Linked_List),
      Post => Is_Empty (Linked_List);

   -- Obtient les données à un index spécifique dans la liste chaînée.
   --
   -- Paramètres :
   --     Linked_List : la liste chaînée à partir de laquelle obtenir les données
   --     Index : l'index à partir duquel obtenir les données
   --
   -- Nécessite :
   --     Linked_List n'est pas vide
   --     Index est dans la plage valide (1 à la longueur de la liste chaînée)
   --
   -- Renvoie :
   --     Les données à l'index spécifié dans la liste chaînée
   --
   function Get_Data(Linked_List : in T_Linked_List; Index : in Integer) return Element_Type with
      Pre => not Is_Empty (Linked_List) and then Index in 1 .. Length(Linked_List),
      Post => Get_Position(Linked_List, Linked_List'Result) = Index;

   -- Obtient la position d'un élément spécifique dans la liste chaînée.
   --
   -- Paramètres :
   --     Linked_List : la liste chaînée dans laquelle chercher
   --     Data : les données à chercher
   --
   -- Nécessite :
   --     Linked_List n'est pas vide
   --
   -- Renvoie :
   --     La position de l'élément dans la liste chaînée, ou -1 si l'élément n'est pas trouvé
   function Get_Position(Linked_List : in T_Linked_List; Data : in Element_Type) return Integer with
      Pre => not Is_Empty (Linked_List),
      Post => (Linked_List'Result in 1 .. Length(Linked_List)) or else (Linked_List'Result = -1);


   -- Imprime tous les éléments de la liste chaînée.
   --
   -- Paramètres :
   --     Linked_List : la liste chaînée à imprimer
   --
   -- Lors de l'exécution de `Print_List`, chaque élément de la liste chaînée serait imprimé sur une nouvelle ligne.
   -- Par exemple, si la liste chaînée contient les entiers 1, 2 et 3, l'output serait :
   -- [1,2,3]
   generic
      with procedure Print_Element (Item : in Element_Type);
   procedure Print_List(Linked_List : in T_Linked_List);

private

   -- Définition d'un nœud dans la liste chaînée.
   type T_Node is record
      Data : Element_Type;
      Next : access T_Node;
   end record;

   -- Définition de la liste chaînée.
   type T_Linked_List is record
      Head : access T_Node;
      Length : Integer;
   end record;

end LinkedList;