with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body LinkedList is

   -- Procédure pour libérer la mémoire allouée à un Noeud
	procedure Free is
		new Ada.Unchecked_Deallocation (T_Node, T_Node_Access);


   -- Procédure pour initialiser la liste chaînée
   procedure Init(Linked_List : out T_Linked_List) is
   begin
      Linked_List.Head := null; -- La tête de la liste est initialisée à null
      Linked_List.Length := 0; -- La longueur de la liste est initialisée à 0
   end Init;


   -- Procédure pour insérer un élément au début de la liste chaînée
   procedure Insert_Beginning(Linked_List : in out T_Linked_List; Data : in Element_Type) is
   begin
      if Is_Empty(Linked_List) then -- Si la liste est vide, crée un nouveau Noeud et le met en tête de la liste
         Linked_List.Head := new T_Node'(Data, Null);
      else -- Sinon, crée un nouveau Noeud et le met en tête de la liste, le reste de la liste devient le suivant du nouveau Noeud 
         Linked_List.Head := new T_Node'(Data, Linked_List.Head);
      end if;
      Linked_List.Length := Linked_List.Length + 1;
   end Insert_Beginning;


   -- Procédure pour ajouter un élément à la fin de la liste chaînée
   procedure Append(Linked_List : in out T_Linked_List; Data : in Element_Type) is
      Current : T_Node_Access; -- Noeud courant
   begin
      if Is_Empty(Linked_List) then -- Si la liste est vide, crée un nouveau Noeud et le met en tête de la liste
         Linked_List.Head := new T_Node'(Data, Null);
      else
         Current := Linked_List.Head;
         while Current.All.Next /= null loop -- Parcourt la liste jusqu'à la fin
            Current := Current.All.Next;
         end loop;
         Current.All.Next := new T_Node'(Data, Null); -- Ajoute le nouveau Noeud à la fin de la liste
      end if;
      Linked_List.Length := Linked_List.Length + 1; -- Incrémente la longueur de la liste
   end Append;


   -- Procédure pour supprimer un élément à un index donné de la liste chaînée
   procedure Pop(Linked_List : in out T_Linked_List; Index : in Integer) is
      Previous_Element : T_Node_Access; -- Élément précédent
      Current : T_Node_Access; -- Noeud courant
      I : Integer; -- Compteur
   begin
      if Index = 1 then -- Si l'index est 1, le Noeud courant devient la tête de la liste
         Current := Linked_List.Head;
         if Length(Linked_List) = 1 then -- Si la liste n'a qu'un seul élément, la tête de la liste devient null
            Linked_List.Head := null;
         else -- Sinon, la tête de la liste devient le suivant du Noeud courant
            Linked_List.Head := Linked_List.Head.All.Next;
         end if;
      else 
         I := 1; 
         Previous_Element := Linked_List.Head;
         while I < Index-1 loop -- Parcourt la liste jusqu'à l'index donné
            Previous_Element := Previous_Element.All.Next;
            I := I + 1;
         end loop;
         Current := Previous_Element.All.Next;
         Previous_Element.All.Next := Current.All.Next; -- Le noeud suivant de l'élément précédent devient le suivant du Noeud courant
      end if;
      Free(Current); -- Libère la mémoire allouée au Noeud courant
      Linked_List.Length := Linked_List.Length - 1;
   end Pop;


   -- Fonction pour vérifier si la liste chaînée est vide
   function Is_Empty(Linked_List : in T_Linked_List) return Boolean is
   begin
      return Linked_List.Head = null;
   end Is_Empty;


   -- Fonction pour avoir la longueure de la chaine
   function Length(LinkedList : in T_Linked_List) return Integer is
   begin
      return LinkedList.Length;
   end Length;


   -- Procédure pour vider la liste chaînée
   procedure Clear(Linked_List : in out T_Linked_List) is
      Current, Next : T_Node_Access; -- Noeuds courant et suivant
   begin
      Current := Linked_List.Head; -- Le Noeud courant devient la tête de la liste
      while Current /= null loop -- Parcourt la liste jusqu'à la fin
         Next := Current.All.Next;
         Free(Current); -- Libère la mémoire allouée au Noeud courant
         Current := Next;
      end loop; 
      Linked_List.Head := null;
      Linked_List.Length := 0;
   end Clear;


   -- Fonction pour obtenir la donnée d'un noeud à un index donné de la liste chaînée
   function Get_Data(Linked_List : in T_Linked_List; Index : in Integer) return Element_Type is
      Current : T_Node_Access; -- Noeud courant
      I : Integer; -- Compteur
   begin
      Current := Linked_List.Head; -- Le Noeud courant devient la tête de la liste
      I := 1;
      while I < Index loop -- Parcourt la liste jusqu'à l'index donné
         Current := Current.All.Next;
         I := I + 1;
      end loop;
      return Current.All.Data;
   end Get_Data;


   -- Fonction pour obtenir la position d'une donnée dans la liste chaînée
   function Get_Position(Linked_List : in T_Linked_List; Data : in Element_Type) return Integer is
      Current : T_Node_Access; -- Noeud courant
      I : Integer; -- Compteur
   begin
      Current := Linked_List.Head;
      I := 1;
      while Current /= null loop -- Parcourt la liste jusqu'à la fin
         if Current.All.Data = Data then -- Si la donnée du Noeud courant est égale à la donnée donnée
            return I; -- Retourne l'index actuel
         end if;
         Current := Current.All.Next;
         I := I + 1;
      end loop;
      return -1; -- Retourne -1 si la donnée n'est pas trouvée dans la liste
   end Get_Position;


   -- Procédure pour modifier la donnée d'un noeud à un index donné de la liste chaînée
   procedure Edit_Data(Linked_List : in out T_Linked_List; Index : in Integer; New_Data : in Element_Type) is
      Current : T_Node_Access; -- Noeud courant
      I : Integer; -- Compteur
   begin
      Current := Linked_List.Head;
      I := 1;
      while I < Index loop -- Parcourt la liste jusqu'à l'index donné
         Current := Current.All.Next;
         I := I + 1;
      end loop;
      Current.All.Data := New_Data; -- Modifie la donnée du Noeud trouvé
   end Edit_Data;


   -- Procédure pour afficher la liste chaînée
   procedure Print_List(Linked_List : in T_Linked_List) is
      Current : T_Node_Access; -- Noeud courant
   begin
      Current := Linked_List.Head;
      Put("[");
      while Current /= null loop -- Parcourt la liste jusqu'à la fin
         Print_Element(Current.All.Data);
         
         if Current.All.Next /= null then -- Si le suivant du Noeud courant n'est pas null
            Put(", ");
         end if;
         
         Current := Current.All.Next;
      end loop;
      Put("]");
      Put_Line("");
   end Print_List;

end LinkedList;