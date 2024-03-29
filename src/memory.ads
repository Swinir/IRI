with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with LinkedList;

-- Le package Memory fournit une liste chaînée pour stocker les instructions.
package Memory is
   -- Une ligne d'instruction est un enregistrement des quatre tokens de cette même ligne.
   type T_Instructions is record
      Token1 : Unbounded_String := To_Unbounded_String("");
      Token2 : Unbounded_String := To_Unbounded_String("");
      Token3 : Unbounded_String := To_Unbounded_String("");
      Token4 : Unbounded_String := To_Unbounded_String("");
   end record;

   -- La procédure Put est utilisée pour afficher une instruction.
   --
   -- Paramètres :
   --     instruction : l'instruction à afficher
   procedure Put(Instructions : in T_Instructions);

   -- Memoire est une liste chaînée d'instructions.
   --
   -- Nous utilisons donc une instanciation générique du package LinkedList, où le type d'élément est T_Instructions et la procédure d'impression est Put.
   package T_Memory_Linked_List is new LinkedList(Element_Type => T_Instructions, Print_Element => Put);

   -- T_Memory est un nouveau type dérivé de la liste chaînée d'instructions. C'est le type qui sera utilisé par les modules qui souhaitent accéder aux instructions.
   type T_Memory is new T_Memory_Linked_List.T_Linked_List;

end Memory;