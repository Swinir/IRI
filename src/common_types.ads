with LinkedList;
with Ada.Text_IO; use Ada.Text_IO;

package Common_Types is
    -- Crée une nouvelle instance du package LinkedList avec le type élément String et la procédure d'affichage Put
    package String_Link_List is new LinkedList(Element_Type => Character, Print_Element => Put);  
    type String_List is new String_Link_List.T_Linked_List;  -- Définit un nouveau type String_List qui est une instance de la liste chaînée
end Common_Types; 