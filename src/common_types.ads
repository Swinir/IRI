with LinkedList;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
package Common_Types is

    -- Crée une nouvelle instance du package LinkedList avec le type élément String et la procédure d'affichage Put
    package String_Link_List is new LinkedList(Element_Type => Unbounded_String, Print_Element => Put);  
    type String_List is new String_Link_List.T_Linked_List;  -- Définit un nouveau type String_List qui est une instance de la liste chaînée
    
    type Unsigned_Integer is mod 2**Integer'Size;
    
    function S(Source : String) return Unbounded_String renames To_Unbounded_String;
end Common_Types; 