-- linked_list.adb

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with LinkedList;
package body LinkedList is

	procedure Free is
		new Ada.Unchecked_Deallocation (T_Node, T_Node_Access);

   procedure Init(Linked_List : out T_Linked_List) is
   begin
      Linked_List.Head := null;
      Linked_List.Length := 0;
   end Init;

   procedure Append(Linked_List : in out T_Linked_List; Data : in Element_Type) is
      Current : T_Node_Access;
   begin
      if Is_Empty(Linked_List) then
         Linked_List.Head := new T_Node'(Data, Null);
      else
         Current := Linked_List.Head;
         while Current.Next /= null loop
            Current := Current.Next;
         end loop;
         Current.Next := new T_Node'(Data, Null);
      end if;
      Linked_List.Length := Linked_List.Length + 1;
   end Append;

   procedure Pop(Linked_List : in out T_Linked_List; Index : in Integer) is
      Previous_Element : T_Node_Access;
      Current : T_Node_Access;
      Element_Index : Integer;
      I : Integer;
   begin
      if Length(Linked_List) = 1 then
         Free(Linked_List.Head);
         Linked_List.Head := null;
      else 
         if Index = 1 then
            Previous_Element := null;
            Current := Linked_List.Head.Next;
            Free(Linked_List.Head);

            Previous.Next := Current.Next;
         else 
            while I < Index-1 loop
               Previous_Element := Previous_Element.Next;
               I := I + 1;
            end loop;
            Current := Previous_Element.Next;
            Linked_List.Head := Current.Next;
         end if;
      end if;
      Linked_List.Length := Linked_List.Length - 1;
   end Pop;

   function Is_Empty(Linked_List : in T_Linked_List) return Boolean is
   begin
      return Linked_List.Head = null;
   end Is_Empty;

   function Length(LinkedList : in T_Linked_List) return Integer is
   begin
      return LinkedList.Length;
   end Length;

   procedure Clear(Linked_List : in out T_Linked_List) is
      Current, Next : T_Node_Access;
   begin
      Current := Linked_List.Head;
      while Current /= null loop
         Next := Current.Next;
         Free(Current);
         Current := Next;
      end loop;
      Linked_List.Head := null;
      Linked_List.Length := 0;
   end Clear;

   function Get_Data(Linked_List : in T_Linked_List; Index : in Integer) return Element_Type is
      Current : access T_Node := Linked_List.Head;
      I : Integer := 1;
   begin
      while I < Index loop
         Current := Current.Next;
         I := I + 1;
      end loop;
      return Current.Data;
   end Get_Data;

   function Get_Position(Linked_List : in T_Linked_List; Data : in Element_Type) return Integer is
      Current : access T_Node := Linked_List.Head;
      I : Integer := 1;
   begin
      while Current /= null loop
         if Current.Data = Data then
            return I;
         end if;
         Current := Current.Next;
         I := I + 1;
      end loop;
      return -1;
   end Get_Position;

   procedure Print_List(Linked_List : in T_Linked_List) is
      Current : access T_Node := Linked_List.Head;
   begin
      while Current /= null loop
         Print_Element(Current.Data);
         Current := Current.Next;
      end loop;
   end Print_List;

end LinkedList;