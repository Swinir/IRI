with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body LinkedList is

	procedure Free is
		new Ada.Unchecked_Deallocation (T_Node, T_Node_Access);

   procedure Init(Linked_List : out T_Linked_List) is
   begin
      Linked_List.Head := null;
      Linked_List.Length := 0;
   end Init;

   procedure Insert_Beginning(Linked_List : in out T_Linked_List; Data : in Element_Type) is
   begin
      if Is_Empty(Linked_List) then
         Linked_List.Head := new T_Node'(Data, Null);
      else
         Linked_List.Head := new T_Node'(Data, Linked_List.Head);
      end if;
   end Insert_Beginning;

   procedure Append(Linked_List : in out T_Linked_List; Data : in Element_Type) is
      Current : T_Node_Access;
   begin
      if Is_Empty(Linked_List) then
         Linked_List.Head := new T_Node'(Data, Null);
      else
         Current := Linked_List.Head;
         while Current.All.Next /= null loop
            Current := Current.All.Next;
         end loop;
         Current.All.Next := new T_Node'(Data, Null);
      end if;
      Linked_List.Length := Linked_List.Length + 1;
   end Append;

   procedure Pop(Linked_List : in out T_Linked_List; Index : in Integer) is
      Previous_Element : T_Node_Access;
      Current : T_Node_Access;
      I : Integer;
   begin
      if Length(Linked_List) = 1 then
         Free(Linked_List.Head);
         Linked_List.Head := null;
      else 
         I := 1;
         Previous_Element := Linked_List.Head;
         while I < Index-1 loop
            Previous_Element := Previous_Element.All.Next;
            I := I + 1;
         end loop;
         Current := Previous_Element.All.Next;
         Previous_Element.All.Next := Current.All.Next;
         Free(Current);
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
         Next := Current.All.Next;
         Free(Current);
         Current := Next;
      end loop;
      Linked_List.Head := null;
      Linked_List.Length := 0;
   end Clear;

   function Get_Data(Linked_List : in T_Linked_List; Index : in Integer) return Element_Type is
      Current : T_Node_Access;
      I : Integer;
   begin
      Current := Linked_List.Head;
      I := 1;
      while I < Index loop
         Current := Current.Next;
         I := I + 1;
      end loop;
      return Current.Data;
   end Get_Data;

   function Get_Position(Linked_List : in T_Linked_List; Data : in Element_Type) return Integer is
      Current : T_Node_Access;
      I : Integer;
   begin
      Current := Linked_List.Head;
      I := 1;
      while Current /= null loop
         if Current.All.Data = Data then
            return I;
         end if;
         Current := Current.All.Next;
         I := I + 1;
      end loop;
      return -1;
   end Get_Position;

   procedure Edit_Data(Linked_List : in out T_Linked_List; Index : in Integer; New_Data : in Element_Type) is
      Current : T_Node_Access;
      I : Integer;
   begin
      Current := Linked_List.Head;
      I := 1;
      while I < Index loop
         Current := Current.Next;
         I := I + 1;
      end loop;
      Current.All.Data := New_Data;
   end Edit_Data;

   procedure Print_List(Linked_List : in T_Linked_List) is
      Current : T_Node_Access;
   begin
      Current := Linked_List.Head;
      Put("[");
      while Current /= null loop
         Print_Element(Current.All.Data);
         
         if Current.All.Next /= null then
            Put(", ");
         end if;
         
         Current := Current.All.Next;
      end loop;
      Put("]");
      Put_Line("");
   end Print_List;

end LinkedList;