with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body register is

    procedure Put(variable : in Variable_Record) is
    begin
        Put("Variable's name : ");
        Put(variable.Name);
        Put("  ---  Value : ");
        Put(variable.Value);
        Put("  ---  Type : ");
        Put(T_Types'Image(variable.T_Type) );
    end Put;

    procedure Init(Register : out Register_Type) is
    begin
        Variable_List.Init(Register);
    end;


    procedure Add_Variable
      (Register : in out Register_Type;
        Name     : in     Unbounded_String;
        T_Type     : in     T_Types;
        Value    : in     Unbounded_String) is
    begin
        Variable_List.Append(Register, (Name, T_Type, Value));
    end Add_Variable;


    procedure Edit_Variable
    (Register : in out Register_Type;
        Name     : in     Unbounded_String;
        T_Type   : in     T_Types;
        Value    : in     Unbounded_String) is
        Current_Element : Variable_Record;
        Found_Element_Index : Integer;
    begin
        Found_Element_Index := 1;
        for I in 1 .. Variable_List.Length(Register) loop
            Current_Element := Variable_List.Get_Data(Register, I);
            if Current_Element.Name = Name then
                Found_Element_Index := I;
            end if;
        end loop;
        Variable_List.Edit_Data(Register, Found_Element_Index, (Name, T_Type, Value));
    end Edit_Variable;


    function Contains_Name
    (Register : in Register_Type; Name : in Unbounded_String)
    return Boolean is
        Current_Element : Variable_Record;
    begin
        for Index in 1 .. Variable_List.Length(Register) loop
            Current_Element := Variable_List.Get_Data(Register, Index);
            if Current_Element.Name = Name then
                Return True;
            end if;
        end loop;
        return False;
    end Contains_Name;


    function Get_Variable
      (Register : in Register_Type;
        Name     : in Unbounded_String)
        return Variable_Record is
        Current_Element : Variable_Record;
        Found_Element_Index : Integer;
    begin
        for I in 1 .. Variable_List.Length(Register) loop
            Current_Element := Variable_List.Get_Data(Register, I);
            if Current_Element.Name = Name then
                Found_Element_Index := I;
            end if;
        end loop;
        return Variable_List.Get_Data(Register, Found_Element_Index);
    end Get_Variable;


    function Length(Register : in Register_Type) return Integer is
    begin
        return Variable_List.Length(Register);
    end Length;

end register;