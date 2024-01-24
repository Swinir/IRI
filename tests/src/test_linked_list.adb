with Ada.Text_IO; use Ada.Text_IO;
with LinkedList;

procedure test_linked_list is 

    package Character_LinkedList is new LinkedList(Element_Type => Character, Print_Element => Put);
    use Character_LinkedList;

    List : T_Linked_List;
    Element : constant Character := 'A';

    procedure Test_Init is
    begin
        Init(List);
        pragma Assert(Is_Empty(List));
    end Test_Init;

    procedure Test_Append is
    begin
        Append(List, Element);
        pragma Assert(not Is_Empty(List));
        pragma Assert(Length(List) = 1);
        pragma Assert(Get_Data(List, 1) = Element);
    end Test_Append;

    procedure Test_Pop is
    begin
        Pop(List, Length(List));
        pragma Assert(Is_Empty(List));
        pragma Assert(Length(List) = 0);
    end Test_Pop;

    procedure Test_Append_Two is
    begin
        Append(List, Element);
        pragma Assert(not Is_Empty(List));
        pragma Assert(Length(List) = 1);
        pragma Assert(Get_Data(List, 1) = Element);
        Append(List, 'B');
        pragma Assert(not Is_Empty(List));
        pragma Assert(Length(List) = 2);
        Print_List(List);
        pragma Assert(Get_Data(List, 2) = 'B');
        Append(List, 'C');
        pragma Assert(not Is_Empty(List));
        pragma Assert(Length(List) = 3);
        pragma Assert(Get_Data(List, 3) = 'C');
    end Test_Append_Two;

    procedure Test_Pop_Two is
    begin
        Pop(List, 2);
        pragma Assert(Length(List) = 2);
        pragma Assert(Get_Data(List, 1) = Element);
        pragma Assert(Get_Data(List, 2) = 'C');
    end Test_Pop_Two;

    procedure Test_Clear is
    begin
        Append(List, Element);
        Clear(List);
        pragma Assert(Is_Empty(List));
    end Test_Clear;

    procedure Test_Get_Data_And_Get_Position is
    begin
        Append(List, Element);
        pragma Assert(Get_Data(List, 1) = Element);
        pragma Assert(Get_Position(List, Element) = 1);
        Clear(List);
    end Test_Get_Data_And_Get_Position;

    procedure Test_Empty_List is
    begin
        pragma Assert(Is_Empty(List));
        pragma Assert(Length(List) = 0);
        pragma Assert(Get_Position(List, Element) = -1);
    end Test_Empty_List;

    procedure Test_Append_Multiple is
    begin
        for C in Character range 'B'..'Z' loop
            Append(List, C);
        end loop;
        pragma Assert(Length(List) = 25);
        pragma Assert(Get_Data(List, 1) = 'B');
        pragma Assert(Get_Data(List, 25) = 'Z');
        pragma Assert(Get_Position(List, 'A') = -1);
    end Test_Append_Multiple;

    procedure Test_Pop_Multiple is
    begin
        -- Remove C
        Pop(List, 2);
        pragma Assert(Get_Position(List, 'C') = -1);
        pragma Assert(Get_Position(List, 'D') = 2);
        pragma Assert(Length(List) = 24);
        -- Remove Z
        Pop(List, 24);
        pragma Assert(Get_Position(List, 'Z') = -1);
        pragma Assert(Length(List) = 23);
        -- Remove E
        Pop(List, 3);
        pragma Assert(Get_Position(List, 'E') = -1);
        pragma Assert(Get_Position(List, 'F') = 3);
        pragma Assert(Get_Position(List, 'D') = 2);
        for I in 1..22 loop
            Pop(List, 1);
        end loop;
        pragma Assert(Is_Empty(List));
        pragma Assert(Length(List) = 0);
    end Test_Pop_Multiple;

    procedure Test_Is_Empty is
    begin
        pragma Assert(Is_Empty(List));
    end Test_Is_Empty;

    procedure Test_Insert_Beg is
    begin
        Insert_Beginning(List, 'A');
        Insert_Beginning(List, 'B');
        Insert_Beginning(List, 'C');
        pragma Assert(Get_Position(List, 'A') = 3);
        pragma Assert(Get_Position(List, 'B') = 2);
        pragma Assert(Get_Position(List, 'C') = 1);
        Clear(List);
        pragma Assert(Is_Empty(List));
    end Test_Insert_Beg;

begin
    Put_Line("Début des tests de linked_list...");
    Test_Init;
    Print_List(List);
    Test_Append;
    Print_List(List);
    Test_Pop;
    Print_List(List);
    Test_Append_Two;
    Print_List(List);
    Test_Pop_Two;
    Print_List(List);
    Test_Clear;
    Print_List(List);
    Test_Get_Data_And_Get_Position;
    Print_List(List);
    Test_Empty_List;
    Test_Append_Multiple;
    Print_List(List);
    Put_Line("");
    Test_Pop_Multiple;
    Test_Is_Empty;
    Test_Insert_Beg;
    Put_Line("Fin des tests de linked_list. Les tests sont passés.");
end test_linked_list;