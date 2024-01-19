with Ada.Text_IO; use Ada.Text_IO;
with LinkedList;

procedure test_linked_list is 

    package Character_LinkedList is new LinkedList(Element_Type => Character, Print_Element => Put);
    use Character_LinkedList;

    List : T_Linked_List;
    Element : constant Character := 'A';

    procedure Printer is new Character_LinkedList.Print_List(Put);

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
        pragma Assert(Get_Data(List, 2) = Element);
        Append(List, 'C');
        pragma Assert(not Is_Empty(List));
        pragma Assert(Length(List) = 3);
        pragma Assert(Get_Data(List, 3) = Element);
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
        for I in 1..25 loop
            Pop(List, I);
        end loop;
        pragma Assert(Is_Empty(List));
        pragma Assert(Length(List) = 0);
    end Test_Pop_Multiple;

    procedure Test_Is_Empty is
    begin
        pragma Assert(Is_Empty(List));
    end Test_Is_Empty;

begin
    Put_Line("Début des tests de linked_list");
    Test_Init;
    Test_Append;
    Test_Pop;
    Test_Append_Two;
    Test_Pop_Two;
    Test_Clear;
    Test_Get_Data_And_Get_Position;
    Printer(List);
    Put_Line("");
    Test_Empty_List;
    Test_Append_Multiple;
    Printer(List);
    Put_Line("");
    Test_Pop_Multiple;
    Test_Is_Empty;
    Put_Line("Fin des tests de linked_list. Les tests sont passés.");
end test_linked_list;