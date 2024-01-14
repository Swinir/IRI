-- test_linked_list.adb
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
        Pop(List, Element);
        pragma Assert(Is_Empty(List));
        pragma Assert(Length(List) = 0);
    end Test_Pop;

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
    end Test_Get_Data_And_Get_Position;

    procedure Test_Empty_List is
    begin
        pragma Assert(Is_Empty(List));
        pragma Assert(Length(List) = 0);
        pragma Assert(Get_Position(List, Element) = 0);
    end Test_Empty_List;

    procedure Test_Append_Multiple is
    begin
        for C in Character range 'B'..'Z' loop
            Append(List, C);
        end loop;
        pragma Assert(Length(List) = 25);
        pragma Assert(Get_Data(List, 1) = 'A');
        pragma Assert(Get_Data(List, 25) = 'Z');
    end Test_Append_Multiple;

    procedure Test_Pop_Multiple is
    begin
        for C in Character range 'A'..'Z' loop
            Pop(List, Element);
        end loop;
        pragma Assert(Is_Empty(List));
        pragma Assert(Length(List) = 0);
    end Test_Pop_Multiple;

    procedure Test_Clear_Empty is
    begin
        Clear(List);
        pragma Assert(Is_Empty(List));
    end Test_Clear_Empty;

begin
    Put_Line("Début des tests de linked_list");
    Test_Init;
    Test_Append;
    Test_Pop;
    Test_Clear;
    Test_Get_Data_And_Get_Position;
    Printer(List);
    Put_Line("");
    Test_Empty_List;
    Test_Append_Multiple;
    Printer(List);
    Put_Line("");
    Test_Pop_Multiple;
    Test_Clear_Empty;
    Put_Line("Fin des tests de linked_list. Les tests sont passés.");
end test_linked_list;