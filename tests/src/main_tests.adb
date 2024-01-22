with Ada.Text_IO;
with test_reader;
with test_linked_list;
with test_lexer;
--with Test_Interpreteur;

procedure main_tests is
begin
    Ada.Text_IO.Put_Line("Début des tests...");

    test_reader;

    test_linked_list;

    test_lexer;

    Ada.Text_IO.Put_Line("Tests terminés");
end main_tests;