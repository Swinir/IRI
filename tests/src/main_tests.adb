with Ada.Text_IO;

with test_linked_list;
with test_reader;
with Test_Evaluator;
with test_lexer;

procedure main_tests is
begin
    Ada.Text_IO.Put_Line("----------- Début des tests -----------");


    test_linked_list;
    test_reader;
    Test_Evaluator;
    test_lexer;

    Ada.Text_IO.Put_Line("----------- Fin des tests -----------");
end main_tests;
