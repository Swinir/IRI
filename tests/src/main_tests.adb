with Ada.Text_IO;
--with test_reader;
with test_linked_list;

procedure main_tests is
begin
    Ada.Text_IO.Put_Line("Test Program");

    --test_reader;

    test_linked_list;

    Ada.Text_IO.Put_Line("End of Test Program");
end main_tests;
