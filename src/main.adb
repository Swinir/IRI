with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type Instru_Type is (GOTO_I, I_IF, END_I);

begin
    Put(Instru_Type'Image(GOTO_I));
    New_Line;
   Put_Line("Hello, World!");
end Main;