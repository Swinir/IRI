with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with interpreteur; use interpreteur;

procedure Main is
   Path : Unbounded_String;
begin
   Put_Line("Hello, World!");
   Put_Line("Please input the full path to the file you want to evaluate");
   Ada.Strings.Unbounded.Text_IO.Get_Line(Path);
   interpreteur.Interpret(Path => Path);
end Main;