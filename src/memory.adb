with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with LinkedList;

package body Memory is
    
    procedure Put(Instructions : in T_Instructions) is

    begin
        Put_line("--- Printing memory content ---");
        Put("Token 1 : ");
        Put(Instructions.Token1);
        Put("  ---  Token 2 : ");
        Put(Instructions.Token2);
        Put("  ---  Token 3 : ");
        Put(Instructions.Token3);
        Put("  ---  Token 4 : ");
        Put(Instructions.Token4);
        Put_line("-------------------------------");
    end Put;

end Memory;