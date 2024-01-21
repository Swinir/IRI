with Ada.Text_IO; use Ada.Text_IO;
with Reader; use Reader;

procedure test_reader is

   -- Test the Reader procedures
   procedure Test_Open_File is
      Handle : File_Handle;
   begin
      Open_File(Path => "test.txt", Handle => Handle);
      Close_File(Handle);
   end Test_Open_File;

   procedure Test_Read_Line is
      Handle : File_Handle;
      File_Line : String(1 .. 500);
   begin
      Open_File(Path => "test.txt", Handle => Handle);
      File_Line := Read_Line(Handle => Handle);
      pragma Assert(File_Line = "first line\n"); -- A définir
      Close_File(Handle);
   end Test_Read_Line;

   procedure Test_Read_Entire_File is
      Handle : File_Handle;
      File_Content_String : String (1 .. 100000);
   begin
      Open_File(Path => "test.txt", Handle => Handle);
      File_Content_String := Read_Entire_File(Handle => Handle);
      pragma Assert(File_Content_String = "test\n test"); -- A définir
      Close_File(Handle);
   end Test_Read_Entire_File;

   procedure Test_Get_Lines is
      Handle : File_Handle;
      Content_List : Reader.File_Content_List;
   begin
      Open_File(Path => "test.txt", Handle => Handle);
      Content_List := Get_Lines(Handle);
      pragma Assert(Length(Content_List) > 0);
      Close_File(Handle);
   end Test_Get_Lines;

begin
   Put_Line("Testing Reader...");
   Test_Open_File;
   --Test_Read_Line;
   Test_Read_Entire_File;
   Test_Get_Lines;
   Put_Line("Reader test successful!");
end test_reader;