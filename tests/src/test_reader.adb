with Ada.Text_IO; use Ada.Text_IO;
with Reader; use Reader;
with Common_Types;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure test_reader is

   -- Test the Reader procedures
   procedure Test_Open_File is
      Handle : File_Handle;
   begin
      Put_Line(Ada.Directories.Current_Directory);
      Open_File(Path => "./tests/test.txt", Handle => Handle);
      Close_File(Handle);
   end Test_Open_File;

   procedure Test_Read_Line is
      Handle : File_Handle;
      File_Line : Unbounded_String;
   begin
      Open_File(Path => "./tests/test.txt", Handle => Handle);
      File_Line := Read_Line(Handle => Handle);
      pragma Assert(File_Line = "first line");
      File_Line := Read_Line(Handle => Handle);
      pragma Assert(File_Line = "second line");
      File_Line := Read_Line(Handle => Handle);
      pragma Assert(File_Line = "this line is very very very very very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very very big");
      File_Line := Read_Line(Handle => Handle);
      pragma Assert(File_Line = "fourth line 9U348UJDUHI_P%%1");
      File_Line := Read_Line(Handle => Handle);
      pragma Assert(File_Line = "    5th line with espaces");
      File_Line := Read_Line(Handle => Handle);
      pragma Assert(File_Line = "6th line with espaces       ");
      File_Line := Read_Line(Handle => Handle);
      pragma Assert(File_Line = "    5th line with tab");
      Close_File(Handle);
   end Test_Read_Line;

   procedure Test_Read_Entire_File is
      Handle : File_Handle;
      File_Content_String : Unbounded_String;
      File_Line : Unbounded_String;
   begin
      Open_File(Path => "./tests/test.txt", Handle => Handle);
      File_Line := Read_Line(Handle => Handle);
      pragma Assert(File_Line = "first line");
      File_Content_String := Read_Entire_File(Handle => Handle);
      pragma Assert(File_Content_String = "first line second line this line is very very very very very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very very big fourth line 9U348UJDUHI_P%%1     5th line with espaces 6th line with espaces            5th line with tab ");
      Close_File(Handle);
   end Test_Read_Entire_File;

   procedure Test_Get_Lines is
      Handle : File_Handle;
      Content_List : Reader.File_Content_List;
   begin
      Open_File(Path => "./tests/test.txt", Handle => Handle);
      Content_List := Get_Lines(Handle);
      pragma Assert(Common_Types.Length(Content_List) = 7);
      pragma Assert(Common_Types.Get_Data(Content_List,1) = "first line");
      pragma Assert(Common_Types.Get_Data(Content_List,2) = "second line");
      Put_Line(To_String(Common_Types.Get_Data(Content_List,4)));
      pragma Assert(Common_Types.Get_Data(Content_List,3) = "this line is very very very very very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very veryvery very very big");
      pragma Assert(Common_Types.Get_Data(Content_List,4) = "fourth line 9U348UJDUHI_P%%1");
      pragma Assert(Common_Types.Get_Data(Content_List,5) = "    5th line with espaces");
      pragma Assert(Common_Types.Get_Data(Content_List,6) = "6th line with espaces       ");
      pragma Assert(Common_Types.Get_Data(Content_List,7) = "    5th line with tab");
      Close_File(Handle);
   end Test_Get_Lines;

begin
   Put_Line("Début des tests de Reader...");
   Test_Open_File;
   Test_Read_Line;
   Test_Read_Entire_File;
   Test_Get_Lines;
   Put_Line("Fin des tests de Reader. Les tests sont passés.");
end test_reader;