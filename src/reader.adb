package body Reader is

    procedure Open_File(Path : in String; Handle : out File_Handle) is
    begin
        Open(File => Handle, Mode => Ada.Text_IO.In_File, Name => Path);
    end Open_File;

    procedure Close_File(Handle : in out File_Handle) is
    begin
        Close(File => Handle);
    --  exception
    --      when others =>
    --          Ada.Text_IO.Put_Line("An error occurred while reading the file.");
    end Close_File;
    
    function Read_Line(Handle : in File_Handle) return Unbounded_String is
    begin
    end Read_Line;

    function Read_Entire_File(Handle : in File_Handle) return Unbounded_String is
        File_Content : Unbounded_String;
        Line_Buffer : Unbounded_String;
    begin
        loop
            exit when End_Of_File(Handle);
            Get_Line(Handle,Line_Buffer);
            File_Content := File_Content & Line_Buffer;
        end loop;
        return File_Content;
    end Read_Entire_File;


    function Get_Lines(Handle : in File_Handle) return File_Content_List is
        Line_List : File_Content_List;
        Line_Buffer : Unbounded_String;
    begin
        loop
            exit when End_Of_File(Handle);
            Get_Line(Handle,Line_Buffer);
            Common_Types.Append(Line_List,Line_Buffer);
        end loop;
        return Line_List;
    end Get_Lines;


end Reader;