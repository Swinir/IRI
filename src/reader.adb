with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Common_Types; use Common_Types;
package body Reader is

    -- Procédure pour ouvrir un fichier
    procedure Open_File(Path : in String; Handle : out File_Handle) is
    begin
        Open(File => Handle, Mode => Ada.Text_IO.In_File, Name => Path); -- Ouvrir le fichier 
    end Open_File;


    -- Procédure pour fermer un fichier
    procedure Close_File(Handle : in out File_Handle) is
    begin
        Close(File => Handle); -- Fermer le fichier
    end Close_File;
    

    -- Fonction pour lire une ligne d'un fichier
    function Read_Line(Handle : in File_Handle) return Unbounded_String is
        Line_Buffer : Unbounded_String;  -- Buffer pour stocker la ligne lue
    begin
        Get_Line(Handle,Line_Buffer); -- Lit une ligne du fichier
        return Line_Buffer; -- Retourne la ligne lue
    end Read_Line;


    -- Fonction pour lire l'intégralité d'un fichier
    function Read_Entire_File(Handle : in out File_Handle) return Unbounded_String is
        File_Content : Unbounded_String;  -- Contenu du fichier
        Line_Buffer : Unbounded_String; -- Buffer pour stocker la ligne lue
    begin
        Reset(Handle); -- Réinitialise le pointeur de fichier au début du fichier
        loop
            exit when End_Of_File(Handle); -- Tant que Buffer pas vide faire : Tant que la fin du fichier n'est pas atteinte
            Get_Line(Handle,Line_Buffer); -- Détecter et extraire la ligne
            File_Content := File_Content & Line_Buffer & " "; -- Ajoute la ligne dans le contenu du fichier
        end loop;
        return File_Content;
    end Read_Entire_File;



    -- Fonction pour obtenir toutes les lignes d'un fichier
    function Get_Lines(Handle : in File_Handle) return File_Content_List is
        Line_List : File_Content_List; -- Liste pour stocker les lignes du fichier
        Line_Buffer : Unbounded_String; -- Buffer pour stocker la ligne lue
        Line_Cleaned : Unbounded_String; -- Ligne nettoyée
        
        -- Fonction pour vérifier si un caractère est autorisé
        function IsAllowed(Char : Character) return Boolean is
        begin
            return Ada.Characters.Handling.Is_Alphanumeric(Char) or else -- Si le caractère n’est pas un espace, une tabulation ou tout autre caractère illégal, retourne vrai
            (Char >= Ada.Characters.Latin_1.Space and Char <= Ada.Characters.Latin_1.DEL);
        end IsAllowed;
    begin
        Common_Types.Init(Line_List); -- Initialisation de la liste des lignes
        loop
            Line_Cleaned := S(""); -- Réinitialisation de la ligne nettoyée
            exit when End_Of_File(Handle); -- Tant que la fin du fichier n'est pas atteinte
            Get_Line(Handle,Line_Buffer); -- Lit une ligne du fichier
            for I in 1..Length(Line_Buffer) loop -- Boucle sur chaque caractère de la ligne : Extraire Le caractère courant
                if IsAllowed(Element(Line_Buffer,I)) then  -- Si le caractère est autorisé
                    Append(Line_Cleaned,Element(Line_Buffer, I)); -- Concaténer le caractère courant à la ligne
                end if;
            end loop;
            Common_Types.Append(Line_List,Line_Cleaned); -- Ajoute la ligne nettoyée à la liste des lignes
        end loop;
        return Line_List;
    end Get_Lines;


end Reader;