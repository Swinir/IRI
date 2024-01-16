with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Common_Types;

package Reader is

   type File_Handle is new File_Type;
   -- Définition d'une liste chaîné contenant des chaînes de caractères.
   subtype File_Content_List is Common_Types.String_List;
   
   -- Ouvre un fichier texte pour la lecture.
   --
   -- Nescessite :
   --     Le chemin du fichier doit être valide.
   --
   -- Assure :
   --     Le fichier est ouvert et prêt à être lu.
   --
   -- Paramètres :
   --     Path : le chemin vers le fichier à ouvrir
   --     Handle : le gestionnaire de fichier à utiliser pour les opérations de lecture
   --
   -- Exception :
   --    File_Open_Error : File_Exists(Path) = False ou problème d'allocation mémoire
   procedure Open_File(Path : in String; Handle : out File_Handle) with 
      Pre  => Path'Length > 0,
      Post => Is_Open(Handle) = True;


   -- Ferme le fichier ouvert.
   --
   -- Nescessite :
   --     Le fichier doit être ouvert.
   --
   -- Assure :
   --     Le fichier est fermé.
   --
   -- Paramètres :
   --     Handle : le gestionnaire de fichier du fichier à fermer
   --
   -- Exception :
   --    File_Close_Error : Ficher non ouvert ou fichier déjà fermer ou problème d'allocation mémoire
   procedure Close_File(Handle : in out File_Handle) with
      Pre  => Is_Open(Handle) = True,
      Post => Is_Open(Handle) = False;


   -- Lit une ligne depuis le fichier ouvert.
   --
   -- Nescessite :
   --     Le fichier doit être ouvert.
   --
   -- Assure :
   --     Une ligne du fichier a été lue et renvoyée.
   --
   -- Paramètres :
   --     Handle : le gestionnaire de fichier du fichier à lire
   --
   -- Renvoie :
   --     La ligne lue depuis le fichier
   --
   -- Exception : 
   --    File_Read_Error : Is_Open(Handle) = False 
   function Read_Line(Handle : in File_Handle) return String with
      Pre  => Is_Open(Handle) = True,
      Post => Read_Line'Result'Length >= 0;


   -- Lit l'intégralité du fichier ouvert.
   --
   -- Nescessite :
   --     Le fichier doit être ouvert.
   --
   -- Assure :
   --     Le contenu entier du fichier a été lu et renvoyé.
   --
   -- Paramètres :
   --     Handle : le gestionnaire de fichier du fichier à lire
   --
   -- Renvoie :
   --     Le contenu entier du fichier sous forme de chaîne de caractères
   --
   -- Exception : 
   --    File_Read_Error : Is_Open(Handle) = False 
   function Read_Entire_File(Handle : in File_Handle) return String with
      Pre  => Is_Open(Handle) = True,
      Post => Read_Entire_File'Result'Length >= 0;


   -- Renvoie une liste chaînée contenant toutes les lignes du fichier ouvert.
   --
   -- Cette fonction utilise le type `Linked_List` pour stocker chaque ligne du fichier comme un élément distinct dans la liste.
   -- Chaque élément de la liste est une chaîne de caractères représentant une ligne du fichier.
   --
   -- Nescessite :
   --     Le fichier doit être ouvert.
   --
   -- Assure :
   --     Une liste chaînée contenant toutes les lignes du fichier a été créée et renvoyée.
   --
   -- Renvoie :
   --     Une liste chaînée de chaînes de caractères, chaque élément étant une ligne du fichier
   --
   -- Exception : 
   --    File_Read_Error : Is_Open(Handle) = False 
   function Get_Lines(Handle : in File_Handle) return File_Content_List with
      Pre  => Is_Open(Handle) = True,
      Post => Common_Types.Length(Get_Lines'Result) >= 0;

   -- Exceptions pour gérer les erreurs de fichier
   File_Open_Error : exception;
   File_Read_Error : exception;
   File_Close_Error : exception;

end Reader;



