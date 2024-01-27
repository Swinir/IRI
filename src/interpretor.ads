with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Memory;
with Register;
with Evaluator;
with Lexer;
with Reader;

package Interpretor is

   type T_Interpretor is private;
      
   -- Initialisation
   --
   -- Cette procédure initialise les variables du registre et du compteur de programme.
   procedure Init(Path : in String; Interpreteur : out T_Interpretor);

   procedure Read_File_Content(Path : in String; Interpreteur : out T_Interpretor);

   -- Getter pour T_IR
   --
   -- Cette fonction renvoie une représentation en chaîne de caractères du registre d'instruction.
   function Get_IR_Value(IR : in Memory.T_Instructions) return Unbounded_String;

   function Get_PC(Interpreteur : in T_Interpretor) return Integer;

   function Get_IR(Interpreteur : in T_Interpretor) return Memory.T_Instructions;

   function Get_Registre(Interpreteur : in T_Interpretor) return Register.Register_Type;

   function Get_Memory(Interpreteur : in T_Interpretor) return Memory.T_Memory;


   -- Incrémentation du compteur de programme
   --
   -- Cette procédure incrémente le compteur de programme.
   -- Nécessite :
   --      Le compteur de programme doit être supérieur ou égal à 0.
   -- Assure :
   --      Le compteur de programme est supérieur à sa valeur précédente.
   procedure Increment_PC(Interpreteur : in out T_Interpretor);
   --    Pre => Get_PC(PC) >= 0,
   --    Post => Get_PC(PC) > Get_PC(PC'Old);

   -- Affichage des informations
   --
   -- Cette procédure affiche les informations de la mémoire et du compteur de programme.
   procedure Display_Infos(Interpreteur : in T_Interpretor);

   procedure Display_Single_Info(Interpreteur : in T_Interpretor);

   -- Interprétation d'une instruction
   --
   -- Cette procédure évalue et exécute une seule instruction.
    procedure Interpret_Single_Instruction(Interpreteur : in out T_Interpretor);

   -- Interprétation d'un programme
   --
   -- Cette procédure interprète un programme en langage intermédiaire complet.
   -- Elle lit le programme et récupère son contenu (Reader), réalise la découpe du programme en chaîne de caractères directement vers des instructions (Lexer), et évalue et exécute toutes les instructions (Evaluator).
   procedure Interpret_All(Interpreteur : in out T_Interpretor);

private

   type T_Interpretor is record
      PC : Integer;
      Memoire : Memory.T_Memory;
      Registre : Register.Register_Type;
      IR : Memory.T_Instructions;
   end record;

end Interpretor;
