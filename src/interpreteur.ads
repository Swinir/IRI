with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Memory;
with Register;
with Evaluator;
with Lexer;
with Reader;

package Interpreteur is

   type T_PC is private;
   type T_IR is private;
   type T_Register is private;
   type T_Memory is private;
   
-- Initialisation
--
-- Cette procédure initialise les variables du registre et du compteur de programme.
procedure Init(PC : out T_PC; IR : out T_IR; Registers : out T_Register; Memory : out T_Memory);

-- Getter pour T_PC
--
-- Cette fonction renvoie la valeur du compteur de programme.
function Get_PC(PC : in T_PC) return Integer;

-- Getter pour T_IR
--
-- Cette fonction renvoie une représentation en chaîne de caractères du registre d'instruction.
function Get_IR_Value(IR : in T_IR) return Unbounded_String;

-- Incrémentation du compteur de programme
--
-- Cette procédure incrémente le compteur de programme.
-- Nécessite :
--      Le compteur de programme doit être supérieur ou égal à 0.
-- Assure :
--      Le compteur de programme est supérieur à sa valeur précédente.
procedure Increment_PC(PC : in out T_PC) with
    Pre => Get_PC(PC) >= 0,
    Post => Get_PC(PC) > Get_PC(PC'Old);

-- Affichage des informations
--
-- Cette procédure affiche les informations de la mémoire et du compteur de programme.
procedure Display_Info(Memory : in T_Memory; Registers : in Register.Register_Type; PC : in T_PC);

-- Interprétation d'une instruction
--
-- Cette procédure évalue et exécute une seule instruction.
procedure Interpret_Single_Instruction(IR : in out T_IR; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type; PC : in out T_PC);

-- Interprétation d'un programme
--
-- Cette procédure interprète un programme en langage intermédiaire complet.
-- Elle lit le programme et récupère son contenu (Reader), réalise la découpe du programme en chaîne de caractères directement vers des instructions (Lexer), et évalue et exécute toutes les instructions (Evaluator).
procedure Interpret(Path : in Unbounded_String; IR : in out T_IR; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type; PC : in out T_PC);

private
   type T_PC is new Integer;
   type T_IR is new Memory.T_Instructions;
   type T_Register is new Register.Register_Type;
   type T_Memory is new Memory.T_Memory;
end Interpreteur;
