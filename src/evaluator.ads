with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Memory;
with Register;
with Common_Types; use Common_Types;

package Evaluator is

-- Évalue l'expression en mémoire et exécute l'instruction correspondante.
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
--      PC (in out): Le compteur de programme.
procedure Evaluate_And_Execute(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type; PC : in out Integer) with
    Pre => IR.Token1 /= To_Unbounded_String("");


--
-- Initialise une variable dans le registre.
--
-- Nécessite :
--      Le premier token du registre d'instruction (IR) doit être égal à INIT.
--
-- Assure :
--      Le registre doit contenir la nouvelle variable.
--      La taille du registre doit avoir augementé de 1.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Init_Variable(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 = S("INIT"),
    Post => Register.Length(Registre) > Register.Length(Registre'Old);


--
-- Initialise une étiquette dans le registre.
--
-- Nécessite :
--      Le premier token du registre d'instruction (IR) doit être égal à LABEL.
--
-- Assure :
--      Le registre doit contenir la nouvelle étiquette.
--      La taille du registre doit avoir augementé de 1
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Init_Label(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 = S("LABEL"),
    Post => Register.Length(Registre) = Register.Length(Registre'Old) + 1 AND Register.Contains_Name(Registre, IR.Token2);

--
-- Affectation par valeur
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--      Le registre doit contenir la variable à assigner.
--
-- Assure :
--      Le registre sont mis à jour selon l'instruction.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Assign_Value(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= To_Unbounded_String("");


-- Cas d’une affectation avec opération
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--      Le registre doit contenir la variable à assigner.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Assign_With_Operation(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= S("");


-- Cas d’un branchement conditionnel
--
-- Nécessite :
--      Le premier token du registre d'instruction (IR) doit être égal à IF.
--      Le troisième token du registre d'instruction (IR) doit être égal à GOTO.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
--      PC (in out): Le compteur de programme.
procedure Conditional_Branch(IR : in Memory.T_Instructions; Registre : in Register.Register_Type; PC : in out Integer) with
    Pre => IR.Token1 = S("IF") AND IR.Token3 = S("GOTO");


-- Cas d’un branchement inconditionnel
--
-- Nécessite :
--      Le premier token du registre d'instruction (IR) doit être égal à GOTO.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
--      PC (in out): Le compteur de programme.
procedure Unconditional_Branch(IR : in Memory.T_Instructions; Registre : in Register.Register_Type; PC : in out Integer) with
    Pre => IR.Token1 = To_Unbounded_String("GOTO");


-- Cas d’une lecture
--
-- Nécessite :
--      Le permier token du registre d'instruction (IR) doit être égal à READ.
--      Le registre doit contenir la variable à lire.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Read_Variable(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 = To_Unbounded_String("READ") AND Register.Contains_Name(Registre, IR.Token2);


-- Cas d’écriture
--
-- Nécessite :
--      Le premier token du registre d'instruction (IR) doit être égal à WRITE.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Write_Variable(IR : in Memory.T_Instructions; Registre : in Register.Register_Type) with
    Pre => IR.Token1 = To_Unbounded_String("WRITE");


-- Cas Null
--
-- Cette procédure est appelée lorsqu'une instruction null est rencontrée.
procedure Null_Operation;


-- Vérifie si la variable à l'intialisation est de type tableau
function Is_Array_Init_Type(IR : in Memory.T_Instructions) return Boolean;

-- Procedure qui initialise le registre de N taille case mémoire pour un tableau, N étant la taille d'un type tableau
procedure Init_Array(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type; Variable_Type  : in Register.T_Types);

-- Fonction qui retourne l'index courant dans le quel un tableau est indéxé, retourne l'index de son adresse en registre.
-- Evalue la variable si l'index est une variable Tab(I) = Evalue la valeur de I et récupère l'adresse dans le registre. 
-- Sinon ex : Tab(2), récupère l'adresse à l'index 2 du tableau.
function Get_Array_Index(Token : In Unbounded_String; Registre : in Register.Register_Type) return Unbounded_String;

-- Vérifie si une variable non présente dans le registre est de type tableau
function Is_Variable_Array(Token : in Unbounded_String) return Boolean;

-- Vérifie si une variable est une chaine de caractère
function Is_String_Type(Token : Unbounded_String) return Boolean;

--
-- Vérifie si la fin du programme a été atteinte.
--
-- Nécessite :
--      Le premier token du registre d'instruction (IR) doit être égal à "END".
--
-- Retourne :
--      Vrai si la fin du programme a été atteinte, faux sinon.
--
-- Paramètres :
--      IR (in) : L'instruction à vérifier.
function Is_End_Of_Programm(IR : in Memory.T_Instructions) return Boolean with
    Pre => IR.Token1 = S("END");


function Is_End_Of_Program(IR : in Memory.T_Instructions) return Boolean;

end Evaluator;
