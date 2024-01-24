with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Memory;
with Register;
with Common_Types; use Common_Types;

package Evaluator is

-- Évalue l'expression en mémoire et exécute l'instruction correspondante.
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--
-- Assure :
--      La mémoire et le registre sont mis à jour selon l'instruction.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
--      PC (in out): Le compteur de programme.
procedure Evaluate_And_Execute(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type; PC : in out Integer) with
    Pre => IR.Token1 /= To_Unbounded_String(""),
    Post => Register.Length(Registre) > 0;


procedure Init_Variable(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type);

procedure Init_Label(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type);

--
-- Affectation par valeur
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--
-- Assure :
--      La mémoire et le registre sont mis à jour selon l'instruction.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Assign_Value(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= To_Unbounded_String(""),
    Post => Register.Length(Registre) > 0;

-- Cas d’une affectation avec opération
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--
-- Assure :
--      La mémoire et le registre sont mis à jour selon l'instruction.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Assign_With_Operation(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= To_Unbounded_String(""),
    Post => Register.Length(Registre) > 0;

-- Cas d’un branchement conditionnel
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--
-- Assure :
--      La mémoire et le registre sont mis à jour selon l'instruction.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
--      PC (in out): Le compteur de programme.
procedure Conditional_Branch(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type; PC : in out Integer) with
    Pre => IR.Token1 /= To_Unbounded_String(""),
    Post => Register.Length(Registre) > 0;

-- Cas d’un branchement inconditionnel
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--
-- Assure :
--      La mémoire et le registre sont mis à jour selon l'instruction.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
--      PC (in out): Le compteur de programme.
procedure Unconditional_Branch(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type; PC : in out Integer) with
    Pre => IR.Token1 /= To_Unbounded_String(""),
    Post => Register.Length(Registre) > 0;

-- Cas d’une lecture
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--
-- Assure :
--      La mémoire et le registre sont mis à jour selon l'instruction.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Read_Variable(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= To_Unbounded_String(""),
    Post => Register.Length(Registre) > 0;

-- Cas d’écriture
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--
-- Assure :
--      La mémoire et le registre sont mis à jour selon l'instruction.
--
-- Paramètres :
--      IR (in) : L'instruction à évaluer et exécuter.
--      Registre (in out): Le registre à mettre à jour.
procedure Write_Variable(IR : in Memory.T_Instructions; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= To_Unbounded_String(""),
    Post => Register.Length(Registre) > 0;

-- Cas Null
--
-- Cette procédure est appelée lorsqu'une instruction null est rencontrée.
--
-- Nécessite :
--      Le registre d'instruction (IR) ne doit pas être null.
--
-- Assure :
--      Aucune action n'est effectuée.
--
-- Paramètres :
--      IR (in) : L'instruction null à traiter.
procedure Null_Operation(IR : in Memory.T_Instructions) with
    Pre => IR.Token1 /= To_Unbounded_String("");

end Evaluator;
