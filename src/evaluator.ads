with Memory;
with Register;

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
--      Memoire (in out): La mémoire contenant les différentes instructions du programme.
--      Registre (in out): Le registre à mettre à jour.
--      PC (in out): Le compteur de programme.
procedure Evaluate_And_Execute(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type; PC : in out Integer) with
    Pre => IR.Token1 /= ' ', -- TODO : Change to string type
    Post => Memory.Length(Memoire) > 0 and Register.Length(Registre) > 0;

-- Initialisation
--
-- Assure :
--      La mémoire et le registre sont initialisés.
--
-- Paramètres :
--      Memoire (in out): La mémoire à initialiser.
--      Registre (in out): Le registre à initialiser.
procedure Initialize(Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) with
    Post => Memory.Length(Memoire) = 0 and Register.Length(Registre) = 0;

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
--      Memoire (in out): La mémoire contenant les différentes instructions du programme.
--      Registre (in out): Le registre à mettre à jour.
procedure Assign_Value(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= ' ', -- TODO : Change to string type
    Post => Memory.Length(Memoire) > 0 and Register.Length(Registre) > 0;

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
--      Memoire (in out): La mémoire contenant les différentes instructions du programme.
--      Registre (in out): Le registre à mettre à jour.
procedure Assign_With_Operation(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= ' ', -- TODO : Change to string type
    Post => Memory.Length(Memoire) > 0 and Register.Length(Registre) > 0;

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
--      Memoire (in out): La mémoire contenant les différentes instructions du programme.
--      Registre (in out): Le registre à mettre à jour.
--      PC (in out): Le compteur de programme.
procedure Conditional_Branch(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type; PC : in out Integer) with
    Pre => IR.Token1 /= ' ', -- TODO : Change to string type
    Post => Memory.Length(Memoire) > 0 and Register.Length(Registre) > 0;

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
--      Memoire (in out): La mémoire contenant les différentes instructions du programme.
--      Registre (in out): Le registre à mettre à jour.
--      PC (in out): Le compteur de programme.
procedure Unconditional_Branch(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type; PC : in out Integer) with
    Pre => IR.Token1 /= ' ', -- TODO : Change to string type
    Post => Memory.Length(Memoire) > 0 and Register.Length(Registre) > 0;

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
--      Memoire (in out): La mémoire contenant les différentes instructions du programme.
--      Registre (in out): Le registre à mettre à jour.
procedure Read_Variable(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= ' ', -- TODO : Change to string type
    Post => Memory.Length(Memoire) > 0 and Register.Length(Registre) > 0;

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
--      Memoire (in out): La mémoire contenant les différentes instructions du programme.
--      Registre (in out): Le registre à mettre à jour.
procedure Write_Variable(IR : in Memory.T_Instructions; Memoire : in out Memory.T_Memory; Registre : in out Register.Register_Type) with
    Pre => IR.Token1 /= ' ', -- TODO : Change to string type
    Post => Memory.Length(Memoire) > 0 and Register.Length(Registre) > 0;

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
    Pre => IR.Token1 /= ' '; -- TODO : Change to string type

end Evaluator;
