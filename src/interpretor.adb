with Ada.Text_IO; use Ada.Text_IO;
with Common_Types;

package body Interpretor is

    -- Procédure pour initialiser l'interpréteur
    procedure Init(Path : in String; Interpreteur : out T_Interpretor) is
        Memoire :  Memory.T_Memory; -- Mémoire
        Registre : Register.Register_Type; -- Registre
        IR : Memory.T_Instructions; -- Instruction IR
    begin
        Register.Init(Registre); -- Initialise le registre
        Memory.Init(Memoire); -- Initialise la mémoire
        Interpreteur.Memoire := Memoire; -- Affecte la mémoire à l'interpréteur
        Interpreteur.Registre := Registre; -- Affecte le registre à l'interpréteur
        Interpreteur.PC := 1; -- Initialise le compteur de programme à 1

        Read_File_Content(Path, Interpreteur); -- Lit le contenu du fichier
        Interpreteur.IR := Memory.Get_Data(Interpreteur.Memoire, Interpreteur.PC); -- Obtient les instructions de la mémoire
    end Init;


    -- Procédure pour lire le contenu du fichier
    procedure Read_File_Content(Path : in String; Interpreteur : out T_Interpretor) is
        Handle : Reader.File_Handle; -- Handle du fichier
        File_Content_String : Common_Types.String_List; -- Contenu du fichier sous forme de chaîne
    begin
        Reader.Open_File(Path => Path, Handle => Handle); -- Ouvre le fichier
        File_Content_String := Reader.Get_Lines(Handle => Handle); -- Obtient les lignes du fichier
        Reader.Close_File(Handle); -- Ferme le fichier
        Lexer.Analyser_Lignes(File_Content_String, Interpreteur.Memoire); -- Analyse les lignes du fichier
    end Read_File_Content;

    -- Getter PC
    function Get_PC(Interpreteur : in T_Interpretor) return Integer is
    begin
        return Interpreteur.PC;
    end Get_PC;

    -- Getter IR
    function Get_IR(Interpreteur : in T_Interpretor) return Memory.T_Instructions is
    begin
        return Interpreteur.IR;
    end Get_IR;

    -- Getter Registre
    function Get_Registre(Interpreteur : in T_Interpretor) return Register.Register_Type is 
    begin
        return Interpreteur.Registre;
    end Get_Registre;

    -- Getter Memory
    function Get_Memory(Interpreteur : in T_Interpretor) return Memory.T_Memory is 
    begin
        return Interpreteur.Memoire;
    end Get_Memory;

    -- Fonction pour obtenir la valeur des instructions
    function Get_IR_Value(IR : in Memory.T_Instructions) return Unbounded_String is
    begin
        Return IR.Token1 & IR.Token2 & IR.Token3 & IR.Token4;
    end Get_IR_Value;

    -- Procédure pour incrémenter le compteur de programme
    procedure Increment_PC(Interpreteur : in out T_Interpretor) is
    begin
        Interpreteur.PC := Interpreteur.PC + 1;
    end Increment_PC;

    -- Procédure pour afficher les informations
    procedure Display_Infos(Interpreteur : in T_Interpretor) is
    begin
        Put_Line("");
        Put_Line("--------------------------------- Register Content ---------------------------------");
        Put("-- Registry index value :" & Integer'Image(1) & "  |  ");
        Register.Put(Register.Variable_List.Get_Data(Interpreteur.Registre, 1));
        Put_Line("");
        for Index in 2..Register.Length(Interpreteur.Registre) loop -- Boucle sur le reste des valeurs du registre
            Put_Line("------------------------------------------------");
            Put("-- Registry index value :" & Integer'Image(Index) & "  |  ");
            Register.Put(Register.Variable_List.Get_Data(Interpreteur.Registre, Index));
            Put_Line("");
        end loop;
        Put_Line("----------------------------------------------------------------------------------");
        Put_Line("");
    end Display_Infos;

    -- Procédure pour afficher une seule information
    procedure Display_Single_Info(Interpreteur : in T_Interpretor) is
    begin
        Put_Line("");
        Put("--------------------------------- PC value :");Put(Integer'Image(Interpreteur.PC));Put(" ---------------------------------");
        Put_Line("");
        Put("-- IR Tokens : ");
        Memory.Put(Interpreteur.IR); -- Affiche les tokens des instructions
        Put_Line("");
        Put_Line("----------------------------------------------------------------------------------");
    end Display_Single_Info;


    -- Procédure pour interpréter une seule instruction
    procedure Interpret_Single_Instruction(Interpreteur : in out T_Interpretor) is
    begin
        if Interpreteur.PC <= Memory.Length(Interpreteur.Memoire) then -- Si le compteur de programme est inférieur ou égal à la longueur de la mémoire
            Evaluator.Evaluate_And_Execute(Interpreteur.IR,Interpreteur.Registre, Interpreteur.PC); -- Évalue et exécute les instructions
            Increment_PC(Interpreteur); -- Incrémente le compteur de programme
            Interpreteur.IR := Memory.Get_Data(Interpreteur.Memoire, Interpreteur.PC); -- Met à jour les instructions avec la nouvelle valeur de pc
        end if;
    end Interpret_Single_Instruction;

    -- Procédure pour interpréter toutes les instructions
    procedure Interpret_All(Interpreteur : in out T_Interpretor) is
    begin
        while (Interpreteur.PC <= Memory.Length(Interpreteur.Memoire) and not Evaluator.Is_End_Of_Programm(Interpreteur.IR)) loop -- Tant que le compteur de programme est inférieur ou égal à la longueur de la mémoire et que ce n'est pas la fin du programme
            Interpret_Single_Instruction(Interpreteur); -- Interprète une seule instruction
        end loop;
        Display_Infos(Interpreteur); -- Affiche les informations
    end Interpret_All;
end Interpretor;