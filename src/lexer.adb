with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;


package body Lexer is

    -- Fonction pour diviser une chaîne en mots en fonction d'un caractère séparateur
    function Split(Source : in String; Separator : in Character) return Words_Array_T is
        Words : Words_Array_T; -- Tableau pour stocker les mots
        Word : Unbounded_String := To_Unbounded_String(""); -- Mot actuel
        Index : Positive := Source'First; -- Index pour parcourir la chaîne source
        Count : Positive := 1; -- Compteur pour le nombre de mots
    begin
        while Index <= Source'Last and Count <= 50 loop
            if Source(Index) /= Separator then -- Si le caractère actuel n'est pas le séparateur, il est ajouté au mot actuel
                Append(Word, Source(Index));
            else
                -- Si le mot actuel n'est pas vide, il est ajouté au tableau de mots
                if Length(Word) > 0 then
                    Words(Count) := Word;
                    Word := To_Unbounded_String("");
                    Count := Count + 1;
                end if;
            end if;
            Index := Index + 1;
        end loop;
        -- Si le dernier mot n'est pas vide, il est ajouté au tableau de mots
        if Length(Word) > 0 and Count <= 50 then
            Words(Count) := Word;
        end if;
        return Words;
    end Split;


    function Extraire_Mots(Ligne : in Unbounded_String) return T_Words_List is
        Words_Array : Words_Array_T; -- Tableau de mots pour stocker les mots
        Words : T_Words_List; -- Liste chainée pour stocker les mots
        Index : Integer := 1; -- Index pour parcourir le tableau de mots
    begin
        Common_Types.Init(Words); -- Initialisation de la liste chainée
        Words_Array := Split(Source => To_String(Ligne), Separator => ' '); -- Division de la ligne en mots à l'aide de la fonction split
        for Index in Words_Array'Range loop -- Boucle pour ajouter les mots non vides à la liste de mots
            if Words_Array(Index) /= To_Unbounded_String("") then
                Common_Types.Append(Words, Words_Array(Index));
            end if;
        end loop;
        return Words;
    end Extraire_Mots;


    -- Procédure pour traiter les mots-clés
    procedure Process_Keywords(Mots : in out T_Words_List; Index : in out Integer; Instructions : out Memory.T_Instructions; Memoire : in out Memory.T_Memory; Nb_Declarations : in out Integer; Nb_Labels : in out Integer) is
        Word : Unbounded_String; -- Mot actuel
        Debut_Instruction : Memory.T_Instructions; -- Instruction qui va contenir le token BEGIN début
    begin
        Debut_Instruction.Token1 := To_Unbounded_String("BEGIN"); -- Initialisation de l'instruction de début
        Word := Common_Types.Get_Data(Mots, 1); -- Obtention du premier mot
        if Element(Word, 1) = 'L' then -- Si le premier caractère du mot est 'L', le mot est traité comme un label
            Process_Label(Mots, Index, Memoire, Nb_Labels, Nb_Declarations);
            Common_Types.Pop(Mots, 1); -- supprimer le premier mot clé
            Word := Common_Types.Get_Data(Mots, 1); -- Rester à la ligne courante et lire le nouveau premier mot clé
        end if;
        -- Si le mot est "IF", "GOTO" ou "NULL", ... le bon sous programme est appelé
        if Word = To_Unbounded_String("IF") then
            Process_If(Mots, Instructions);
        elsif Word = To_Unbounded_String("GOTO") then -- Si le premier mot clé est "GOTO" alors
            Process_Goto(Mots, Instructions); -- Enregistrer les mots clés de GOTO (instru : in out instruction, mots : in Mots_clés)
        elsif Word = To_Unbounded_String("NULL") then -- Si le premier mot clé est “NULL”
            Process_Null(Mots, Instructions); -- Enregistrer le premier mot clé dans le 1er token (instru : in out instruction, mots : in Mots_clés)
        elsif Ada.Strings.Fixed.Index(To_String(Word), "Ecrire") > 0 then -- Si le mot clé “Ecrire” est trouvé
            Process_Write(Mots, Instructions);
        elsif Ada.Strings.Fixed.Index(To_String(Word), "Lire") > 0 then -- Si le mot clé “Lire” est trouvé
            Process_Read(Mots, Instructions);
        elsif Word = To_Unbounded_String("Programme") then -- Si le premier mot clé est “Programme” 
            Process_Function(Mots, Instructions);
        elsif Word = To_Unbounded_String("Debut") then -- Si le mot clé “Début” est trouvé dans la liste de mots 
            Process_Begin_Variable(Mots, Instructions); -- Enregistrer “BEGIN” dans le 1er token
        elsif Word = To_Unbounded_String("Fin") then -- Si le premier mot clé est “Fin” 
            Process_End_Variable(Mots, Instructions); -- Enregistrer “END” dans le 1er token
        elsif Common_Types.Length(Mots) >= 2 and Memory.Get_Position(Memoire, Debut_Instruction) = - 1 then 
        -- Si l'instruction début n'est pas présente dans la mémoire alors on considere que nous sommes dans le cas d'initialisation de variables
            Process_Var_Init(Mots, Memoire, Nb_Declarations);
        elsif Common_Types.Length(Mots) >= 2 and Memory.Get_Position(Memoire, Debut_Instruction) /= - 1 then
        -- Si l'instruction début est présente dans la mémoire alors on considere que nous sommes dans le cas d'affectation de valeur aux variables
            if Common_Types.Get_Data(Mots, 2) = To_Unbounded_String("<-") then -- Si le deuxième mot est une fleche d'affectation (Premier_Mot : in String, out :booléen)
                Process_Value_Variable(Mots, Instructions); -- Enregistrer les mots clés d’une valeur ou variable 
            end if;
        else
            Ada.Text_IO.Put_Line("Uhoh program crashed cause keyword is not reconnized. Keyword in question : "); -- Message d'erreur si le mot-clé n'est pas reconnu
            Ada.Strings.Unbounded.Text_IO.Put(Word); -- Affichage du mot-clé non reconnu
        end if;
    end Process_Keywords;
    

    -- Procédure pour enregistrer les instructions en mémoire
    procedure Enregistrer_Instructions(Instructions : in Memory.T_Instructions; Memoire : in out Memory.T_Memory) is
    begin
        Memory.Append(Memoire, Instructions); -- Ajouter l'instructions à la mémoire
    end Enregistrer_Instructions;


    -- Procédure pour traiter l'instruction PROGRAM
    procedure Process_Function(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String("PROGRAM"); -- Enregistrer “PROGRAM” dans le 1er token
    end Process_Function;


    -- Procédure pour traiter l'instruction GOTO
    procedure Process_Goto (Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String; -- Mot actuel
    begin
        Instructions.Token1 := To_Unbounded_String("GOTO"); -- Enregistrer GOTO dans le premier mot clé 

        Word := Common_Types.Get_Data(Mots, 2); -- Lire le deuxième mot clé 
        Instructions.Token2 := Word; -- Enregistrer le deuxième mot clé
    end Process_Goto;


    -- Procédure pour traiter l'instruction IF
    procedure Process_If(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String; -- Mot actuel
    begin
        Word := Common_Types.Get_Data(Mots, 1);  -- Lire le premier mot clé 
        Instructions.Token1 := Word; -- Enregistrer le premier mot clé dans le 1er token

        Word := Common_Types.Get_Data(Mots, 2); -- Lire le deuxième mot clé 
        Instructions.Token2 := Word; -- Enregistrer le deuxième mot clé dans le 2ème token

        Word := Common_Types.Get_Data(Mots, 3); -- Lire le troisième mot clé
        Instructions.Token3 := Word; -- Enregistrer le troisième mot clé dans le 3ème token

        Word := Common_Types.Get_Data(Mots, 4); -- Lire le quatrième mot clé
        Instructions.Token4 := Word; -- Enregistrer le quatrième mot clé dans le 4ème token
    end Process_If;

     -- Procédure pour traiter les étiquettes
    procedure Process_Label(Mots : in T_Words_List; Index : in Integer; Memoire : in out Memory.T_Memory; Nb_Labels : in out Integer; Nb_Declarations : in Integer) is
        Word : Unbounded_String; -- Mot actuel
        Old_Instruction : Memory.T_Instructions; -- Ancienne instruction
        New_Instruction : Memory.T_Instructions; -- Nouvelle instruction
        Instruction : Memory.T_Instructions; -- Instruction actuelle
        Line_nb : Integer; -- Numéro de ligne actuel
        Old_line_nb : Integer; -- Ancien numéro de ligne
    begin
        for Index_MEM in 1..Memory.Length(Memoire) loop -- Boucle pour parcourir la mémoire
            Old_Instruction := Memory.Get_Data(Memoire, Index_MEM);
            if Old_Instruction.Token1 = To_Unbounded_String("LABEL") then -- Si le premier token de l'instruction est "LABEL"
                New_Instruction.Token1 := Old_Instruction.Token1; -- Récupération du token "LABEL"
                New_Instruction.Token2 := Old_Instruction.Token2; -- Récupération du token contenant le nom du label
                Old_line_nb := Integer'Value(To_String (Old_Instruction.Token3)) + 1; -- Ajout de un à l'ancien numéro de ligne puisque une ligne va être rajouté à la mémoire
                New_Instruction.Token3 := To_Unbounded_String(Trim(Integer'Image(Old_line_nb), Ada.Strings.Left)); -- Sauverarde du numéro de ligne dans le token 3
                Memory.Edit_Data(Memoire, Index_MEM, New_Instruction); -- Modification de l'instruction dans la mémoire
            end if;
        end loop;
        Nb_Labels := Nb_Labels + 1;
        Instruction.Token1 := To_Unbounded_String("LABEL"); -- Enregistrer “LABEL” dans le 1er token
        Word := Common_Types.Get_Data(Mots, 1); -- Lire le premier mot clé
        Instruction.Token2 := Word; -- Enregistrer le premier mot clé dans le 2ème token

        Line_nb := Index + Nb_Labels + (Nb_Declarations - 1); -- Déterminer la valeur du numéro de ligne actuel
        Instruction.Token3 := To_Unbounded_String(Trim(Integer'Image(Line_nb), Ada.Strings.Left));  -- Enregistrer le numéro de ligne actuel dans le 2ème token (instru : in out instruction)

        Memory.Insert_Beginning(Memoire, Instruction); -- Ajouter au début de la mémoire
    end Process_Label;


    -- Procédure pour traiter l'affectation de valeur à une variable
    procedure Process_Value_Variable(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String; -- Mot actuel
    begin
        Word := Common_Types.Get_Data(Mots, 1); -- Lecture du premier mot clé
        Instructions.Token1 := Word; -- Enregistrer le premier mot clé dans le 1er token

        Word := Common_Types.Get_Data(Mots, 3); -- Lire le troisième mot clé 
        Instructions.Token2 := Word; -- Enregistrer le troisième mot clé dans le 2ieme token 

        if Common_Types.Length(Mots) = 5 then -- Si il existe un 4ème et un 5ème mot clé

            Word := Common_Types.Get_Data(Mots, 4); -- Lire le quatrième mot clé
            Instructions.Token3 := Word; -- Enregistrer le quatrième mot clé dans le 3ieme token

            Word := Common_Types.Get_Data(Mots, 5); -- Lire le cinquième mot clé
            Instructions.Token4 := Word; -- Enregistrer le cinquième mot clé dans le 4ieme token
        end if;
    end Process_Value_Variable;


    -- Fonction pour inverser une chaîne de caractères
    function Reverse_String(Str : in Unbounded_String) return Unbounded_String is
        Result : Unbounded_String := To_Unbounded_String(""); -- Résultat initialisé à une chaîne vide
    begin
        for I in reverse 1 .. Length(Str) loop -- Boucle à travers la chaîne en sens inverse
            Result := Result & Element(Str, I); -- Ajout de chaque caractère à la fin de résultat
        end loop;
        return Result;
    end Reverse_String;


    -- Procédure pour traiter l'instruction READ
    procedure Process_Read(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String; -- Mot actuel
        Open_Paren_Pos : Integer; -- Position de la première parenthèse ouvrante
        Close_Paren_Pos : Integer; -- Position de la dernière parenthèse fermante
    begin
        Instructions.Token1 := To_Unbounded_String("READ"); -- Initialisation du premier token avec "READ"

        Word := Common_Types.Get_Data(Mots, 1); -- Lecture du premier mot clé
        Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Word), "("); -- Trouver la position de la première parenthèse ouvrante
        Close_Paren_Pos := To_String(Word)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Word)), ")"); -- Trouver la position de la dernière parenthèse fermante

        Instructions.Token2 := To_Unbounded_String(Slice(Word, Open_Paren_Pos + 1, Close_Paren_Pos - 1)); -- Extraction de la sous-chaîne entre les parenthèses et enregistre le résultat dans le deuxième token

    end Process_Read;


    -- Procédure pour traiter l'instruction WRITE
    procedure Process_Write(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
        Word : Unbounded_String; -- Mot actuel
        Open_Paren_Pos : Integer; -- Position de la première parenthèse ouvrante
        Close_Paren_Pos : Integer; -- Position de la dernière parenthèse fermante
    begin
        Instructions.Token1 := To_Unbounded_String("WRITE"); -- Initialisation du premier token avec "WRITE"

        Word := Common_Types.Get_Data(Mots, 1); -- Lecture du premier mot clé
        Open_Paren_Pos := Ada.Strings.Fixed.Index(To_String(Word), "("); -- Trouver la position de la première parenthèse ouvrante
        Close_Paren_Pos := To_String(Word)'Length - Ada.Strings.Fixed.Index(To_String(Reverse_String(Word)), ")"); -- Trouver la position de la dernière parenthèse fermante

        Instructions.Token2 := To_Unbounded_String(Slice(Word, Open_Paren_Pos + 1, Close_Paren_Pos)); -- Extraction de la sous-chaîne entre les parenthèses et enregistre le résultat dans le deuxième token
    end Process_Write;


    -- Procédure pour traiter l'instruction BEGIN
    procedure Process_Begin_Variable(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String("BEGIN"); -- Initialisation du premier token avec "BEGIN"
    end Process_Begin_Variable;


    -- Procédure pour traiter l'instruction END
    procedure Process_End_Variable(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String("END"); -- Initialisation du premier token avec "END"
    end Process_End_Variable;


    -- Procédure pour traiter l'instruction NULL
    procedure Process_Null(Mots : in T_Words_List; Instructions : out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String("NULL");
    end Process_Null;


    -- Procédure pour traiter l'initialisation de variables
    procedure Process_Var_Init(Mots : in T_Words_List; Memoire : in out Memory.T_Memory; Nb_Declarations : in out Integer) is
        Instruction_Array : Instruction_Array_T; -- Tableau d'instructions
        Temp_Instruction : Memory.T_Instructions; -- Instruction temporaire
        Word : Unbounded_String := To_Unbounded_String(""); -- Mot actuel
        Nb_Words : Integer; -- Nombre de mots
        Open_Paren_Pos : Integer; -- Position de la première parenthèse ouvrante
        Close_Paren_Pos : Integer; -- Position de la dernière parenthèse fermante
        Value : Integer; -- Valeur de la taille du tableau à créer
    begin
        Nb_Words := Common_Types.Length(Mots) - 1; -- Calcul du nombre de mots
        if Common_Types.Get_Data(Mots, 3) = To_Unbounded_String("Tableau") then -- Si le troisième mot clé est Tableau
            Temp_Instruction.Token1 := To_Unbounded_String("INIT"); -- Initialisation du premier token avec "INIT"
            Temp_Instruction.Token2 := Common_Types.Get_Data(Mots, 1); -- Initialisation du deuxième token avec le premier mot

            Word := Common_Types.Get_Data(Mots, 4); -- Lecture du quatrième mot
            Open_Paren_Pos := Index(Word, "("); -- Trouver la position de la première parenthèse ouvrante
            Close_Paren_Pos := Index(Word, ")"); -- Trouver la position de la dernière parenthèse fermante

            Word := To_Unbounded_String(Slice(Word, Open_Paren_Pos + 1, Close_Paren_Pos - 1)); -- Extraction de la sous-chaîne entre les parenthèses et enregistrement dans le mot
            Value := Integer'Value(To_String(Word)); -- Conversion de la sous-chaîne en entier
            Temp_Instruction.Token4 := To_Unbounded_String("TAB:") & Trim(Integer'Image(Value), Ada.Strings.Left); -- Initialisation du quatrième token avec "TAB:" et la valeur convertie en chaîne
            Temp_Instruction.Token3 := Common_Types.Get_Data(Mots, 6); -- Initialisation du troisième token avec le sixième mot
            Enregistrer_Instructions(Temp_Instruction, Memoire); -- Enregistrement des instructions dans la mémoire

        else -- Si le troisième mot clé n'est pas Tableau
            for Index in 1..Nb_Words loop   
                Word := Common_Types.Get_Data(Mots, Index); -- Lecture du mot à l'index courant
                if Word /= ":" then -- Si le mot n'est pas ":"
                    Nb_Declarations := Nb_Declarations + 1; -- Incrémentation du nombre de déclarations
                    Temp_Instruction.Token1 := To_Unbounded_String("INIT"); -- Enregistrer “INIT” dans le 1er token 
                    Word := Common_Types.Get_Data(Mots, Index); -- Lecture du mot à l'index courant
                    if element(Word, length (Word)) = ',' then -- Si le dernier caractère du mot est ","
                        Word := To_Unbounded_String(Ada.Strings.Unbounded.Slice(Word, 1, Ada.Strings.Unbounded.Length(Word) - 1)); -- Suppression de la virgule
                    end if; 
                    Temp_Instruction.Token2 := Word; -- Enregistrer le premier mot clé dans le 2ème token (nom de la variable)
                    -- Le type sera défini plus tard (token 3)

                    Instruction_Array(Index) := Temp_Instruction; -- Enregistrement de l'instruction temporaire dans le tableau d'instructions à l'index courant

                else -- Si le mot est ":"
                    Word := Common_Types.Get_Data(Mots, Index + 1); -- Lecture du mot suivant
                    for Index in Instruction_Array'Range loop -- Boucle sur toutes les instructions dans le tableau d'instructions
                        if Instruction_Array(Index).Token1 /= To_Unbounded_String("") then -- Si le premier token de l'instruction à l'index courant n'est pas vide
                            Instruction_Array(Index).Token3 := Word; -- Initialisation du troisième token avec le type de la variable
                        end if;
                    end loop;
                end if;
            end loop;
            for Index in Instruction_Array'Range loop -- Boucle sur toutes les instructions dans le tableau d'instructions
                if Instruction_Array(Index).Token1 /= To_Unbounded_String("") then -- Si le premier token de l'instruction à l'index courant n'est pas vide
                    Enregistrer_Instructions(Instruction_Array(Index), Memoire); -- Enregistrement de l'instruction dans la mémoire
                end if;
            end loop;
        end if;
    end Process_Var_Init;


    -- Procédure pour vider les instructions
    procedure Clear_Instructions(Instructions : in out Memory.T_Instructions) is
    begin
        Instructions.Token1 := To_Unbounded_String(""); -- Réinitialisation du premier token
        Instructions.Token2 := To_Unbounded_String(""); -- Réinitialisation du deuxième token
        Instructions.Token3 := To_Unbounded_String(""); -- Réinitialisation du troisième token
        Instructions.Token4 := To_Unbounded_String(""); -- Réinitialisation du quatrième token
    end Clear_Instructions;


    -- Procédure pour analyser les lignes de code
    procedure Analyser_Lignes(Lignes : in Common_Types.String_List; Memoire : in out Memory.T_Memory) is
        Mots : T_Words_List; -- Liste des mots
        Index : Integer := 1; -- Index total
        Index_without_Blanks : Integer := 1; -- Index sans les saut de ligne et les commentaires
        Instructions : Memory.T_Instructions; -- Instructions
        Nb_Declarations : Integer := 0; -- Nombre de déclarations
        Nb_Labels : Integer := 0; -- Nombre de labels
    begin
        while Index <= Common_Types.Length(Lignes) loop -- Boucle tant que l'index est inférieur ou égal au nombre de lignes
            if Ada.Strings.Fixed.Index(To_String(Common_Types.Get_Data(Lignes, Index)), "--") = 0 AND Common_Types.Get_Data(Lignes, Index) /= To_Unbounded_String("") then -- Si la ligne courante ne contient pas de commentaire et n'est pas vide
                Mots := Extraire_Mots(Common_Types.Get_Data(Lignes, Index)); -- Extraction des mots de la ligne courante
                Process_Keywords(Mots, Index_without_Blanks, Instructions, Memoire, Nb_Declarations, Nb_Labels); -- Traitement des mots clés
                if Instructions.Token1 /= To_Unbounded_String("LABEL") and Instructions.Token1 /= To_Unbounded_String("") then -- Si le premier token des instructions n'est pas "LABEL" et n'est pas vide
                    Enregistrer_Instructions(Instructions, Memoire); -- Enregistrement des instructions dans la mémoire
                end if;
                Common_Types.Clear(Mots); -- Nettoyage de la liste des mots
                Clear_Instructions(Instructions);  -- Nettoyage des instructions
                Index_without_Blanks := Index_without_Blanks + 1; -- Incrémentation de l'index sans les blancs
            end if;
            Index := Index + 1;  -- Incrémentation de l'index total
        end loop;
    end Analyser_Lignes;

end lexer;