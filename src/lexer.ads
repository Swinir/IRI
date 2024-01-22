-- Spécification de lexer, module qui découpe les lignes d'un programmes, en un ensemble d'instructions evaluable par evaluator.
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Memory;
with Common_Types;


package Lexer is

    subtype T_Words_List is Common_Types.String_List;

    -- Procédure principe qui analyse et enregistre les mots clés d'un programme en mémoire
    -- Pour chaque ligne dans la liste des lignes, extrait les instructions et les enregistre sous formes de mots clés (Tokens) en mémoire.
    --  ex : Memoire : [ { "INIT", "T1", "INTEGER", "" }, -- initialise une variable avec son nom 
    --                  { "T1", "2", "", "" }, -- affectation d'une valeur à la variable T1
    --                  { "T3", "25", "*", "T1" },  -- affectation d'un calcul avec l'opérateur * de 25 et la variable T1 sur T3
    --                  { "GOTO", "L1", "", "" } ]   -- Jump à la ligne en valeur du label L1
    -- Voir Process Keywords pour le détails des mots clés
    --
    -- Nécessite :
    --      La liste de lignes ne doit pas être vide
    --
    -- Assure :
    --      La mémoire est mise à jour avec les nouvelles instructions
    --
    -- Paramètres :
    --      Lignes (in) : Liste de lignes d'un programmes à traiter
    --      Mémoire (in out): La mémoire contenant les différentes instructions du programme
    procedure Analyser_Lignes(Lignes : in Common_Types.String_List; Memoire : in out Memory.T_Memory) with
        Pre => Common_Types.Length(Lignes) > 0,
        Post => Memory.Length(Memoire) > Common_Types.Length(Lignes);


    -- Fonction qui analyse et récupére chaque sous-chaîne de la ligne délimité par des espaces.
    -- Elle Prend une ligne en entrée, la découpe et renvoie une liste de mots extraits de la ligne.
    --
    -- Nécessite :
    --      La ligne ne doit pas être vide
    --
    -- Ex : Ligne := "T3 <- T1 OR T2" = ["T3", "T1", "OR", "T2"]
    -- Paramètres :
    --      Ligne (in) : La ligne d'instruction à découper
    function Extraire_Mots(Ligne : in String) return T_Words_List with
        Pre => Ligne'Length > 0;


    -- Enregistrer l'instructions en paramètre dans la mémoire 
    --
    -- Nécessite :
    --      Le premier token ne doit pas être vide
    --
    -- Assure :
    --      La mémoire est mise à jour avec les nouvelles instructions
    --
    -- Paramètres :
    --     Instructions (in): L'engistrement qui contiendra les 4 tokens
    --     Mémoire (in out): La mémoire contenant les différentes instructions du programme
    procedure Enregistrer_Instructions(Instructions : in Memory.T_Instructions; Memoire : in out Memory.T_Memory) with
        Pre => Instructions.Token1 /= To_Unbounded_String(""),
        Post => Memory.Length(Memoire) > 0;
    
    -- Cette procédure identifie les mots clés et génére le quadruplet de mots clés (tokens)
    -- Elle prend une liste de mots en entrée et génère le quadruplet de tokens correspondant avec la procédure approprié.
    -- Elle gère les mots clés de l'instruction actuelle des façon suivante :
    -- Ex : ["T3","<-", "T1", "OR", "T2"] = {"T3", "T1", "OR", "T2"}
    --      ["N","<-", "2" ] = {"N", "2", "", ""}
    -- Pour le détails de chaque mots clés (tokens) voir les procédures Process_X ci dessous dédié à gérer chaque mots clés.
    --
    -- Nécessite :
    --      La liste mot n'est pas vide
    --      L'index n'est pas négatif
    --
    -- Assure :
    --      Le premier token n'est pas vide
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Index (in out): Numéro de la ligne courante traiter
    --     Instructions (out): L'engistrement qui contiendra les 4 tokens
    procedure Process_Keywords(Mots : in out T_Words_List; Index : in out Integer; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots) and Index >= 0,
        Post => Instructions.Token1 /= To_Unbounded_String("");


    -- Cette procédure est responsable de la gestion du mot clé PROGRAMME, trouver en début d'un programme.
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Mot clé N.1 : Enregistre le premier mot clé (PROGRAM) en tant que premier token
    --
    -- Ex {"PROGRAM", "", "", ""}
    --
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec le nouveau token
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les 4 tokens
    procedure Process_Function(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");


    -- Cette procédure est responsable de la gestion de l'intialisation des variables présente au début d'un programme.
    -- Si des variables sont trouvé le premier est extrait et supprimer pour ensuite retraiter la suite de l'instruction
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Mot clé N.1 : Enregistre le premier mot clé (INIT) en tant que premier token
    -- Mot clé N.2 : Enregistre le deuxième mot clé (Variable) en tant que deuxième token
    -- Mot clé N.3 : Enregistre le troisième mot clé (Type) en tant que troisième token
    
    -- Ex : n, i : Entier
    --      Instructions :  {"INIT", "N", "INTEGER", ""}, prochaine itération : {"INIT", "I", "INTEGER", ""}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les 4 tokens
    procedure Process_Var_Init(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");


    -- Cette procédure est responsable de la gestion du mot clé GOTO trouvé en première position dans la liste de mots donnée.
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Mot clé N.1 : Enregistre le premier mot clé (GOTO) en tant que premier token 
    -- Mot clé N.2 : Enregistre le deuxième mot clé (Label) en tant que deuxième token 
    --
    -- Ex : {"GOTO", "L1", "", ""}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les 4 tokens
    procedure Process_Goto(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");
        

    -- Cette procédure est responsable de la gestion du mot clé IF trouvé en première position dans la liste de mots donnée.
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Mot clé N.1 : Enregistre le premier mot clé (IF) en tant que premier token
    -- Mot clé N.2 : Enregistre le deuxième mot clé (Variable) en tant que deuxième token
    -- Mot clé N.3 : Enregistre le troisième mot clé (GOTO) en tant que troisième token
    -- Mot clé N.4 : Enregistre le quatrième mot clé (Label) en tant que quatrième token
    --
    -- Ex : {"IF","T1","GOTO","L1"}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les 4 tokens
    procedure Process_If(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");


    -- Cette procédure est responsable de la gestion du mot clé LABEL trouvé en première position dans la liste de mots donnée.
    -- Si un label est trouvé ce dernier est extrait et supprimer pour ensuite retraiter la suite de l'instruction
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Mot clé N.1 : Enregistre le premier mot clé (LABEL) en tant que premier token si 'L' trouvé en début
    -- Mot clé N.3 : Enregistre le troisième mot clé (Nom) en tant que deuxième token
    -- Mot clé N.3 : Enregistre le troisième mot clé (Numéro de ligne) en tant que troisième token
    --
    -- Ex : {"LABEL","L1","5",""}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les 4 tokens
    procedure Process_Label(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");


    -- Cette procédure est responsable de la gestion des mots clés de valeur ou de variable trouvés en première position dans la liste de mots donnée.
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Mot clé N.1 : Enregistre le premier mot clé (Variable) en tant que premier token
    -- Mot clé N.3 : Enregistre le troisième mot clé (Variable ou Valeur) en tant que deuxième token
    -- Mot clé N.4 : Enregistre le quatrième mot clé (Mot logique (OR, AND, ....)) en tant que troisième token
    -- Mot clé N.5 : Enregistre le cinquième mot clé (Variable ou Valeur) en tant que quatrième token
    --
    -- Ex : {"T1","25","",""}, {"T2","T1","",""}, {"T2","I","+","1"}, {"T2","T3","AND","T1"}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les 4 tokens
    procedure Process_Value_Variable(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");


    -- Cette procédure est responsable de la gestion du mot clé "Lire" trouvé en première position dans la liste de mots donnée.
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Si le premier mot clé est "Lire" :
    --     Mot N.1 : Enregistre "READ" en tant que premier token
    --     Mot N.2 : Enregistre le deuxième mot (variable) en tant que deuxième token
    --
    -- Ex : {"READ","T1","",""}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les tokens
    procedure Process_Read(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");

    -- Cette procédure est responsable de la gestion du mot clé "Ecrire" trouvé en première position dans la liste de mots donnée.
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Si le premier mot clé est "Ecrire" :
    --     Mot N.1 : Enregistre "WRITE" en tant que premier token
    --     Mot N.2 : Enregistre le deuxième mot (variable ou valeur) en tant que deuxième token
    --
    -- Ex : {"WRITE","25","",""} , {"WRITE","T1","",""}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les tokens
    procedure Process_Write(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");


    -- Cette procédure est responsable de la gestion du mot clé "Début" trouvé en première position dans la liste de mots donnée.
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Si le premier mot clé est "Début" :
    --     Mot N.1 : Enregistre "BEGIN" en tant que premier token
    --
    -- Ex : {"BEGIN","","",""}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les tokens
    procedure Process_Begin_Variable(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");


    -- Cette procédure est responsable de la gestion du mot clé "Fin" trouvé en première position dans la liste de mots donnée.
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Si le premier mot clé est "Fin" :
    --     Mot N.1 : Enregistre "END" en tant que premier token
    --
    -- Ex : {"END","","",""}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les tokens
    procedure Process_End_Variable(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");


    -- Cette procédure est responsable de la gestion du mot clé NULL trouvé en première position dans la liste de mots donnée.
    -- Elle gère les mots clés de l'instruction actuelle de la façon suivante :
    --
    -- Si le premier mot clé est "NULL" :
    --     Mot N.1 : Enregistre le premier mot clé (NULL) en tant que premier token
    --
    -- Ex : {"NULL","","",""}
    --
    -- Nécessite :
    --      La liste de mots ne doit pas être vide
    --
    -- Assure :
    --      Les instructions sont mises à jour avec les nouveaux tokens
    --
    -- Paramètres :
    --     Mots (in out): La ligne d'instruction à traiter
    --     Instructions (out): L'engistrement qui contiendra les tokens
    procedure Process_Null(Mots : in out T_Words_List; Instructions : out Memory.T_Instructions) with
        Pre => not Common_Types.Is_Empty(Mots),
        Post => Instructions.Token1 /= To_Unbounded_String("");

end Lexer;