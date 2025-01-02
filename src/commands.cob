*> --- RegisterCommand ---
*> Register a command with the server.
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-COMMANDS.
LINKAGE SECTION.
    *> command definition
    01 LK-NAME              PIC X ANY LENGTH.
    01 LK-HELP              PIC X ANY LENGTH.
    01 LK-PTR-EXECUTE       PROGRAM-POINTER.
    *> resulting node index
    01 LK-COMMAND-NODE      BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-NAME LK-HELP LK-PTR-EXECUTE LK-COMMAND-NODE.
    ADD 1 TO COMMAND-COUNT

    MOVE LK-NAME TO COMMAND-NAME(COMMAND-COUNT)
    MOVE LK-HELP TO COMMAND-HELP(COMMAND-COUNT)
    MOVE LK-PTR-EXECUTE TO COMMAND-PTR-EXECUTE(COMMAND-COUNT)

    CALL "AddCommandLiteral" USING OMITTED LK-NAME LK-COMMAND-NODE

    GOBACK.

END PROGRAM RegisterCommand.

*> --- HandleCommand ---
*> Handle a command, either input via the server console (client id = 0), or sent by a player (client id > 0).
IDENTIFICATION DIVISION.
PROGRAM-ID. HandleCommand.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-COMMANDS.
    *> constants
    01 C-COLOR-WHITE            PIC X(16)                   VALUE "white".
    *> command parsing
    01 OFFSET                   BINARY-LONG UNSIGNED.
    01 INPUT-LENGTH             BINARY-LONG UNSIGNED.
    01 INPUT-INDEX              BINARY-LONG UNSIGNED.
    01 PARTS.
        02 PART-COUNT           BINARY-LONG UNSIGNED.
        02 PART OCCURS 128 TIMES.
            03 PART-VALUE       PIC X(256).
            03 PART-LENGTH      BINARY-LONG UNSIGNED.
    *> command handling
    01 BUFFER                   PIC X(255).
    01 COMMAND-INDEX            BINARY-LONG UNSIGNED.
    01 PTR                      PROGRAM-POINTER.
    01 PRINT-USAGE              BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT-ID             BINARY-LONG UNSIGNED.
    01 LK-INPUT                 PIC X(256).
    01 LK-INPUT-LENGTH          BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT-ID LK-INPUT LK-INPUT-LENGTH.
    *> TODO: Implement a permission system to restrict commands.
    *>       Make sure to double-check which commands are admin-only - e.g., "/say" is, while it may not seem like it.

    *> TODO: Log admin commands

    MOVE 0 TO OFFSET
    MOVE LK-INPUT-LENGTH TO INPUT-LENGTH

    *> Ignore leading forward slash and terminating newline
    IF INPUT-LENGTH > 0 AND LK-INPUT(1:1) = "/"
        ADD 1 TO OFFSET
        SUBTRACT 1 FROM INPUT-LENGTH
    END-IF
    IF INPUT-LENGTH > 0 AND LK-INPUT(OFFSET + INPUT-LENGTH:1) = X"0A"
        SUBTRACT 1 FROM INPUT-LENGTH
    END-IF

    *> Parse the string into space-delimited parts
    MOVE 1 TO PART-COUNT
    MOVE 0 TO PART-LENGTH(1)
    MOVE SPACES TO PART-VALUE(1)
    PERFORM VARYING INPUT-INDEX FROM 1 BY 1 UNTIL INPUT-INDEX > INPUT-LENGTH
        EVALUATE PART-LENGTH(PART-COUNT) ALSO LK-INPUT(OFFSET + INPUT-INDEX:1)
            *> ignore spaces at the beginning
            WHEN 0 ALSO " "
                CONTINUE
            *> for a non-empty part, space terminates the part
            WHEN > 0 ALSO " "
                ADD 1 TO PART-COUNT
                MOVE 0 TO PART-LENGTH(PART-COUNT)
                MOVE SPACES TO PART-VALUE(PART-COUNT)
            *> any other character extends the part
            WHEN OTHER
                ADD 1 TO PART-LENGTH(PART-COUNT)
                MOVE LK-INPUT(OFFSET + INPUT-INDEX:1) TO PART-VALUE(PART-COUNT)(PART-LENGTH(PART-COUNT):1)
        END-EVALUATE
    END-PERFORM
    IF PART-LENGTH(PART-COUNT) = 0
        SUBTRACT 1 FROM PART-COUNT
    END-IF

    *> Check for empty input
    IF PART-COUNT < 1
        EXIT SECTION
    END-IF

    *> Handle the command by looking up the first part
    PERFORM VARYING COMMAND-INDEX FROM 1 BY 1 UNTIL COMMAND-INDEX > COMMAND-COUNT
        IF COMMAND-NAME(COMMAND-INDEX) = PART-VALUE(1)(1:PART-LENGTH(1))
            MOVE COMMAND-PTR-EXECUTE(COMMAND-INDEX) TO PTR
            MOVE 0 TO PRINT-USAGE
            CALL PTR USING LK-CLIENT-ID PARTS PRINT-USAGE
            *> If the command executed successfully, exit the loop
            IF PRINT-USAGE = 0
                EXIT SECTION
            END-IF
            *> Print the usage for the command
            *> TOOD synthesize the usage string from the node structure
            INITIALIZE BUFFER
            STRING "Usage: " COMMAND-HELP(COMMAND-INDEX) INTO BUFFER
            CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
            EXIT SECTION
        END-IF
    END-PERFORM

    *> Command not found
    INITIALIZE BUFFER
    STRING "Unknown command: " PART-VALUE(1)(1:PART-LENGTH(1)) INTO BUFFER
    CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE

    GOBACK.

END PROGRAM HandleCommand.

*> --- AddCommandNode ---
*> Add a command node to the structure.
IDENTIFICATION DIVISION.
PROGRAM-ID. AddCommandNode.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-COMMAND-NODES.
    *> temporary variables
    01 PARENT-NODE-INDEX        BINARY-LONG UNSIGNED.
    01 PROPERTIES-LENGTH        BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    *> optional parent node index (default: root node)
    01 LK-PARENT-NODE           BINARY-LONG UNSIGNED.
    *> node definition
    01 LK-NODE-TYPE             BINARY-CHAR UNSIGNED.
    01 LK-NODE-NAME             PIC X ANY LENGTH.
    *> resulting node index
    01 LK-NODE-INDEX            BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING OPTIONAL LK-PARENT-NODE LK-NODE-TYPE LK-NODE-NAME LK-NODE-INDEX.
    *> allow coding OMITTED to refer to the root node
    IF LK-PARENT-NODE IS OMITTED
        MOVE 1 TO PARENT-NODE-INDEX
    ELSE
        MOVE LK-PARENT-NODE TO PARENT-NODE-INDEX
    END-IF

    *> ensure the root node exists and memory is initialized
    IF COMMAND-NODE-COUNT = 0 AND PARENT-NODE-INDEX = 1
        INITIALIZE COMMAND-NODES
        MOVE 1 TO COMMAND-NODE-COUNT
        MOVE 0 TO COMMAND-NODE-TYPE(1)
    END-IF

    *> add a new node
    ADD 1 TO COMMAND-NODE-COUNT
    MOVE COMMAND-NODE-COUNT TO LK-NODE-INDEX
    MOVE LK-NODE-TYPE TO COMMAND-NODE-TYPE(LK-NODE-INDEX)
    MOVE LK-NODE-NAME TO COMMAND-NODE-NAME(LK-NODE-INDEX)

    *> add the node to the parent
    ADD 1 TO COMMAND-NODE-CHILD-COUNT(PARENT-NODE-INDEX)
    COMPUTE COMMAND-NODE-CHILD-INDEX(PARENT-NODE-INDEX, COMMAND-NODE-CHILD-COUNT(PARENT-NODE-INDEX)) = LK-NODE-INDEX - 1

    GOBACK.

END PROGRAM AddCommandNode.

*> --- AddCommandLiteral ---
*> Add a literal argument to a command node.
IDENTIFICATION DIVISION.
PROGRAM-ID. AddCommandLiteral.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-COMMAND-NODES.
    01 NODE-TYPE                BINARY-CHAR UNSIGNED        VALUE 1.
LINKAGE SECTION.
    01 LK-PARENT-NODE           BINARY-LONG UNSIGNED.
    01 LK-NODE-NAME             PIC X ANY LENGTH.
    01 LK-NODE-INDEX            BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING OPTIONAL LK-PARENT-NODE LK-NODE-NAME LK-NODE-INDEX.
    IF LK-PARENT-NODE IS OMITTED
        CALL "AddCommandNode" USING OMITTED NODE-TYPE LK-NODE-NAME LK-NODE-INDEX
    ELSE
        CALL "AddCommandNode" USING LK-PARENT-NODE NODE-TYPE LK-NODE-NAME LK-NODE-INDEX
    END-IF
    GOBACK.

END PROGRAM AddCommandLiteral.

*> --- AddCommandArgument ---
*> Add an argument node to a command node. SetCommandParser must be called to set the parser.
IDENTIFICATION DIVISION.
PROGRAM-ID. AddCommandArgument.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-COMMAND-NODES.
    COPY DD-COMMAND-CONSTANTS.
    01 NODE-TYPE                BINARY-CHAR UNSIGNED        VALUE 2.
    01 PARSERS-REGISTRY         PIC X(31)                   VALUE "minecraft:command_argument_type".
    01 PARSER-ID                BINARY-LONG.
    01 PROPERTIES-LENGTH        BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-PARENT-NODE           BINARY-LONG UNSIGNED.
    01 LK-NODE-NAME             PIC X ANY LENGTH.
    01 LK-PARSER                PIC X ANY LENGTH.
    01 LK-PROPERTIES            PIC X ANY LENGTH.
    *> resulting node index
    01 LK-NODE-INDEX            BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-PARENT-NODE LK-NODE-NAME LK-PARSER OPTIONAL LK-PROPERTIES LK-NODE-INDEX.
    CALL "AddCommandNode" USING LK-PARENT-NODE NODE-TYPE LK-NODE-NAME LK-NODE-INDEX

    CALL "Registries-Get-EntryId" USING PARSERS-REGISTRY LK-PARSER PARSER-ID
    IF PARSER-ID <= 0
        DISPLAY "Unknown command parser: " FUNCTION TRIM(LK-PARSER)
        STOP RUN RETURNING 1
    END-IF
    MOVE PARSER-ID TO COMMAND-NODE-PARSER(LK-NODE-INDEX)

    IF LK-PROPERTIES IS NOT OMITTED
        MOVE FUNCTION LENGTH(LK-PROPERTIES) TO PROPERTIES-LENGTH
        MOVE LK-PROPERTIES TO COMMAND-NODE-PROPERTIES(LK-NODE-INDEX)(1:PROPERTIES-LENGTH)
        MOVE PROPERTIES-LENGTH TO COMMAND-NODE-PROPERTIES-LENGTH(LK-NODE-INDEX)
    END-IF

    *> Verify properties at registration time to avoid runtime errors.
    EVALUATE LK-PARSER ALSO TRUE
        WHEN CMD-PARSER-STRING ALSO (LK-PROPERTIES = CMD-STRING-SINGLE OR CMD-STRING-QUOTABLE-PHRASE OR CMD-STRING-GREEDY)
            GOBACK
        WHEN CMD-PARSER-ENTITY ALSO (LK-PROPERTIES = CMD-ENTITY-ANY-ANY OR CMD-ENTITY-ANY-PLAYER OR CMD-ENTITY-SINGLE-ANY OR CMD-ENTITY-SINGLE-PLAYER)
            GOBACK
        WHEN CMD-PARSER-GAME-PROFILE ALSO (LK-PROPERTIES IS OMITTED)
            GOBACK
        WHEN CMD-PARSER-GAMEMODE ALSO (LK-PROPERTIES IS OMITTED)
            GOBACK
    END-EVALUATE

    DISPLAY "Invalid properties for parser: " FUNCTION TRIM(LK-PARSER)
    STOP RUN RETURNING 1.

END PROGRAM AddCommandArgument.

*> --- SetCommandExecutable ---
*> Set the executable flag for a command node.
IDENTIFICATION DIVISION.
PROGRAM-ID. SetCommandExecutable.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-COMMAND-NODES.
LINKAGE SECTION.
    01 LK-NODE-INDEX            BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-NODE-INDEX.
    MOVE 1 TO COMMAND-NODE-EXECUTABLE(LK-NODE-INDEX)
    GOBACK.

END PROGRAM SetCommandExecutable.
