*> --- RegisterCommand ---
*> Register a command with the server.
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-COMMANDS.
LINKAGE SECTION.
    01 LK-NAME              PIC X ANY LENGTH.
    01 LK-HELP              PIC X ANY LENGTH.
    01 LK-PTR-EXECUTE       PROGRAM-POINTER.

PROCEDURE DIVISION USING LK-NAME LK-HELP LK-PTR-EXECUTE.
    ADD 1 TO COMMAND-COUNT
    MOVE LK-NAME TO COMMAND-NAME(COMMAND-COUNT)
    MOVE LK-HELP TO COMMAND-HELP(COMMAND-COUNT)
    MOVE LK-PTR-EXECUTE TO COMMAND-PTR-EXECUTE(COMMAND-COUNT)
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
            CALL "SendChatMessage" USING LK-CLIENT-ID COMMAND-HELP(COMMAND-INDEX) C-COLOR-WHITE
            EXIT SECTION
        END-IF
    END-PERFORM

    *> Command not found
    INITIALIZE BUFFER
    STRING "Unknown command: " PART-VALUE(1)(1:PART-LENGTH(1)) INTO BUFFER
    CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE

    GOBACK.

END PROGRAM HandleCommand.

*> --- SendCommandData ---
*> Send a data structure describing the available commands and how to parse them to a client.
IDENTIFICATION DIVISION.
PROGRAM-ID. SendCommandData.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> Constants - node types
    78 TYPE-ROOT                GLOBAL                  VALUE 0.
    78 TYPE-LITERAL             GLOBAL                  VALUE 1.
    78 TYPE-ARGUMENT            GLOBAL                  VALUE 2.
    *> Constants - argument parser types
    01 PARSER-STRING            BINARY-LONG UNSIGNED    VALUE 5.
    *> Constants - properties
    01 PROPERTIES-STRING-SINGLE PIC X(1)                VALUE X"00".
    01 PROPERTIES-STRING-GREEDY PIC X(1)                VALUE X"02".
    *> The structure is initialized on first use.
    COPY DD-COMMAND-NODES REPLACING
        ==PREFIX-NODES== BY ==COMMAND-NODES GLOBAL==
        LEADING ==PREFIX== BY ==COMMAND==.
    01 IS-INITIALIZED           BINARY-CHAR UNSIGNED.
    *> temporary variables
    01 EXECUTABLE               BINARY-CHAR UNSIGNED.
    01 NODE-NAME                PIC X(256).
    01 PROPERTIES-LENGTH        BINARY-LONG UNSIGNED.
    01 PROPERTIES-VALUE         PIC X(256).
LINKAGE SECTION.
    01 LK-CLIENT-ID             BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT-ID.
    IF IS-INITIALIZED = 0
        MOVE 1 TO IS-INITIALIZED

        *> TODO make this part of command registration

        *> root node
        MOVE 1 TO COMMAND-NODE-COUNT
        MOVE TYPE-ROOT TO COMMAND-NODE-TYPE(1)

        *> "/gamemode"
        MOVE "gamemode" to NODE-NAME
        MOVE 0 TO EXECUTABLE
        CALL "AddCommandNode" USING NODE-NAME EXECUTABLE
        *> argument
        MOVE "gamemode" to NODE-NAME
        MOVE 1 TO EXECUTABLE
        MOVE LENGTH OF PROPERTIES-STRING-SINGLE TO PROPERTIES-LENGTH
        MOVE PROPERTIES-STRING-SINGLE TO PROPERTIES-VALUE
        CALL "AddArgumentNode" USING NODE-NAME EXECUTABLE PARSER-STRING PROPERTIES-LENGTH PROPERTIES-VALUE

        *> "/help"
        MOVE "help" to NODE-NAME
        MOVE 1 TO EXECUTABLE
        CALL "AddCommandNode" USING NODE-NAME EXECUTABLE

        *> "/say <message>"
        MOVE "say" to NODE-NAME
        MOVE 0 TO EXECUTABLE
        CALL "AddCommandNode" USING NODE-NAME EXECUTABLE
        *> argument
        MOVE "message" to NODE-NAME
        MOVE 1 TO EXECUTABLE
        MOVE LENGTH OF PROPERTIES-STRING-GREEDY TO PROPERTIES-LENGTH
        MOVE PROPERTIES-STRING-GREEDY TO PROPERTIES-VALUE
        CALL "AddArgumentNode" USING NODE-NAME EXECUTABLE PARSER-STRING PROPERTIES-LENGTH PROPERTIES-VALUE

        *> "/save"
        MOVE "save" to NODE-NAME
        MOVE 1 TO EXECUTABLE
        CALL "AddCommandNode" USING NODE-NAME EXECUTABLE

        *> "/stop"
        MOVE "stop" to NODE-NAME
        MOVE 1 TO EXECUTABLE
        CALL "AddCommandNode" USING NODE-NAME EXECUTABLE

        *> "/time set (day|noon|night|midnight|<time>)"
        MOVE "time" to NODE-NAME
        MOVE 0 TO EXECUTABLE
        CALL "AddCommandNode" USING NODE-NAME EXECUTABLE
        *> "set"
        MOVE "set" to NODE-NAME
        MOVE 0 TO EXECUTABLE
        CALL "AddLiteralNode" USING NODE-NAME EXECUTABLE
        *> argument
        *> TODO: differentiate between the literals and <time>
        MOVE "time" to NODE-NAME
        MOVE 1 TO EXECUTABLE
        MOVE LENGTH OF PROPERTIES-STRING-SINGLE TO PROPERTIES-LENGTH
        MOVE PROPERTIES-STRING-SINGLE TO PROPERTIES-VALUE
        CALL "AddArgumentNode" USING NODE-NAME EXECUTABLE PARSER-STRING PROPERTIES-LENGTH PROPERTIES-VALUE

        *> "/whitelist (reload|list|add|remove) [player]"
        MOVE "whitelist" to NODE-NAME
        MOVE 0 TO EXECUTABLE
        CALL "AddCommandNode" USING NODE-NAME EXECUTABLE
        *> argument
        *> TODO: differentiate between subcommands (requires appending children to any previous node)
        MOVE "command" to NODE-NAME
        MOVE 1 TO EXECUTABLE
        MOVE LENGTH OF PROPERTIES-STRING-SINGLE TO PROPERTIES-LENGTH
        MOVE PROPERTIES-STRING-SINGLE TO PROPERTIES-VALUE
        CALL "AddArgumentNode" USING NODE-NAME EXECUTABLE PARSER-STRING PROPERTIES-LENGTH PROPERTIES-VALUE
        *> player name
        *> TODO: make this conditional on the subcommand
        MOVE "player" to NODE-NAME
        MOVE 1 TO EXECUTABLE
        MOVE LENGTH OF PROPERTIES-STRING-SINGLE TO PROPERTIES-LENGTH
        MOVE PROPERTIES-STRING-SINGLE TO PROPERTIES-VALUE
        CALL "AddArgumentNode" USING NODE-NAME EXECUTABLE PARSER-STRING PROPERTIES-LENGTH PROPERTIES-VALUE
    END-IF

    CALL "SendPacket-Commands" USING LK-CLIENT-ID COMMAND-NODES
    GOBACK.

    *> --- AddCommandNode ---
    *> Add a command node to the structure.
    IDENTIFICATION DIVISION.
    PROGRAM-ID. AddCommandNode.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-NAME              PIC X ANY LENGTH.
        01 LK-EXECUTABLE        BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-NAME LK-EXECUTABLE.
        *> add a new child to the root node
        ADD 1 TO COMMAND-NODE-CHILD-COUNT(1)
        MOVE COMMAND-NODE-COUNT TO COMMAND-NODE-CHILD-INDEX(1, COMMAND-NODE-CHILD-COUNT(1))
        ADD 1 TO COMMAND-NODE-COUNT
        MOVE TYPE-LITERAL TO COMMAND-NODE-TYPE(COMMAND-NODE-COUNT)
        MOVE LK-NAME TO COMMAND-NODE-NAME(COMMAND-NODE-COUNT)
        MOVE LK-EXECUTABLE TO COMMAND-NODE-EXECUTABLE(COMMAND-NODE-COUNT)
        GOBACK.

    END PROGRAM AddCommandNode.

    *> --- AddLiteralNode ---
    *> Add a literal node to an existing node. This means that the literal must follow the existing node.
    IDENTIFICATION DIVISION.
    PROGRAM-ID. AddLiteralNode.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-NAME              PIC X ANY LENGTH.
        01 LK-EXECUTABLE        BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-NAME LK-EXECUTABLE.
        *> add a new child to the last node
        ADD 1 TO COMMAND-NODE-CHILD-COUNT(COMMAND-NODE-COUNT)
        MOVE COMMAND-NODE-COUNT TO COMMAND-NODE-CHILD-INDEX(COMMAND-NODE-COUNT, COMMAND-NODE-CHILD-COUNT(COMMAND-NODE-COUNT))
        ADD 1 TO COMMAND-NODE-COUNT
        *> add the literal node
        MOVE TYPE-LITERAL TO COMMAND-NODE-TYPE(COMMAND-NODE-COUNT)
        MOVE LK-NAME TO COMMAND-NODE-NAME(COMMAND-NODE-COUNT)
        MOVE LK-EXECUTABLE TO COMMAND-NODE-EXECUTABLE(COMMAND-NODE-COUNT)
        GOBACK.

    END PROGRAM AddLiteralNode.

    *> --- AddArgumentNode ---
    *> Add an argument node to an existing node. This means that the argument may (or must) follow the existing node.
    *> If multiple arguments are added to a node, they are treated as alternatives.
    *> To add multiple arguments that must be given in sequence, add each argument to its predecessor.
    IDENTIFICATION DIVISION.
    PROGRAM-ID. AddArgumentNode.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-NAME              PIC X ANY LENGTH.
        01 LK-EXECUTABLE        BINARY-CHAR UNSIGNED.
        01 LK-PARSER            BINARY-LONG UNSIGNED.
        01 LK-PROPERTIES-LENGTH BINARY-LONG UNSIGNED.
        01 LK-PROPERTIES        PIC X ANY LENGTH.

    PROCEDURE DIVISION USING LK-NAME LK-EXECUTABLE LK-PARSER LK-PROPERTIES-LENGTH LK-PROPERTIES.
        *> add a new child to the last node
        ADD 1 TO COMMAND-NODE-CHILD-COUNT(COMMAND-NODE-COUNT)
        MOVE COMMAND-NODE-COUNT TO COMMAND-NODE-CHILD-INDEX(COMMAND-NODE-COUNT, COMMAND-NODE-CHILD-COUNT(COMMAND-NODE-COUNT))
        ADD 1 TO COMMAND-NODE-COUNT
        *> add the argument node
        MOVE TYPE-ARGUMENT TO COMMAND-NODE-TYPE(COMMAND-NODE-COUNT)
        MOVE LK-NAME TO COMMAND-NODE-NAME(COMMAND-NODE-COUNT)
        MOVE LK-EXECUTABLE TO COMMAND-NODE-EXECUTABLE(COMMAND-NODE-COUNT)
        MOVE LK-PARSER TO COMMAND-NODE-PARSER(COMMAND-NODE-COUNT)
        MOVE LK-PROPERTIES-LENGTH TO COMMAND-NODE-PROPERTIES-LENGTH(COMMAND-NODE-COUNT)
        MOVE LK-PROPERTIES(1:LK-PROPERTIES-LENGTH) TO COMMAND-NODE-PROPERTIES(COMMAND-NODE-COUNT)
        GOBACK.

    END PROGRAM AddArgumentNode.

END PROGRAM SendCommandData.
