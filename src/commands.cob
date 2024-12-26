*> --- HandleCommand ---
*> Handle a command, either input via the server console (client id = 0), or sent by a player (client id > 0).
IDENTIFICATION DIVISION.
PROGRAM-ID. HandleCommand.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> constants
    01 C-COLOR-WHITE            PIC X(16)                   VALUE "white".
    *> command parsing
    01 OFFSET                   BINARY-LONG UNSIGNED.
    01 INPUT-LENGTH             BINARY-LONG UNSIGNED.
    01 INPUT-INDEX              BINARY-LONG UNSIGNED.
    01 PART-COUNT               BINARY-LONG UNSIGNED.
    01 PARTS.
        02 PART OCCURS 128 TIMES.
            03 PART-VALUE       PIC X(256).
            03 PART-LENGTH      BINARY-LONG UNSIGNED.
    *> command handling
    01 PART-INDEX               BINARY-LONG UNSIGNED.
    01 PLAYER-ID                BINARY-LONG UNSIGNED.
    01 BUFFER                   PIC X(64000).
    01 BUFFER-POS               BINARY-LONG UNSIGNED.
    01 BYTE-COUNT               BINARY-LONG UNSIGNED.
    01 TEMP-INT8                BINARY-CHAR.
    01 TEMP-INT64               BINARY-LONG-LONG.
    01 TEMP-INT64-PIC           PIC -(19)9.
    01 TEMP-FLOAT               FLOAT-SHORT.
    *> shared data
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    COPY DD-WHITELIST.
    *> help text
    01 HELP-TEXT-COUNT          BINARY-LONG UNSIGNED.
    01 HELP-TEXT                PIC X(256) OCCURS 16 TIMES.
    *> game mode enum to string
    01 GAMEMODE-STRING-ENUM.
        02 SURVIVAL-MODE        PIC X(16) VALUE "Survival Mode".
        02 CREATIVE-MODE        PIC X(16) VALUE "Creative Mode".
        02 ADVENTURE-MODE       PIC X(16) VALUE "Adventure Mode".
        02 SPECTATOR-MODE       PIC X(16) VALUE "Spectator Mode".
    01 GAMEMODE-STRINGS         REDEFINES GAMEMODE-STRING-ENUM.
        02 GAMEMODE-STRING      PIC X(16) OCCURS 4 TIMES.
    *> whitelist commands
    01 WHITELIST-INDEX          BINARY-LONG UNSIGNED.
    01 WHITELIST-FAILURE        BINARY-CHAR UNSIGNED.
    01 WHITELIST-TEMP-UUID      PIC X(16).
    01 WHITELIST-TEMP-NAME      PIC X(16).
LINKAGE SECTION.
    01 LK-CLIENT-ID             BINARY-LONG UNSIGNED.
    01 LK-INPUT                 PIC X(256).
    01 LK-INPUT-LENGTH          BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT-ID LK-INPUT LK-INPUT-LENGTH.
    *> TODO: Implement a permission system to restrict commands.
    *>       Make sure to double-check which commands are admin-only - e.g., "/say" is, while it may not seem like it.

    EVALUATE LK-CLIENT-ID
        WHEN 0
            MOVE 0 TO PLAYER-ID
        WHEN OTHER
            MOVE CLIENT-PLAYER(LK-CLIENT-ID) TO PLAYER-ID
    END-EVALUATE

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

    *> Handle the command
    EVALUATE PART-VALUE(1)
        WHEN "gamemode"
            IF PART-COUNT NOT = 2
                MOVE "Usage: /gamemode <gamemode>" TO BUFFER
                CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                EXIT SECTION
            END-IF
            EVALUATE PART-VALUE(2)
                WHEN "survival"
                    MOVE 0 TO PLAYER-GAMEMODE(PLAYER-ID)
                    MOVE 0 TO PLAYER-FLYING(PLAYER-ID)
                WHEN "creative"
                    MOVE 1 TO PLAYER-GAMEMODE(PLAYER-ID)
                WHEN "adventure"
                    MOVE 2 TO PLAYER-GAMEMODE(PLAYER-ID)
                    MOVE 0 TO PLAYER-FLYING(PLAYER-ID)
                WHEN "spectator"
                    MOVE 3 TO PLAYER-GAMEMODE(PLAYER-ID)
                WHEN OTHER
                    MOVE "Usage: /gamemode <gamemode>" TO BUFFER
                    CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                    EXIT SECTION
            END-EVALUATE
            INITIALIZE BUFFER
            STRING "Set own game mode to " FUNCTION TRIM(GAMEMODE-STRING(PLAYER-GAMEMODE(PLAYER-ID) + 1)) INTO BUFFER
            CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
            *> game event 3: change game mode
            MOVE 3 TO TEMP-INT8
            MOVE PLAYER-GAMEMODE(PLAYER-ID) TO TEMP-FLOAT
            CALL "SendPacket-GameEvent" USING LK-CLIENT-ID TEMP-INT8 TEMP-FLOAT
            CALL "SendPacket-PlayerAbilities" USING LK-CLIENT-ID PLAYER-GAMEMODE(PLAYER-ID) PLAYER-FLYING(PLAYER-ID)

        WHEN "help"
            MOVE "Available commands:" TO HELP-TEXT(1)
            MOVE "/gamemode <gamemode> - change your game mode" TO HELP-TEXT(2)
            MOVE "/help - show this help" TO HELP-TEXT(3)
            MOVE "/say <message> - broadcast a message" TO HELP-TEXT(4)
            MOVE "/save - save the world" TO HELP-TEXT(5)
            MOVE "/stop - stop the server" TO HELP-TEXT(6)
            MOVE "/time set (day|noon|night|midnight|<time>) - change the time" TO HELP-TEXT(7)
            MOVE "/whitelist (reload|list|add|remove) [player] - manage the whitelist" TO HELP-TEXT(8)
            MOVE 8 TO HELP-TEXT-COUNT
            PERFORM VARYING TEMP-INT64 FROM 1 BY 1 UNTIL TEMP-INT64 > HELP-TEXT-COUNT
                CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID HELP-TEXT(TEMP-INT64) C-COLOR-WHITE
            END-PERFORM

        WHEN "say"
            *> TODO handle empty message
            *> sender prefix
            IF LK-CLIENT-ID = 0
                MOVE "[Server]" TO BUFFER
                MOVE 8 TO BYTE-COUNT
            ELSE
                INITIALIZE BUFFER
                STRING "[" PLAYER-NAME(PLAYER-ID)(1:PLAYER-NAME-LENGTH(PLAYER-ID)) "]" INTO BUFFER
                MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BYTE-COUNT
            END-IF
            *> message body
            PERFORM VARYING PART-INDEX FROM 2 BY 1 UNTIL PART-INDEX > PART-COUNT
                MOVE " " TO BUFFER(BYTE-COUNT + 1:1)
                ADD 1 TO BYTE-COUNT
                MOVE PART-VALUE(PART-INDEX) TO BUFFER(BYTE-COUNT + 1:PART-LENGTH(PART-INDEX))
                ADD PART-LENGTH(PART-INDEX) TO BYTE-COUNT
            END-PERFORM
            *> broadcast it
            CALL "BroadcastChatMessage" USING BUFFER BYTE-COUNT C-COLOR-WHITE

        WHEN "save"
            MOVE "Saving world" TO BUFFER
            CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
            CALL "Server-Save"
            MOVE "World saved" TO BUFFER
            CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE

        WHEN "stop"
            CALL "Server-Stop"

        WHEN "time"
            IF PART-COUNT NOT = 3 OR PART-VALUE(2) NOT = "set"
                MOVE "Usage: /time set (day|noon|night|midnight|<time>)" TO BUFFER
                CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                EXIT SECTION
            END-IF
            EVALUATE PART-VALUE(3)(1:PART-LENGTH(3))
                WHEN "day"
                    MOVE 1000 TO TEMP-INT64
                WHEN "noon"
                    MOVE 6000 TO TEMP-INT64
                WHEN "night"
                    MOVE 13000 TO TEMP-INT64
                WHEN "midnight"
                    MOVE 18000 TO TEMP-INT64
                WHEN OTHER
                    MOVE FUNCTION NUMVAL(PART-VALUE(3)) TO TEMP-INT64
            END-EVALUATE
            CALL "World-SetTime" USING TEMP-INT64
            MOVE TEMP-INT64 TO TEMP-INT64-PIC
            INITIALIZE BUFFER
            STRING "Set the time to " FUNCTION TRIM(TEMP-INT64-PIC) INTO BUFFER
            CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE

        WHEN "whitelist"
            IF PART-COUNT = 2 AND PART-VALUE(2) = "reload"
                CALL "Whitelist-Read" USING WHITELIST-FAILURE
                IF WHITELIST-FAILURE NOT = 0
                    MOVE "Error reloading the whitelist" TO BUFFER
                    CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                    EXIT SECTION
                END-IF
                MOVE "Reloaded the whitelist" TO BUFFER
                CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                EXIT SECTION
            END-IF
            IF PART-COUNT = 2 AND PART-VALUE(2) = "list"
                IF WHITELIST-LENGTH = 0
                    MOVE "There are no whitelisted players" TO BUFFER
                    CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                    EXIT SECTION
                END-IF
                MOVE WHITELIST-LENGTH TO TEMP-INT64-PIC
                INITIALIZE BUFFER
                STRING "There are " FUNCTION TRIM(TEMP-INT64-PIC) " whitelisted player(s):" INTO BUFFER
                COMPUTE BUFFER-POS = FUNCTION STORED-CHAR-LENGTH(BUFFER) + 2
                PERFORM VARYING WHITELIST-INDEX FROM 1 BY 1 UNTIL WHITELIST-INDEX > WHITELIST-LENGTH
                    IF WHITELIST-INDEX > 1
                        MOVE ", " TO BUFFER(BUFFER-POS:2)
                        ADD 2 TO BUFFER-POS
                    END-IF
                    MOVE FUNCTION STORED-CHAR-LENGTH(WHITELIST-NAME(WHITELIST-INDEX)) TO BYTE-COUNT
                    MOVE WHITELIST-NAME(WHITELIST-INDEX)(1:BYTE-COUNT) TO BUFFER(BUFFER-POS:BYTE-COUNT)
                    ADD BYTE-COUNT TO BUFFER-POS
                END-PERFORM
                CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                EXIT SECTION
            END-IF
            IF PART-COUNT = 3
                MOVE PART-VALUE(3) TO WHITELIST-TEMP-NAME
                MOVE FUNCTION STORED-CHAR-LENGTH(WHITELIST-TEMP-NAME) TO BYTE-COUNT
                *> TODO refactor this to a subroutine (currently duplicated with server)
                MOVE X"00000000000000000000000000000000" TO WHITELIST-TEMP-UUID
                MOVE WHITELIST-TEMP-NAME(1:BYTE-COUNT) TO WHITELIST-TEMP-UUID(1:BYTE-COUNT)
                IF PART-VALUE(2) = "add"
                    CALL "Whitelist-Add" USING WHITELIST-TEMP-UUID WHITELIST-TEMP-NAME BYTE-COUNT WHITELIST-FAILURE
                    IF WHITELIST-FAILURE NOT = 0
                        MOVE "Player is already whitelisted" TO BUFFER
                    ELSE
                        INITIALIZE BUFFER
                        STRING "Added " FUNCTION TRIM(WHITELIST-TEMP-NAME(1:BYTE-COUNT)) " to the whitelist" INTO BUFFER
                    END-IF
                    CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                    EXIT SECTION
                END-IF
                IF PART-VALUE(2) = "remove"
                    CALL "Whitelist-Remove" USING WHITELIST-TEMP-UUID WHITELIST-TEMP-NAME BYTE-COUNT WHITELIST-FAILURE
                    IF WHITELIST-FAILURE NOT = 0
                        MOVE "Player is not whitelisted" TO BUFFER
                    ELSE
                        INITIALIZE BUFFER
                        STRING "Removed " FUNCTION TRIM(WHITELIST-TEMP-NAME(1:BYTE-COUNT)) " from the whitelist" INTO BUFFER
                    END-IF
                    CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                    EXIT SECTION
                END-IF
            END-IF
            MOVE "Usage: /whitelist (reload|list|add|remove) [player]" TO BUFFER
            CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE

        WHEN OTHER
            INITIALIZE BUFFER
            STRING "Unknown command: " PART-VALUE(1)(1:PART-LENGTH(1)) INTO BUFFER
            CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE

    END-EVALUATE

    GOBACK.

    *> --- HandleCommand-SendToClient ---
    *> Subroutine to send a message to the client (player or server console) that executed the command.
    IDENTIFICATION DIVISION.
    PROGRAM-ID. HandleCommand-SendToClient.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-CLIENTS.
        01 BYTE-COUNT               BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        01 LK-CLIENT-ID             BINARY-LONG UNSIGNED.
        01 LK-MESSAGE               PIC X ANY LENGTH.
        01 LK-COLOR                 PIC X(16).

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-MESSAGE LK-COLOR.
        MOVE FUNCTION STORED-CHAR-LENGTH(LK-MESSAGE) TO BYTE-COUNT
        IF LK-CLIENT-ID = 0
            DISPLAY LK-MESSAGE(1:BYTE-COUNT)
            GOBACK
        END-IF
        CALL "SendPacket-SystemChat" USING LK-CLIENT-ID LK-MESSAGE BYTE-COUNT LK-COLOR
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
