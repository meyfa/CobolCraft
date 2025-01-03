*> --- RegisterCommand-GameMode ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-GameMode.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-COMMAND-CONSTANTS.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "gamemode".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/gamemode <gamemode> [player] - change your game mode".
    01 PTR                          PROGRAM-POINTER.
    01 NODE-ROOT                    BINARY-LONG UNSIGNED.
    01 NODE-GAMEMODE                BINARY-LONG UNSIGNED.
    01 NODE-PLAYER                  BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    SET PTR TO ENTRY "Callback-Execute"
    CALL "RegisterCommand" USING COMMAND-NAME COMMAND-HELP PTR NODE-ROOT

    CALL "AddCommandArgument" USING NODE-ROOT "gamemode" CMD-PARSER-GAMEMODE OMITTED NODE-GAMEMODE
    CALL "SetCommandExecutable" USING NODE-GAMEMODE

    *> Uses entity parser (instead of game profile parser) to restrict to online players
    CALL "AddCommandArgument" USING NODE-GAMEMODE "player" CMD-PARSER-ENTITY CMD-ENTITY-SINGLE-PLAYER NODE-PLAYER
    CALL "SetCommandExecutable" USING NODE-PLAYER

    GOBACK.

    *> --- Callback-Execute ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Execute.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-CLIENTS.
        COPY DD-PLAYERS.
        COPY DD-SERVER-PROPERTIES.
        01 BUFFER                   PIC X(255).
        01 PLAYER-ID                BINARY-LONG UNSIGNED.
        01 GAME-EVENT-TYPE          BINARY-CHAR UNSIGNED.
        01 GAME-EVENT-VALUE         FLOAT-SHORT.
        *> game mode enum to string
        01 GAMEMODE-STRING-ENUM.
            02 SURVIVAL-MODE        PIC X(16) VALUE "Survival Mode".
            02 CREATIVE-MODE        PIC X(16) VALUE "Creative Mode".
            02 ADVENTURE-MODE       PIC X(16) VALUE "Adventure Mode".
            02 SPECTATOR-MODE       PIC X(16) VALUE "Spectator Mode".
        01 GAMEMODE-STRINGS         REDEFINES GAMEMODE-STRING-ENUM.
            02 GAMEMODE-STRING      PIC X(16) OCCURS 4 TIMES.
    LINKAGE SECTION.
        COPY DD-CALLBACK-COMMAND-EXECUTE.

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-PARTS LK-PRINT-USAGE.
        IF LK-PART-COUNT < 2 OR > 3
            MOVE 1 TO LK-PRINT-USAGE
            GOBACK
        END-IF

        EVALUATE LK-CLIENT-ID
            WHEN 0
                *> Console requires a player argument
                IF LK-PART-COUNT < 3
                    MOVE 1 TO LK-PRINT-USAGE
                    GOBACK
                END-IF
            WHEN OTHER
                MOVE CLIENT-PLAYER(LK-CLIENT-ID) TO PLAYER-ID
        END-EVALUATE

        IF LK-PART-COUNT = 3
            PERFORM VARYING PLAYER-ID FROM 1 BY 1 UNTIL PLAYER-ID > MAX-PLAYERS
                IF PLAYER-NAME(PLAYER-ID) = LK-PART-VALUE(3)
                    EXIT PERFORM
                END-IF
            END-PERFORM
            IF PLAYER-ID > MAX-PLAYERS
                MOVE "Player not found" TO BUFFER
                CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER OMITTED
                GOBACK
            END-IF
        END-IF

        EVALUATE LK-PART-VALUE(2)
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
                MOVE 1 TO LK-PRINT-USAGE
                GOBACK
        END-EVALUATE

        *> game event 3: change game mode
        MOVE 3 TO GAME-EVENT-TYPE
        MOVE PLAYER-GAMEMODE(PLAYER-ID) TO GAME-EVENT-VALUE
        CALL "SendPacket-GameEvent" USING PLAYER-CLIENT(PLAYER-ID) GAME-EVENT-TYPE GAME-EVENT-VALUE

        CALL "SendPacket-PlayerAbilities" USING PLAYER-CLIENT(PLAYER-ID) PLAYER-GAMEMODE(PLAYER-ID) PLAYER-FLYING(PLAYER-ID)

        IF PLAYER-CLIENT(PLAYER-ID) = LK-CLIENT-ID
            INITIALIZE BUFFER
            STRING "Set own game mode to " FUNCTION TRIM(GAMEMODE-STRING(PLAYER-GAMEMODE(PLAYER-ID) + 1)) INTO BUFFER
            CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER OMITTED
        ELSE
            INITIALIZE BUFFER
            STRING "Set " FUNCTION TRIM(PLAYER-NAME(PLAYER-ID)) "'s game mode to " FUNCTION TRIM(GAMEMODE-STRING(PLAYER-GAMEMODE(PLAYER-ID) + 1)) INTO BUFFER
            CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER OMITTED
        END-IF

        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-GameMode.
