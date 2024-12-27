*> --- RegisterCommand-GameMode ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-GameMode.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "gamemode".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/gamemode <gamemode> - change your game mode".
    01 PTR                          PROGRAM-POINTER.
    01 NODE-ROOT                    BINARY-LONG UNSIGNED.
    01 NODE-GAMEMODE                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    SET PTR TO ENTRY "Callback-Execute"
    CALL "RegisterCommand" USING COMMAND-NAME COMMAND-HELP PTR NODE-ROOT

    CALL "AddCommandArgument-Simple" USING NODE-ROOT "gamemode" NODE-GAMEMODE
    CALL "SetCommandExecutable" USING NODE-GAMEMODE

    GOBACK.

    *> --- Callback-Execute ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Execute.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-CLIENTS.
        COPY DD-PLAYERS.
        01 C-COLOR-WHITE            PIC X(16)                   VALUE "white".
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
        IF LK-PART-COUNT NOT = 2
            MOVE 1 TO LK-PRINT-USAGE
            GOBACK
        END-IF

        EVALUATE LK-CLIENT-ID
            WHEN 0
                *> TODO handle console
                MOVE "This command is not available from the console" TO BUFFER
                CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                GOBACK
            WHEN OTHER
                MOVE CLIENT-PLAYER(LK-CLIENT-ID) TO PLAYER-ID
        END-EVALUATE

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

        INITIALIZE BUFFER
        STRING "Set own game mode to " FUNCTION TRIM(GAMEMODE-STRING(PLAYER-GAMEMODE(PLAYER-ID) + 1)) INTO BUFFER
        CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE

        *> game event 3: change game mode
        MOVE 3 TO GAME-EVENT-TYPE
        MOVE PLAYER-GAMEMODE(PLAYER-ID) TO GAME-EVENT-VALUE
        CALL "SendPacket-GameEvent" USING LK-CLIENT-ID GAME-EVENT-TYPE GAME-EVENT-VALUE

        CALL "SendPacket-PlayerAbilities" USING LK-CLIENT-ID PLAYER-GAMEMODE(PLAYER-ID) PLAYER-FLYING(PLAYER-ID)

        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-GameMode.
