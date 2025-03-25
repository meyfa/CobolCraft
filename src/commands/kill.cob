*> --- RegisterCommand-Kill ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-Kill.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-COMMAND-CONSTANTS.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "kill".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/kill [player] - kill a player".
    01 PTR                          PROGRAM-POINTER.
    01 NODE-ROOT                    BINARY-LONG UNSIGNED.
    01 NODE-PLAYER                  BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    SET PTR TO ENTRY "Callback-Execute"
    CALL "RegisterCommand" USING COMMAND-NAME COMMAND-HELP PTR NODE-ROOT
    CALL "SetCommandExecutable" USING NODE-ROOT

    *> Uses entity parser (instead of game profile parser) to restrict to online players
    *> TODO support entities
    CALL "AddCommandArgument" USING NODE-ROOT "player" CMD-PARSER-ENTITY CMD-ENTITY-SINGLE-PLAYER NODE-PLAYER
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
        *> as close to infinity as we can get
        01 DAMAGE-AMOUNT            FLOAT-SHORT                 VALUE 3.40282346638528859811704183484516925E+38.
        01 DAMAGE-TYPE              BINARY-LONG.
        01 BUFFER                   PIC X(255).
        01 PLAYER-ID                BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-COMMAND-EXECUTE.

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-PARTS LK-PRINT-USAGE.
        IF (LK-PART-COUNT < 1 OR > 2) OR (LK-CLIENT-ID = 0 AND LK-PART-COUNT < 2)
            MOVE 1 TO LK-PRINT-USAGE
            GOBACK
        END-IF

        IF LK-CLIENT-ID > 0
            MOVE CLIENT-PLAYER(LK-CLIENT-ID) TO PLAYER-ID
        END-IF

        IF LK-PART-COUNT = 2
            PERFORM VARYING PLAYER-ID FROM 1 BY 1 UNTIL PLAYER-ID > MAX-PLAYERS
                IF PLAYER-NAME(PLAYER-ID) = LK-PART-VALUE(2)
                    EXIT PERFORM
                END-IF
            END-PERFORM
            IF PLAYER-ID > MAX-PLAYERS
                MOVE "Player not found" TO BUFFER
                CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER OMITTED
                GOBACK
            END-IF
        END-IF

        CALL "Registries-Lookup" USING "minecraft:damage_type" "minecraft:generic_kill" DAMAGE-TYPE
        CALL "Players-Damage" USING PLAYER-ID DAMAGE-AMOUNT DAMAGE-TYPE

        INITIALIZE BUFFER
        STRING "Killed " FUNCTION TRIM(PLAYER-NAME(PLAYER-ID)) INTO BUFFER
        CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER OMITTED

        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-Kill.
