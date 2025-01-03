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
        01 ENTITY-EVENT-DEATH       BINARY-CHAR UNSIGNED        VALUE 3.
        01 BUFFER                   PIC X(255).
        01 PLAYER-ID                BINARY-LONG UNSIGNED.
        01 BYTE-COUNT               BINARY-LONG UNSIGNED.
        01 OTHER-PLAYER-ID          BINARY-LONG UNSIGNED.
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

        MOVE 0 TO PLAYER-HEALTH(PLAYER-ID)
        CALL "SendPacket-SetHealth" USING PLAYER-CLIENT(PLAYER-ID) PLAYER-HEALTH(PLAYER-ID) PLAYER-FOOD-LEVEL(PLAYER-ID) PLAYER-SATURATION(PLAYER-ID)

        *> Play the death sound and animation to all players (including the dying player).
        *> For this to have any effect, the player must have 0 health. For the dying player, this is already the case.
        *> For all others, it will be handled by "set entity metadata" in the main loop.
        PERFORM VARYING OTHER-PLAYER-ID FROM 1 BY 1 UNTIL OTHER-PLAYER-ID > MAX-PLAYERS
            IF PLAYER-CLIENT(OTHER-PLAYER-ID) NOT = 0
                CALL "SendPacket-EntityEvent" USING PLAYER-CLIENT(OTHER-PLAYER-ID) PLAYER-ID ENTITY-EVENT-DEATH
            END-IF
        END-PERFORM

        INITIALIZE BUFFER
        STRING FUNCTION TRIM(PLAYER-NAME(PLAYER-ID)) " was killed" INTO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BYTE-COUNT
        CALL "BroadcastChatMessage" USING BUFFER BYTE-COUNT OMITTED

        INITIALIZE BUFFER
        STRING "Killed " FUNCTION TRIM(PLAYER-NAME(PLAYER-ID)) INTO BUFFER
        CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER OMITTED

        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-Kill.
