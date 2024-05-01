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
    01 BUFFER                   PIC X(256).
    01 BYTE-COUNT               BINARY-LONG UNSIGNED.
    01 TEMP-INT64               BINARY-LONG-LONG.
    01 TEMP-INT64-PIC           PIC -(19)9.
    *> shared data
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    *> help text
    01 HELP-TEXT-COUNT          BINARY-LONG UNSIGNED.
    01 HELP-TEXT                PIC X(256) OCCURS 16 TIMES.
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
    IF LK-INPUT(1:1) = "/"
        ADD 1 TO OFFSET
        SUBTRACT 1 FROM INPUT-LENGTH
    END-IF
    IF LK-INPUT(OFFSET + INPUT-LENGTH:1) = X"0A"
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
        WHEN "help"
            MOVE "Available commands:" TO HELP-TEXT(1)
            MOVE "  /help - show this help" TO HELP-TEXT(2)
            MOVE "  /say <message> - broadcast a message" TO HELP-TEXT(3)
            MOVE "  /save - save the world" TO HELP-TEXT(4)
            MOVE "  /stop - stop the server" TO HELP-TEXT(5)
            MOVE "  /time set (day|noon|night|midnight|<time>) - change the time" TO HELP-TEXT(6)
            MOVE 6 TO HELP-TEXT-COUNT
            PERFORM VARYING TEMP-INT64 FROM 1 BY 1 UNTIL TEMP-INT64 > HELP-TEXT-COUNT
                CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID HELP-TEXT(TEMP-INT64) C-COLOR-WHITE
            END-PERFORM

        WHEN "say"
            *> sender prefix
            IF LK-CLIENT-ID = 0
                MOVE "[Server]" TO BUFFER
                MOVE 8 TO BYTE-COUNT
            ELSE
                MOVE CLIENT-PLAYER(LK-CLIENT-ID) TO PLAYER-ID
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

        WHEN "stop"
            CALL "Server-Stop"

        WHEN "save"
            MOVE "Saving world" TO BUFFER
            CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
            CALL "Server-Save"
            MOVE "World saved" TO BUFFER
            CALL "HandleCommand-SendToClient" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE

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
