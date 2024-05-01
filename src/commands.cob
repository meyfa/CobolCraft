*> --- HandleCommand ---
*> Handle a command, either input via the server console, or sent by a player.
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
    01 BUFFER                   PIC X(256).
    01 BYTE-COUNT               BINARY-LONG UNSIGNED.
    01 TEMP-INT64               BINARY-LONG-LONG.
LINKAGE SECTION.
    01 LK-INPUT                 PIC X(256).
    01 LK-INPUT-LENGTH          BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-INPUT-LENGTH.
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
            DISPLAY "Available commands:"
            DISPLAY "  /help - show this help"
            DISPLAY "  /say <message> - broadcast a message"
            DISPLAY "  /save - save the world"
            DISPLAY "  /stop - stop the server"
            DISPLAY "  /time set <time> - change the time"

        WHEN "say"
            MOVE "[Server]" TO BUFFER
            MOVE 8 TO BYTE-COUNT
            PERFORM VARYING PART-INDEX FROM 2 BY 1 UNTIL PART-INDEX > PART-COUNT
                MOVE " " TO BUFFER(BYTE-COUNT + 1:1)
                ADD 1 TO BYTE-COUNT
                MOVE PART-VALUE(PART-INDEX) TO BUFFER(BYTE-COUNT + 1:PART-LENGTH(PART-INDEX))
                ADD PART-LENGTH(PART-INDEX) TO BYTE-COUNT
            END-PERFORM
            CALL "BroadcastChatMessage" USING BUFFER BYTE-COUNT C-COLOR-WHITE

        WHEN "stop"
            CALL "Server-Stop"

        WHEN "save"
            CALL "Server-Save"

        WHEN "time"
            IF PART-COUNT NOT = 3 OR PART-VALUE(2) NOT = "set"
                DISPLAY "Usage: /time set <time>"
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
            DISPLAY "Set the time to " TEMP-INT64

        WHEN OTHER
            DISPLAY "Unknown command: " PART-VALUE(1)(1:PART-LENGTH(1))

    END-EVALUATE

    GOBACK.

END PROGRAM HandleCommand.
