*> --- RegisterCommand-Say ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-Say.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-COMMAND-CONSTANTS.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "say".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/say <message> - broadcast a message".
    01 PTR                          PROGRAM-POINTER.
    01 NODE-ROOT                    BINARY-LONG UNSIGNED.
    01 NODE-MESSAGE                 BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    SET PTR TO ENTRY "Callback-Execute"
    CALL "RegisterCommand" USING COMMAND-NAME COMMAND-HELP PTR NODE-ROOT

    CALL "AddCommandArgument" USING NODE-ROOT "message" CMD-PARSER-STRING CMD-STRING-GREEDY NODE-MESSAGE
    CALL "SetCommandExecutable" USING NODE-MESSAGE

    GOBACK.

    *> --- Callback-Execute ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Execute.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-CLIENTS.
        COPY DD-PLAYERS.
        01 BUFFER                   PIC X(1024).
        01 BYTE-COUNT               BINARY-LONG UNSIGNED.
        01 PLAYER-ID                BINARY-LONG UNSIGNED.
        01 PART-INDEX               BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-COMMAND-EXECUTE.

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-PARTS LK-PRINT-USAGE.
        *> TODO handle empty message

        *> sender prefix
        EVALUATE LK-CLIENT-ID
            WHEN 0
                MOVE "[Server]" TO BUFFER
                MOVE 8 TO BYTE-COUNT
            WHEN OTHER
                MOVE CLIENT-PLAYER(LK-CLIENT-ID) TO PLAYER-ID
                INITIALIZE BUFFER
                STRING "[" FUNCTION TRIM(PLAYER-NAME(PLAYER-ID)) "]" INTO BUFFER
                MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BYTE-COUNT
        END-EVALUATE

        *> message body
        PERFORM VARYING PART-INDEX FROM 2 BY 1 UNTIL PART-INDEX > LK-PART-COUNT
            MOVE " " TO BUFFER(BYTE-COUNT + 1:1)
            ADD 1 TO BYTE-COUNT
            MOVE LK-PART-VALUE(PART-INDEX) TO BUFFER(BYTE-COUNT + 1:LK-PART-LENGTH(PART-INDEX))
            ADD LK-PART-LENGTH(PART-INDEX) TO BYTE-COUNT
        END-PERFORM

        *> broadcast it
        CALL "BroadcastChatMessage" USING BUFFER BYTE-COUNT OMITTED

        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-Say.
