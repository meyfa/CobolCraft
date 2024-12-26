*> --- RegisterCommand-Help ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-Help.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "help".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/help - show this help".
    01 PTR                          PROGRAM-POINTER.

PROCEDURE DIVISION.
    SET PTR TO ENTRY "Callback-Execute"
    CALL "RegisterCommand" USING COMMAND-NAME COMMAND-HELP PTR
    GOBACK.

    *> --- Callback-Execute ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Execute.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-COMMANDS.
        01 C-COLOR-WHITE            PIC X(16)                   VALUE "white".
        01 BUFFER                   PIC X(255).
        01 COMMAND-INDEX            BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-COMMAND-EXECUTE.

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-PARTS LK-PRINT-USAGE.
        IF LK-PART-COUNT NOT = 1
            MOVE 1 TO LK-PRINT-USAGE
            GOBACK
        END-IF

        MOVE "Available commands:" TO BUFFER
        CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE

        PERFORM VARYING COMMAND-INDEX FROM 1 BY 1 UNTIL COMMAND-INDEX > COMMAND-COUNT
            MOVE COMMAND-HELP(COMMAND-INDEX) TO BUFFER
            CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
        END-PERFORM

        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-Help.
