*> --- RegisterCommand-Stop ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-Stop.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "stop".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/stop - stop the server".
    01 PTR                          PROGRAM-POINTER.
    01 NODE-ROOT                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    SET PTR TO ENTRY "Callback-Execute"
    CALL "RegisterCommand" USING COMMAND-NAME COMMAND-HELP PTR NODE-ROOT
    CALL "SetCommandExecutable" USING NODE-ROOT

    GOBACK.

    *> --- Callback-Execute ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Execute.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-COMMAND-EXECUTE.

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-PARTS LK-PRINT-USAGE.
        IF LK-PART-COUNT NOT = 1
            MOVE 1 TO LK-PRINT-USAGE
            GOBACK
        END-IF
        CALL "Server-Stop"
        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-Stop.
