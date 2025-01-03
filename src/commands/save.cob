*> --- RegisterCommand-Save ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-Save.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "save".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/save - save the world".
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
    WORKING-STORAGE SECTION.
        01 BUFFER                   PIC X(255).
    LINKAGE SECTION.
        COPY DD-CALLBACK-COMMAND-EXECUTE.

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-PARTS LK-PRINT-USAGE.
        IF LK-PART-COUNT NOT = 1
            MOVE 1 TO LK-PRINT-USAGE
            GOBACK
        END-IF

        MOVE "Saving world" TO BUFFER
        CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER OMITTED

        CALL "Server-Save"

        MOVE "World saved" TO BUFFER
        CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER OMITTED

        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-Save.
