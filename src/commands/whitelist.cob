*> --- RegisterCommand-Whitelist ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-Whitelist.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "whitelist".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/whitelist (reload|list|add|remove) [player] - manage the whitelist".
    01 PTR                          PROGRAM-POINTER.
    01 NODE-ROOT                    BINARY-LONG UNSIGNED.
    01 NODE-OPERATION               BINARY-LONG UNSIGNED.
    01 NODE-ARGUMENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    SET PTR TO ENTRY "Callback-Execute"
    CALL "RegisterCommand" USING COMMAND-NAME COMMAND-HELP PTR NODE-ROOT

    CALL "AddCommandNode-Literal" USING NODE-ROOT "reload" NODE-OPERATION
    CALL "SetCommandExecutable" USING NODE-OPERATION

    CALL "AddCommandNode-Literal" USING NODE-ROOT "list" NODE-OPERATION
    CALL "SetCommandExecutable" USING NODE-OPERATION

    CALL "AddCommandNode-Literal" USING NODE-ROOT "add" NODE-OPERATION
    CALL "AddCommandArgument-Simple" USING NODE-OPERATION "player" NODE-ARGUMENT
    CALL "SetCommandExecutable" USING NODE-ARGUMENT

    CALL "AddCommandNode-Literal" USING NODE-ROOT "remove" NODE-OPERATION
    CALL "AddCommandArgument-Simple" USING NODE-OPERATION "player" NODE-ARGUMENT
    CALL "SetCommandExecutable" USING NODE-ARGUMENT

    GOBACK.

    *> --- Callback-Execute ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Execute.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-WHITELIST.
        01 C-COLOR-WHITE            PIC X(16)                   VALUE "white".
        01 BUFFER                   PIC X(255).
        01 BUFFER-POS               BINARY-LONG UNSIGNED.
        01 BYTE-COUNT               BINARY-LONG UNSIGNED.
        01 WHITELIST-INDEX          BINARY-LONG UNSIGNED.
        01 FAILURE                  BINARY-CHAR UNSIGNED.
        01 TEMP-UUID                PIC X(16).
        01 TEMP-NAME                PIC X(16).
        01 TEMP-INT64-PIC           PIC -(19)9.
    LINKAGE SECTION.
        COPY DD-CALLBACK-COMMAND-EXECUTE.

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-PARTS LK-PRINT-USAGE.
        IF LK-PART-COUNT = 2 AND LK-PART-VALUE(2) = "reload"
            CALL "Whitelist-Read" USING FAILURE
            IF FAILURE NOT = 0
                MOVE "Error reloading the whitelist" TO BUFFER
                CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                GOBACK
            END-IF
            MOVE "Reloaded the whitelist" TO BUFFER
            CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
            GOBACK
        END-IF

        IF LK-PART-COUNT = 2 AND LK-PART-VALUE(2) = "list"
            IF WHITELIST-LENGTH = 0
                MOVE "There are no whitelisted players" TO BUFFER
                CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                GOBACK
            END-IF
            MOVE WHITELIST-LENGTH TO TEMP-INT64-PIC
            INITIALIZE BUFFER
            STRING "There are " FUNCTION TRIM(TEMP-INT64-PIC) " whitelisted player(s):" INTO BUFFER
            COMPUTE BUFFER-POS = FUNCTION STORED-CHAR-LENGTH(BUFFER) + 2
            PERFORM VARYING WHITELIST-INDEX FROM 1 BY 1 UNTIL WHITELIST-INDEX > WHITELIST-LENGTH
                IF WHITELIST-INDEX > 1
                    MOVE ", " TO BUFFER(BUFFER-POS:2)
                    ADD 2 TO BUFFER-POS
                END-IF
                MOVE FUNCTION STORED-CHAR-LENGTH(WHITELIST-NAME(WHITELIST-INDEX)) TO BYTE-COUNT
                MOVE WHITELIST-NAME(WHITELIST-INDEX)(1:BYTE-COUNT) TO BUFFER(BUFFER-POS:BYTE-COUNT)
                ADD BYTE-COUNT TO BUFFER-POS
            END-PERFORM
            CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
            GOBACK
        END-IF

        IF LK-PART-COUNT = 3
            MOVE LK-PART-VALUE(3) TO TEMP-NAME
            MOVE FUNCTION STORED-CHAR-LENGTH(TEMP-NAME) TO BYTE-COUNT
            *> TODO refactor this to a subroutine (currently duplicated with server)
            MOVE X"00000000000000000000000000000000" TO TEMP-UUID
            MOVE TEMP-NAME(1:BYTE-COUNT) TO TEMP-UUID(1:BYTE-COUNT)

            IF LK-PART-VALUE(2) = "add"
                CALL "Whitelist-Add" USING TEMP-UUID TEMP-NAME BYTE-COUNT FAILURE
                IF FAILURE NOT = 0
                    MOVE "Player is already whitelisted" TO BUFFER
                ELSE
                    INITIALIZE BUFFER
                    STRING "Added " FUNCTION TRIM(TEMP-NAME(1:BYTE-COUNT)) " to the whitelist" INTO BUFFER
                END-IF
                CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                GOBACK
            END-IF

            IF LK-PART-VALUE(2) = "remove"
                CALL "Whitelist-Remove" USING TEMP-UUID TEMP-NAME BYTE-COUNT FAILURE
                IF FAILURE NOT = 0
                    MOVE "Player is not whitelisted" TO BUFFER
                ELSE
                    INITIALIZE BUFFER
                    STRING "Removed " FUNCTION TRIM(TEMP-NAME(1:BYTE-COUNT)) " from the whitelist" INTO BUFFER
                END-IF
                CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER C-COLOR-WHITE
                GOBACK
            END-IF
        END-IF

        MOVE 1 TO LK-PRINT-USAGE

        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-Whitelist.
