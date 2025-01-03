*> --- RegisterCommand-Time ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-Time.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-COMMAND-CONSTANTS.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "time".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/time set (day|noon|night|midnight|<time>) - change the time".
    01 PTR                          PROGRAM-POINTER.
    01 NODE-ROOT                    BINARY-LONG UNSIGNED.
    01 NODE-OPERATION               BINARY-LONG UNSIGNED.
    01 NODE-ARGUMENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    SET PTR TO ENTRY "Callback-Execute"
    CALL "RegisterCommand" USING COMMAND-NAME COMMAND-HELP PTR NODE-ROOT

    CALL "AddCommandLiteral" USING NODE-ROOT "set" NODE-OPERATION

    CALL "AddCommandLiteral" USING NODE-OPERATION "day" NODE-ARGUMENT
    CALL "SetCommandExecutable" USING NODE-ARGUMENT

    CALL "AddCommandLiteral" USING NODE-OPERATION "noon" NODE-ARGUMENT
    CALL "SetCommandExecutable" USING NODE-ARGUMENT

    CALL "AddCommandLiteral" USING NODE-OPERATION "night" NODE-ARGUMENT
    CALL "SetCommandExecutable" USING NODE-ARGUMENT

    CALL "AddCommandLiteral" USING NODE-OPERATION "midnight" NODE-ARGUMENT
    CALL "SetCommandExecutable" USING NODE-ARGUMENT

    CALL "AddCommandArgument" USING NODE-OPERATION "time" CMD-PARSER-STRING CMD-STRING-SINGLE NODE-ARGUMENT
    CALL "SetCommandExecutable" USING NODE-ARGUMENT

    GOBACK.

    *> --- Callback-Execute ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Execute.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-CLIENTS.
        COPY DD-PLAYERS.
        01 BUFFER                   PIC X(255).
        01 TEMP-INT64               BINARY-LONG-LONG.
        01 TEMP-INT64-PIC           PIC -(19)9.
    LINKAGE SECTION.
        COPY DD-CALLBACK-COMMAND-EXECUTE.

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-PARTS LK-PRINT-USAGE.
        IF LK-PART-COUNT NOT = 3 OR LK-PART-VALUE(2) NOT = "set"
            MOVE 1 TO LK-PRINT-USAGE
            GOBACK
        END-IF

        EVALUATE LK-PART-VALUE(3)(1:LK-PART-LENGTH(3))
            WHEN "day"
                MOVE 1000 TO TEMP-INT64
            WHEN "noon"
                MOVE 6000 TO TEMP-INT64
            WHEN "night"
                MOVE 13000 TO TEMP-INT64
            WHEN "midnight"
                MOVE 18000 TO TEMP-INT64
            WHEN OTHER
                MOVE FUNCTION NUMVAL(LK-PART-VALUE(3)) TO TEMP-INT64
        END-EVALUATE

        CALL "World-SetTime" USING TEMP-INT64

        MOVE TEMP-INT64 TO TEMP-INT64-PIC
        INITIALIZE BUFFER
        STRING "Set the time to " FUNCTION TRIM(TEMP-INT64-PIC) INTO BUFFER
        CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER OMITTED

        GOBACK.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-Time.
