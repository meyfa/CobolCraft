*> --- RegisterCommand-Whitelist ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterCommand-Whitelist.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-COMMAND-CONSTANTS.
    01 COMMAND-NAME                 PIC X(100)                  VALUE "whitelist".
    01 COMMAND-HELP                 PIC X(255)                  VALUE "/whitelist (reload|on|off|list|add|remove) [player] - manage the whitelist".
    01 PTR                          PROGRAM-POINTER.
    01 NODE-ROOT                    BINARY-LONG UNSIGNED.
    01 NODE-OPERATION               BINARY-LONG UNSIGNED.
    01 NODE-ARGUMENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    SET PTR TO ENTRY "Callback-Execute"
    CALL "RegisterCommand" USING COMMAND-NAME COMMAND-HELP PTR NODE-ROOT

    CALL "AddCommandLiteral" USING NODE-ROOT "reload" NODE-OPERATION
    CALL "SetCommandExecutable" USING NODE-OPERATION

    CALL "AddCommandLiteral" USING NODE-ROOT "on" NODE-OPERATION
    CALL "SetCommandExecutable" USING NODE-OPERATION

    CALL "AddCommandLiteral" USING NODE-ROOT "off" NODE-OPERATION
    CALL "SetCommandExecutable" USING NODE-OPERATION

    CALL "AddCommandLiteral" USING NODE-ROOT "list" NODE-OPERATION
    CALL "SetCommandExecutable" USING NODE-OPERATION

    CALL "AddCommandLiteral" USING NODE-ROOT "add" NODE-OPERATION
    CALL "AddCommandArgument" USING NODE-OPERATION "player" CMD-PARSER-GAME-PROFILE OMITTED NODE-ARGUMENT
    CALL "SetCommandExecutable" USING NODE-ARGUMENT

    CALL "AddCommandLiteral" USING NODE-ROOT "remove" NODE-OPERATION
    CALL "AddCommandArgument" USING NODE-OPERATION "player" CMD-PARSER-GAME-PROFILE OMITTED NODE-ARGUMENT
    CALL "SetCommandExecutable" USING NODE-ARGUMENT

    GOBACK.

    *> --- Callback-Execute ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Execute.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-WHITELIST.
        COPY DD-SERVER-PROPERTIES.
        01 BUFFER                   PIC X(128).
        01 BUFFER-POS               BINARY-LONG UNSIGNED.
        01 BUFFER-ADDRESS           USAGE POINTER.
        01 DYN-BUFFER-LEN           BINARY-LONG UNSIGNED VALUE ZERO.
        01 WHITELIST-INDEX          BINARY-LONG UNSIGNED.
        01 IO-FAILURE               BINARY-CHAR UNSIGNED.
        01 STATE-FAILURE            BINARY-CHAR UNSIGNED.
        01 TEMP-UUID                PIC X(16).
        01 TEMP-NAME                PIC X(16).
        01 TEMP-INT64-PIC           PIC -(19)9.
    LINKAGE SECTION.
        COPY DD-CALLBACK-COMMAND-EXECUTE.
        01 DYN-BUFFER.
            02 FILLER               PIC X OCCURS UNBOUNDED DEPENDING ON DYN-BUFFER-LEN.

    PROCEDURE DIVISION USING LK-CLIENT-ID LK-PARTS LK-PRINT-USAGE.
        IF LK-PART-COUNT < 2
            MOVE 1 TO LK-PRINT-USAGE
            GOBACK
        END-IF

        EVALUATE LK-PART-COUNT ALSO LK-PART-VALUE(2)
            WHEN 2 ALSO "reload"
                CALL "Whitelist-Read" USING IO-FAILURE
                PERFORM HandleDiskFailure
                MOVE "Reloaded the whitelist" TO BUFFER

            WHEN 2 ALSO "on"
                MOVE 1 TO SP-WHITELIST-ENABLE
                CALL "ServerProperties-Write" USING IO-FAILURE
                PERFORM HandleDiskFailure
                MOVE "Whitelist enabled" TO BUFFER

            WHEN 2 ALSO "off"
                MOVE 0 TO SP-WHITELIST-ENABLE
                CALL "ServerProperties-Write" USING IO-FAILURE
                PERFORM HandleDiskFailure
                MOVE "Whitelist disabled" TO BUFFER

            WHEN 2 ALSO "list"
                IF WHITELIST-LENGTH > 0
                    *> allocate enough space for the message, using the fixed buffer if possible
                    COMPUTE DYN-BUFFER-LEN = 32 + WHITELIST-LENGTH * (LENGTH OF WHITELIST-NAME(1) + 2)
                    IF DYN-BUFFER-LEN > LENGTH OF BUFFER
                        ALLOCATE DYN-BUFFER-LEN CHARACTERS RETURNING BUFFER-ADDRESS
                        SET ADDRESS OF DYN-BUFFER TO BUFFER-ADDRESS
                    ELSE
                        SET ADDRESS OF DYN-BUFFER TO ADDRESS OF BUFFER
                    END-IF

                    *> no need to initialize the buffer as we only use it in the places we explicit set
                    MOVE WHITELIST-LENGTH TO TEMP-INT64-PIC
                    MOVE 1 TO BUFFER-POS
                    STRING "There are " FUNCTION TRIM(TEMP-INT64-PIC, LEADING) " whitelisted player(s): "
                            FUNCTION TRIM(WHITELIST-NAME(1), TRAILING)
                            INTO DYN-BUFFER WITH POINTER BUFFER-POS
                    PERFORM VARYING WHITELIST-INDEX FROM 2 BY 1 UNTIL WHITELIST-INDEX > WHITELIST-LENGTH
                        STRING ", " FUNCTION TRIM(WHITELIST-NAME(WHITELIST-INDEX), TRAILING)
                            INTO DYN-BUFFER WITH POINTER BUFFER-POS
                            *> note: we ensured that the buffer is always big enough, so no need for ON OVERFLOW here
                    END-PERFORM
                    SUBTRACT 1 FROM BUFFER-POS

                    CALL "SendChatMessage" USING LK-CLIENT-ID DYN-BUFFER OMITTED BUFFER-POS
                    IF DYN-BUFFER-LEN > LENGTH OF BUFFER
                        FREE BUFFER-ADDRESS
                    END-IF

                    GOBACK
                END-IF

                MOVE "There are no whitelisted players" TO BUFFER

            WHEN 3 ALSO "add"
                MOVE LK-PART-VALUE(3) TO TEMP-NAME
                CALL "Players-NameToUUID" USING TEMP-NAME TEMP-UUID

                CALL "Whitelist-Add" USING TEMP-UUID TEMP-NAME STATE-FAILURE IO-FAILURE
                PERFORM HandleDiskFailure

                INITIALIZE BUFFER
                IF STATE-FAILURE NOT = 0
                    STRING "Player is already whitelisted" INTO BUFFER
                ELSE
                    STRING "Added " FUNCTION TRIM(TEMP-NAME) " to the whitelist" INTO BUFFER
                END-IF

            WHEN 3 ALSO "remove"
                MOVE LK-PART-VALUE(3) TO TEMP-NAME
                CALL "Players-NameToUUID" USING TEMP-NAME TEMP-UUID

                CALL "Whitelist-Remove" USING TEMP-UUID TEMP-NAME STATE-FAILURE IO-FAILURE
                PERFORM HandleDiskFailure

                INITIALIZE BUFFER
                IF STATE-FAILURE NOT = 0
                    STRING "Player is not whitelisted" INTO BUFFER
                ELSE
                    STRING "Removed " FUNCTION TRIM(TEMP-NAME) " from the whitelist" INTO BUFFER
                END-IF

            WHEN OTHER
                MOVE 1 TO LK-PRINT-USAGE
                GOBACK
        END-EVALUATE

        CALL "SendChatMessage" USING LK-CLIENT-ID BUFFER
        GOBACK.

    HandleDiskFailure.
        IF IO-FAILURE NOT = 0
            CALL "SendChatMessage" USING LK-CLIENT-ID "Input/output error"
            GOBACK
        END-IF.

    END PROGRAM Callback-Execute.

END PROGRAM RegisterCommand-Whitelist.
