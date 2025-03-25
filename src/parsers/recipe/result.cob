*> --- Parse-Recipe-Result ---
*> Parse the "result" field of a recipe JSON object.
IDENTIFICATION DIVISION.
PROGRAM-ID. Parse-Recipe-Result.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 STR                          PIC X(128).
    01 AT-END                       BINARY-CHAR UNSIGNED.
    01 RESULT-NAME                  PIC X(128).
LINKAGE SECTION.
    01 LK-JSON                      PIC X ANY LENGTH.
    01 LK-OFFSET                    BINARY-LONG UNSIGNED.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.
    01 LK-RESULT.
        02 LK-RESULT-ID             BINARY-LONG.
        02 LK-RESULT-COUNT          BINARY-LONG.

PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE LK-RESULT.
    *> Recipe JSON files have the result as either an object (for most recipe types), or as a string (for some types).
    *> Objects have an "id" field but may be missing a "count" field (such as furnace recipes), so we default to 1.

    MOVE 1 TO LK-RESULT-COUNT

    CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE RESULT-NAME
    IF LK-FAILURE = 0
        PERFORM LookupId
        GOBACK
    END-IF

    CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE STR

        EVALUATE STR
            WHEN "count"
                CALL "JsonParse-Integer" USING LK-JSON LK-OFFSET LK-FAILURE LK-RESULT-COUNT
            WHEN "id"
                CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE RESULT-NAME
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING LK-JSON LK-OFFSET LK-FAILURE
        END-EVALUATE

        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET AT-END
        IF AT-END NOT = 0
            EXIT PERFORM
        END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF
    .

LookupId.
    CALL "Registries-Lookup" USING "minecraft:item" RESULT-NAME LK-RESULT-ID
    IF LK-RESULT-ID <= 0 OR LK-RESULT-COUNT <= 0
        DISPLAY "Invalid recipe result: " FUNCTION TRIM(LK-RESULT-ID) " x" FUNCTION TRIM(LK-RESULT-COUNT)
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF
    .

END PROGRAM Parse-Recipe-Result.
