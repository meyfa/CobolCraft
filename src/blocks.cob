*> --- Blocks-Parse ---
*> Parse the blocks.json file. Afterwards, the parsed contents are stored in the BLOCKS shared data structure.
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Parse.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 OFFSET           BINARY-LONG UNSIGNED.
    01 EXIT-LOOP        BINARY-CHAR UNSIGNED.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-JSON          PIC X ANY LENGTH.
    01 LK-JSON-LEN      BINARY-LONG UNSIGNED.
    01 LK-FAILURE       BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-JSON LK-JSON-LEN LK-FAILURE.
    MOVE 0 TO BLOCKS-COUNT

    MOVE 1 TO OFFSET

    *> Expect start of the root object.
    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    *> Loop over each key in the root object, each representing a block.
    PERFORM UNTIL EXIT
        *> Read the block.
        ADD 1 TO BLOCKS-COUNT
        CALL "Blocks-Parse-Block" USING LK-JSON OFFSET LK-FAILURE BLOCKS-COUNT
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        *> Check if there is a comma; if not, exit the loop.
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
        IF EXIT-LOOP = 1
            EXIT PERFORM
        END-IF
    END-PERFORM

    *> Expect end of the root object.
    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE

    GOBACK.

    *> --- Blocks-Parse-Block ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Blocks-Parse-Block.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 EXIT-LOOP        BINARY-CHAR UNSIGNED.
        01 OBJECT-KEY       PIC X(100).
        *> shared data
        COPY DD-BLOCKS.
    LINKAGE SECTION.
        01 LK-JSON          PIC X ANY LENGTH.
        01 LK-OFFSET        BINARY-LONG UNSIGNED.
        01 LK-FAILURE       BINARY-CHAR UNSIGNED.
        01 LK-BLOCK         BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE LK-BLOCK.
        MOVE 0 TO BLOCK-ENTRY-STATES-COUNT(LK-BLOCK)

        *> Read the block name.
        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE BLOCK-ENTRY-NAME(LK-BLOCK)
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        *> Expect start of the block object.
        CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        *> Loop over each key in the object. We only care about the "states" key.
        PERFORM UNTIL EXIT
            *> Read the key.
            CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            EVALUATE OBJECT-KEY
                WHEN "states"
                    *> Expect the start of the states array.
                    CALL "JsonParse-ArrayStart" USING LK-JSON LK-OFFSET LK-FAILURE
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF

                    *> Loop over each key in the states array, each representing a block state.
                    PERFORM UNTIL EXIT
                        *> Read the state.
                        ADD 1 TO BLOCK-ENTRY-STATES-COUNT(LK-BLOCK)
                        CALL "Blocks-Parse-State" USING LK-JSON LK-OFFSET LK-FAILURE BLOCK-ENTRY-STATE(LK-BLOCK, BLOCK-ENTRY-STATES-COUNT(LK-BLOCK))
                        IF LK-FAILURE NOT = 0
                            GOBACK
                        END-IF

                        *> If the state was marked as default, store its index for fast lookup.
                        MOVE BLOCK-ENTRY-STATES-COUNT(LK-BLOCK) TO BLOCK-ENTRY-DEFAULT-STATE(LK-BLOCK)

                        *> Check if there is a comma; if not, exit the loop.
                        CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
                        IF EXIT-LOOP = 1
                            EXIT PERFORM
                        END-IF
                    END-PERFORM

                    *> Expect the end of the states array.
                    CALL "JsonParse-ArrayEnd" USING LK-JSON LK-OFFSET LK-FAILURE
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF

                WHEN OTHER
                    CALL "JsonParse-SkipValue" USING LK-JSON LK-OFFSET LK-FAILURE
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF
            END-EVALUATE

            *> Check if there is a comma; if not, exit the loop.
            CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
            IF EXIT-LOOP = 1
                EXIT PERFORM
            END-IF
        END-PERFORM

        *> Expect the end of the block object.
        CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE

        GOBACK.

        *> --- Blocks-Parse-State ---
        IDENTIFICATION DIVISION.
        PROGRAM-ID. Blocks-Parse-State.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
            01 EXIT-LOOP        BINARY-CHAR UNSIGNED.
            01 OBJECT-KEY       PIC X(100).
        LINKAGE SECTION.
            01 LK-JSON          PIC X ANY LENGTH.
            01 LK-OFFSET        BINARY-LONG UNSIGNED.
            01 LK-FAILURE       BINARY-CHAR UNSIGNED.
            01 LK-BLOCK-STATE.
                02 LK-BLOCK-STATE-ID BINARY-LONG UNSIGNED.
                02 LK-BLOCK-STATE-IS-DEFAULT BINARY-CHAR UNSIGNED.

        PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE LK-BLOCK-STATE.
            MOVE 0 TO LK-BLOCK-STATE-IS-DEFAULT

            *> Expect the start of the state object.
            CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            *> Loop over each key looking for "id" and "default".
            PERFORM UNTIL EXIT
                *> Read the key.
                CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
                IF LK-FAILURE NOT = 0
                    GOBACK
                END-IF

                EVALUATE OBJECT-KEY
                    WHEN "id"
                        CALL "JsonParse-Integer" USING LK-JSON LK-OFFSET LK-FAILURE LK-BLOCK-STATE-ID

                    WHEN "default"
                        CALL "JsonParse-Boolean" USING LK-JSON LK-OFFSET LK-FAILURE LK-BLOCK-STATE-IS-DEFAULT

                    WHEN OTHER
                        CALL "JsonParse-SkipValue" USING LK-JSON LK-OFFSET LK-FAILURE
                END-EVALUATE

                IF LK-FAILURE NOT = 0
                    GOBACK
                END-IF

                *> Check if there is a comma; if not, exit the loop.
                CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
                IF EXIT-LOOP = 1
                    EXIT PERFORM
                END-IF
            END-PERFORM

            *> Expect the end of the state object.
            CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE

            GOBACK.

        END PROGRAM Blocks-Parse-State.

    END PROGRAM Blocks-Parse-Block.

END PROGRAM Blocks-Parse.

*> --- Blocks-Get-DefaultStateId ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Get-DefaultStateId.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BLOCK-INDEX      BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-BLOCK-NAME    PIC X ANY LENGTH.
    01 LK-STATE-ID      BINARY-LONG.

PROCEDURE DIVISION USING LK-BLOCK-NAME LK-STATE-ID.
    MOVE -1 TO LK-STATE-ID
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCKS-COUNT
        IF LK-BLOCK-NAME = BLOCK-ENTRY-NAME(BLOCK-INDEX)
            IF BLOCK-ENTRY-DEFAULT-STATE(BLOCK-INDEX) > 0
                MOVE BLOCK-ENTRY-STATE-ID(BLOCK-INDEX, BLOCK-ENTRY-DEFAULT-STATE(BLOCK-INDEX)) TO LK-STATE-ID
            END-IF
            EXIT PERFORM
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Blocks-Get-DefaultStateId.
