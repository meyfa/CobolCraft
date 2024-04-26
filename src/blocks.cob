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
        01 STATE-ID         BINARY-LONG UNSIGNED.
        01 IS-DEFAULT       BINARY-CHAR UNSIGNED.
        *> shared data
        COPY DD-BLOCKS.
    LINKAGE SECTION.
        01 LK-JSON          PIC X ANY LENGTH.
        01 LK-OFFSET        BINARY-LONG UNSIGNED.
        01 LK-FAILURE       BINARY-CHAR UNSIGNED.
        01 LK-BLOCK         BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE LK-BLOCK.
        MOVE 999999999 TO BLOCK-ENTRY-MINIMUM-STATE-ID(LK-BLOCK)
        MOVE 0 TO BLOCK-ENTRY-MAXIMUM-STATE-ID(LK-BLOCK)
        MOVE 0 TO BLOCK-ENTRY-DEFAULT-STATE-ID(LK-BLOCK)

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

        *> Loop over each key in the object.
        PERFORM UNTIL EXIT
            *> Read the key.
            CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            EVALUATE OBJECT-KEY
                WHEN "properties"
                    *> Expect the start of the properties object.
                    CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF

                    *> Loop over each key in the properties object, each representing a block property.
                    PERFORM UNTIL EXIT
                        *> Read the property.
                        ADD 1 TO BLOCK-ENTRY-PROPERTY-COUNT(LK-BLOCK)
                        CALL "Blocks-Parse-Property" USING LK-JSON LK-OFFSET LK-FAILURE BLOCK-ENTRY-PROPERTY(LK-BLOCK, BLOCK-ENTRY-PROPERTY-COUNT(LK-BLOCK))
                        IF LK-FAILURE NOT = 0
                            GOBACK
                        END-IF

                        *> Check if there is a comma; if not, exit the loop.
                        CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
                        IF EXIT-LOOP = 1
                            EXIT PERFORM
                        END-IF
                    END-PERFORM

                    *> Expect the end of the properties object.
                    CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF

                WHEN "states"
                    *> Expect the start of the states array.
                    CALL "JsonParse-ArrayStart" USING LK-JSON LK-OFFSET LK-FAILURE
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF

                    *> Loop over each key in the states array, each representing a block state.
                    PERFORM UNTIL EXIT
                        *> Read the state.
                        CALL "Blocks-Parse-State" USING LK-JSON LK-OFFSET LK-FAILURE STATE-ID IS-DEFAULT
                        IF LK-FAILURE NOT = 0
                            GOBACK
                        END-IF

                        COMPUTE BLOCK-ENTRY-MINIMUM-STATE-ID(LK-BLOCK) = FUNCTION MIN(BLOCK-ENTRY-MINIMUM-STATE-ID(LK-BLOCK), STATE-ID)
                        COMPUTE BLOCK-ENTRY-MAXIMUM-STATE-ID(LK-BLOCK) = FUNCTION MAX(BLOCK-ENTRY-MAXIMUM-STATE-ID(LK-BLOCK), STATE-ID)
                        IF IS-DEFAULT > 0
                            MOVE STATE-ID TO BLOCK-ENTRY-DEFAULT-STATE-ID(LK-BLOCK)
                        END-IF

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

        *> --- Blocks-Parse-Property ---
        IDENTIFICATION DIVISION.
        PROGRAM-ID. Blocks-Parse-Property.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
            01 EXIT-LOOP        BINARY-CHAR UNSIGNED.
        LINKAGE SECTION.
            01 LK-JSON          PIC X ANY LENGTH.
            01 LK-OFFSET        BINARY-LONG UNSIGNED.
            01 LK-FAILURE       BINARY-CHAR UNSIGNED.
            01 LK-PROPERTY.
                02 LK-PROPERTY-NAME         PIC X(64).
                02 LK-PROPERTY-VALUE-COUNT  BINARY-LONG UNSIGNED.
                02 LK-PROPERTY-VALUE OCCURS 32 TIMES.
                    03 LK-PROPERTY-VALUE-NAME  PIC X(64).

        PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE LK-PROPERTY.
            *> Read the property name.
            CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE LK-PROPERTY-NAME
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            *> Expect the start of the property value array.
            CALL "JsonParse-ArrayStart" USING LK-JSON LK-OFFSET LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            *> Loop over each value in the array.
            PERFORM UNTIL EXIT
                *> Read the value.
                ADD 1 TO LK-PROPERTY-VALUE-COUNT
                CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE LK-PROPERTY-VALUE(LK-PROPERTY-VALUE-COUNT)
                IF LK-FAILURE NOT = 0
                    GOBACK
                END-IF

                *> Check if there is a comma; if not, exit the loop.
                CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
                IF EXIT-LOOP = 1
                    EXIT PERFORM
                END-IF
            END-PERFORM

            *> Expect the end of the property value array.
            CALL "JsonParse-ArrayEnd" USING LK-JSON LK-OFFSET LK-FAILURE

            GOBACK.

        END PROGRAM Blocks-Parse-Property.

        *> --- Blocks-Parse-State ---
        IDENTIFICATION DIVISION.
        PROGRAM-ID. Blocks-Parse-State.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
            01 EXIT-LOOP            BINARY-CHAR UNSIGNED.
            01 OBJECT-KEY           PIC X(100).
        LINKAGE SECTION.
            01 LK-JSON              PIC X ANY LENGTH.
            01 LK-OFFSET            BINARY-LONG UNSIGNED.
            01 LK-FAILURE           BINARY-CHAR UNSIGNED.
            01 LK-BLOCK-STATE-ID    BINARY-LONG UNSIGNED.
            01 LK-IS-DEFAULT        BINARY-CHAR UNSIGNED.

        PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE LK-BLOCK-STATE-ID LK-IS-DEFAULT.
            MOVE 0 TO LK-IS-DEFAULT

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
                        CALL "JsonParse-Boolean" USING LK-JSON LK-OFFSET LK-FAILURE LK-IS-DEFAULT

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
            MOVE BLOCK-ENTRY-DEFAULT-STATE-ID(BLOCK-INDEX) TO LK-STATE-ID
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Blocks-Get-DefaultStateId.

*> --- Blocks-Get-StateDescription ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Get-StateDescription.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
    01 PROPERTY-INDEX       BINARY-LONG UNSIGNED.
    01 PROPERTY-CODE        BINARY-LONG UNSIGNED.
    01 PROPERTY-VALUE-INDEX BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-STATE-ID          BINARY-LONG.
    01 LK-STATE-DESCRIPTION.
        02 LK-STATE-NAME            PIC X(64).
        02 LK-STATE-PROPERTY-COUNT  BINARY-LONG UNSIGNED.
        02 LK-STATE-PROPERTY OCCURS 16 TIMES.
            03 LK-STATE-PROPERTY-NAME   PIC X(64).
            03 LK-STATE-PROPERTY-VALUE  PIC X(64).

PROCEDURE DIVISION USING LK-STATE-ID LK-STATE-DESCRIPTION.
    *> TODO optimize this operation (e.g. binary search)
    MOVE SPACES TO LK-STATE-DESCRIPTION
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCKS-COUNT
        *> Skip the block entirely if the state ID is out of range. This works because state IDs are contiguous.
        IF LK-STATE-ID >= BLOCK-ENTRY-MINIMUM-STATE-ID(BLOCK-INDEX) AND LK-STATE-ID <= BLOCK-ENTRY-MAXIMUM-STATE-ID(BLOCK-INDEX)
            MOVE BLOCK-ENTRY-NAME(BLOCK-INDEX) TO LK-STATE-NAME
            MOVE BLOCK-ENTRY-PROPERTY-COUNT(BLOCK-INDEX) TO LK-STATE-PROPERTY-COUNT
            *> The property values are computed based on the block state's position within the block's states range.
            COMPUTE PROPERTY-CODE = LK-STATE-ID - BLOCK-ENTRY-MINIMUM-STATE-ID(BLOCK-INDEX) - 1
            PERFORM VARYING PROPERTY-INDEX FROM LK-STATE-PROPERTY-COUNT BY -1 UNTIL PROPERTY-INDEX < 1
                MOVE BLOCK-ENTRY-PROPERTY-NAME(BLOCK-INDEX, PROPERTY-INDEX) TO LK-STATE-PROPERTY-NAME(PROPERTY-INDEX)
                DIVIDE PROPERTY-CODE BY BLOCK-ENTRY-PROPERTY-VALUE-COUNT(BLOCK-INDEX, PROPERTY-INDEX) GIVING PROPERTY-CODE REMAINDER PROPERTY-VALUE-INDEX
                MOVE BLOCK-ENTRY-PROPERTY-VALUE(BLOCK-INDEX, PROPERTY-INDEX, PROPERTY-VALUE-INDEX + 1) TO LK-STATE-PROPERTY-VALUE(PROPERTY-INDEX)
            END-PERFORM
            EXIT PERFORM
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Blocks-Get-StateDescription.

*> --- Blocks-Get-StateId ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Get-StateId.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BLOCK-INDEX      BINARY-LONG UNSIGNED.
    01 PROPERTY-CODE    BINARY-LONG UNSIGNED.
    01 PROPERTY-INDEX   BINARY-LONG UNSIGNED.
    01 PROPERTY-INDEX-2 BINARY-LONG UNSIGNED.
    01 VALUE-INDEX      BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-STATE-DESCRIPTION.
        02 LK-STATE-NAME            PIC X(64).
        02 LK-STATE-PROPERTY-COUNT  BINARY-LONG UNSIGNED.
        02 LK-STATE-PROPERTY OCCURS 16 TIMES.
            03 LK-STATE-PROPERTY-NAME   PIC X(64).
            03 LK-STATE-PROPERTY-VALUE  PIC X(64).
    01 LK-STATE-ID      BINARY-LONG.

PROCEDURE DIVISION USING LK-STATE-DESCRIPTION LK-STATE-ID.
    *> TODO optimize this operation
    MOVE -1 TO LK-STATE-ID
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCKS-COUNT
        IF LK-STATE-NAME = BLOCK-ENTRY-NAME(BLOCK-INDEX)
            MOVE BLOCK-ENTRY-DEFAULT-STATE-ID(BLOCK-INDEX) TO LK-STATE-ID
            *> Compute the property code (= offset within the block's states array) based on the property values.
            MOVE 0 TO PROPERTY-CODE
            *> Loop over each expected property.
            PERFORM VARYING PROPERTY-INDEX FROM 1 BY 1 UNTIL PROPERTY-INDEX > BLOCK-ENTRY-PROPERTY-COUNT(BLOCK-INDEX)
                COMPUTE PROPERTY-CODE = PROPERTY-CODE * BLOCK-ENTRY-PROPERTY-VALUE-COUNT(BLOCK-INDEX, PROPERTY-INDEX)
                *> Find the property in the state description. If not found, the first value is assumed.
                PERFORM VARYING PROPERTY-INDEX-2 FROM 1 BY 1 UNTIL PROPERTY-INDEX-2 > LK-STATE-PROPERTY-COUNT
                    IF BLOCK-ENTRY-PROPERTY-NAME(BLOCK-INDEX, PROPERTY-INDEX) = LK-STATE-PROPERTY-NAME(PROPERTY-INDEX-2)
                        PERFORM VARYING VALUE-INDEX FROM 1 BY 1 UNTIL VALUE-INDEX > BLOCK-ENTRY-PROPERTY-VALUE-COUNT(BLOCK-INDEX, PROPERTY-INDEX)
                            IF BLOCK-ENTRY-PROPERTY-VALUE(BLOCK-INDEX, PROPERTY-INDEX, VALUE-INDEX) = LK-STATE-PROPERTY-VALUE(PROPERTY-INDEX-2)
                                COMPUTE PROPERTY-CODE = PROPERTY-CODE + (VALUE-INDEX - 1)
                            END-IF
                        END-PERFORM
                        EXIT PERFORM
                    END-IF
                END-PERFORM
            END-PERFORM
            COMPUTE LK-STATE-ID = BLOCK-ENTRY-MINIMUM-STATE-ID(BLOCK-INDEX) + PROPERTY-CODE
            EXIT PERFORM
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Blocks-Get-StateId.
