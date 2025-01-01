*> --- Blocks-Parse ---
*> Parse the blocks.json file. Afterwards, the parsed contents are stored in the BLOCKS shared data structure.
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Parse.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-BLOCKS.
LOCAL-STORAGE SECTION.
    01 OFFSET           BINARY-LONG UNSIGNED    VALUE 1.
    01 EXIT-LOOP        BINARY-CHAR UNSIGNED    VALUE 0.
LINKAGE SECTION.
    01 LK-JSON          PIC X ANY LENGTH.
    01 LK-JSON-LEN      BINARY-LONG UNSIGNED.
    01 LK-FAILURE       BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-JSON LK-JSON-LEN LK-FAILURE.
    MOVE 0 TO BLOCKS-COUNT

    *> Expect start of the root object.
    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE

    *> Each key in the root object represents a block.
    PERFORM UNTIL LK-FAILURE NOT = 0 OR EXIT-LOOP = 1
        ADD 1 TO BLOCKS-COUNT
        CALL "Blocks-Parse-Block" USING LK-JSON OFFSET LK-FAILURE BLOCKS-COUNT

        *> Continue reading if there is a comma.
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
    END-PERFORM

    *> Expect end of the root object.
    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE

    *> Construct the by-name lookup table.
    PERFORM VARYING BLOCK-NAMES-INDEX FROM 1 BY 1 UNTIL BLOCK-NAMES-INDEX > BLOCKS-COUNT
        MOVE BLOCK-ENTRY-NAME(BLOCK-NAMES-INDEX) TO BLOCK-NAMES-ENTRY-NAME(BLOCK-NAMES-INDEX)
        MOVE BLOCK-NAMES-INDEX TO BLOCK-NAMES-ENTRY-INDEX(BLOCK-NAMES-INDEX)
    END-PERFORM
    SORT BLOCK-NAMES

    GOBACK.

    *> --- Blocks-Parse-Block ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Blocks-Parse-Block.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 EXIT-LOOP        BINARY-CHAR UNSIGNED.
        01 OBJECT-KEY       PIC X(50).
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
        MOVE 0 TO BLOCK-ENTRY-PROPERTY-COUNT(LK-BLOCK)
        MOVE 999999999 TO BLOCK-ENTRY-MINIMUM-STATE-ID(LK-BLOCK)
        MOVE 0 TO BLOCK-ENTRY-MAXIMUM-STATE-ID(LK-BLOCK)
        MOVE 0 TO BLOCK-ENTRY-DEFAULT-STATE-ID(LK-BLOCK)

        *> Read the block name.
        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE BLOCK-ENTRY-NAME(LK-BLOCK)

        *> Expect start of the block object.
        CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE

        *> Loop over each key in the object.
        PERFORM UNTIL LK-FAILURE NOT = 0
            *> Read the key.
            CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            EVALUATE OBJECT-KEY
                WHEN "definition"
                    *> Expect the start of the definition object.
                    CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE

                    *> Loop over each key in the definition object.
                    PERFORM UNTIL LK-FAILURE NOT = 0
                        *> Read the key.
                        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
                        IF LK-FAILURE NOT = 0
                            GOBACK
                        END-IF

                        *> We care about the "type" key.
                        EVALUATE OBJECT-KEY
                            WHEN "type"
                                CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE BLOCK-ENTRY-TYPE(LK-BLOCK)
                            WHEN OTHER
                                CALL "JsonParse-SkipValue" USING LK-JSON LK-OFFSET LK-FAILURE
                        END-EVALUATE

                        *> Check if there is a comma; if not, exit the loop.
                        CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
                        IF EXIT-LOOP = 1
                            EXIT PERFORM
                        END-IF
                    END-PERFORM

                    *> Expect the end of the definition object.
                    CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE

                WHEN "properties"
                    *> Expect the start of the properties object.
                    CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE

                    *> Loop over each key in the properties object, each representing a block property.
                    PERFORM UNTIL LK-FAILURE NOT = 0
                        *> Read the property.
                        ADD 1 TO BLOCK-ENTRY-PROPERTY-COUNT(LK-BLOCK)
                        CALL "Blocks-Parse-Property" USING LK-JSON LK-OFFSET LK-FAILURE BLOCK-ENTRY-PROPERTY(LK-BLOCK, BLOCK-ENTRY-PROPERTY-COUNT(LK-BLOCK))

                        *> Check if there is a comma; if not, exit the loop.
                        CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
                        IF EXIT-LOOP = 1
                            EXIT PERFORM
                        END-IF
                    END-PERFORM

                    *> Expect the end of the properties object.
                    CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE

                WHEN "states"
                    *> Expect the start of the states array.
                    CALL "JsonParse-ArrayStart" USING LK-JSON LK-OFFSET LK-FAILURE

                    *> Loop over each key in the states array, each representing a block state.
                    PERFORM UNTIL LK-FAILURE NOT = 0
                        *> Read the state.
                        CALL "Blocks-Parse-State" USING LK-JSON LK-OFFSET LK-FAILURE STATE-ID IS-DEFAULT

                        MOVE FUNCTION MIN(BLOCK-ENTRY-MINIMUM-STATE-ID(LK-BLOCK), STATE-ID) TO BLOCK-ENTRY-MINIMUM-STATE-ID(LK-BLOCK)
                        MOVE FUNCTION MAX(BLOCK-ENTRY-MAXIMUM-STATE-ID(LK-BLOCK), STATE-ID) TO BLOCK-ENTRY-MAXIMUM-STATE-ID(LK-BLOCK)
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

                WHEN OTHER
                    CALL "JsonParse-SkipValue" USING LK-JSON LK-OFFSET LK-FAILURE
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
                02 LK-PROPERTY-NAME         PIC X(32).
                02 LK-PROPERTY-VALUE-COUNT  BINARY-LONG UNSIGNED.
                02 LK-PROPERTY-VALUE        PIC X(32) OCCURS 32 TIMES.

        PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE LK-PROPERTY.
            MOVE 0 TO LK-PROPERTY-VALUE-COUNT

            *> Read the property name.
            CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE LK-PROPERTY-NAME

            *> Expect the start of the property value array.
            CALL "JsonParse-ArrayStart" USING LK-JSON LK-OFFSET LK-FAILURE

            *> Loop over each value in the array.
            PERFORM UNTIL LK-FAILURE NOT = 0
                *> Read the value.
                ADD 1 TO LK-PROPERTY-VALUE-COUNT
                CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE LK-PROPERTY-VALUE(LK-PROPERTY-VALUE-COUNT)

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
            01 OBJECT-KEY           PIC X(50).
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

*> --- Blocks-GetCount ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-GetCount.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-COUNT         BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-COUNT.
    MOVE BLOCKS-COUNT TO LK-COUNT
    GOBACK.

END PROGRAM Blocks-GetCount.

*> --- Blocks-Iterate-Name ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Iterate-Name.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-INDEX         BINARY-LONG UNSIGNED.
    01 LK-NAME          PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-INDEX LK-NAME.
    MOVE BLOCK-ENTRY-NAME(LK-INDEX) TO LK-NAME
    GOBACK.

END PROGRAM Blocks-Iterate-Name.

*> --- Blocks-Iterate-Type ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Iterate-Type.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-INDEX         BINARY-LONG UNSIGNED.
    01 LK-TYPE          PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-INDEX LK-TYPE.
    MOVE BLOCK-ENTRY-TYPE(LK-INDEX) TO LK-TYPE
    GOBACK.

END PROGRAM Blocks-Iterate-Type.

*> --- Blocks-Iterate-StateIds ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Iterate-StateIds.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-INDEX         BINARY-LONG UNSIGNED.
    01 LK-MINIMUM-ID    BINARY-LONG.
    01 LK-MAXIMUM-ID    BINARY-LONG.

PROCEDURE DIVISION USING LK-INDEX LK-MINIMUM-ID LK-MAXIMUM-ID.
    MOVE BLOCK-ENTRY-MINIMUM-STATE-ID(LK-INDEX) TO LK-MINIMUM-ID
    MOVE BLOCK-ENTRY-MAXIMUM-STATE-ID(LK-INDEX) TO LK-MAXIMUM-ID
    GOBACK.

END PROGRAM Blocks-Iterate-StateIds.

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
    COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==LK-STATE==.

PROCEDURE DIVISION USING LK-STATE-ID LK-STATE-DESCRIPTION.
    *> TODO optimize this operation (e.g. binary search)
    MOVE SPACES TO LK-STATE-DESCRIPTION
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCKS-COUNT
        *> Skip the block entirely if the state ID is out of range. This works because state IDs are contiguous.
        IF LK-STATE-ID >= BLOCK-ENTRY-MINIMUM-STATE-ID(BLOCK-INDEX) AND LK-STATE-ID <= BLOCK-ENTRY-MAXIMUM-STATE-ID(BLOCK-INDEX)
            MOVE BLOCK-ENTRY-NAME(BLOCK-INDEX) TO LK-STATE-NAME
            MOVE BLOCK-ENTRY-PROPERTY-COUNT(BLOCK-INDEX) TO LK-STATE-PROPERTY-COUNT
            *> The property values are computed based on the block state's position within the block's states range.
            COMPUTE PROPERTY-CODE = LK-STATE-ID - BLOCK-ENTRY-MINIMUM-STATE-ID(BLOCK-INDEX)
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
    COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==LK-STATE==.
    01 LK-STATE-ID      BINARY-LONG.

PROCEDURE DIVISION USING LK-STATE-DESCRIPTION LK-STATE-ID.
    MOVE -1 TO LK-STATE-ID

    *> Binary search over the block names, to find the index into the primary table.
    SEARCH ALL BLOCK-NAMES
        WHEN BLOCK-NAMES-ENTRY-NAME(BLOCK-NAMES-INDEX) = LK-STATE-NAME
            MOVE BLOCK-NAMES-ENTRY-INDEX(BLOCK-NAMES-INDEX) TO BLOCK-INDEX
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
    END-SEARCH

    GOBACK.

END PROGRAM Blocks-Get-StateId.

*> --- Blocks-Get-Name ---
*> More efficient variant of Blocks-Get-StateDescription that only retrieves the block name.
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Get-Name.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-STATE-ID          BINARY-LONG.
    01 LK-STATE-NAME        PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-STATE-ID LK-STATE-NAME.
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCKS-COUNT
        IF LK-STATE-ID >= BLOCK-ENTRY-MINIMUM-STATE-ID(BLOCK-INDEX) AND LK-STATE-ID <= BLOCK-ENTRY-MAXIMUM-STATE-ID(BLOCK-INDEX)
            MOVE BLOCK-ENTRY-NAME(BLOCK-INDEX) TO LK-STATE-NAME
            GOBACK
        END-IF
    END-PERFORM
    MOVE SPACES TO LK-STATE-NAME
    GOBACK.

END PROGRAM Blocks-Get-Name.

*> --- Blocks-CompareBlockType ---
*> Check if two block state IDs belong to the same block type.
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-CompareBlockType.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BLOCK-INDEX      BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-BLOCKS.
LINKAGE SECTION.
    01 LK-STATE-ID-1    BINARY-LONG.
    01 LK-STATE-ID-2    BINARY-LONG.
    01 LK-RESULT        BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-STATE-ID-1 LK-STATE-ID-2 LK-RESULT.
    IF LK-STATE-ID-1 = LK-STATE-ID-2
        MOVE 1 TO LK-RESULT
        GOBACK
    END-IF
    IF LK-STATE-ID-1 = 0 OR LK-STATE-ID-2 = 0
        MOVE 0 TO LK-RESULT
        GOBACK
    END-IF
    *> TODO optimize this operation
    MOVE 0 TO LK-RESULT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCKS-COUNT
        IF LK-STATE-ID-1 >= BLOCK-ENTRY-MINIMUM-STATE-ID(BLOCK-INDEX) AND LK-STATE-ID-1 <= BLOCK-ENTRY-MAXIMUM-STATE-ID(BLOCK-INDEX)
            IF LK-STATE-ID-2 >= BLOCK-ENTRY-MINIMUM-STATE-ID(BLOCK-INDEX) AND LK-STATE-ID-2 <= BLOCK-ENTRY-MAXIMUM-STATE-ID(BLOCK-INDEX)
                MOVE 1 TO LK-RESULT
            END-IF
            EXIT PERFORM
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Blocks-CompareBlockType.

*> --- Blocks-Description-GetValue ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Description-GetValue.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PROPERTY-INDEX               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==LK-STATE==.
    01 LK-PROPERTY-NAME             PIC X ANY LENGTH.
    01 LK-PROPERTY-VALUE            PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-STATE-DESCRIPTION LK-PROPERTY-NAME LK-PROPERTY-VALUE.
    MOVE SPACES TO LK-PROPERTY-VALUE
    PERFORM VARYING PROPERTY-INDEX FROM 1 BY 1 UNTIL PROPERTY-INDEX > LK-STATE-PROPERTY-COUNT
        IF LK-PROPERTY-NAME = LK-STATE-PROPERTY-NAME(PROPERTY-INDEX)
            MOVE LK-STATE-PROPERTY-VALUE(PROPERTY-INDEX) TO LK-PROPERTY-VALUE
            EXIT PERFORM
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Blocks-Description-GetValue.

*> --- Blocks-Description-SetValue ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Blocks-Description-SetValue.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PROPERTY-INDEX               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==LK-STATE==.
    01 LK-PROPERTY-NAME             PIC X ANY LENGTH.
    01 LK-PROPERTY-VALUE            PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-STATE-DESCRIPTION LK-PROPERTY-NAME LK-PROPERTY-VALUE.
    PERFORM VARYING PROPERTY-INDEX FROM 1 BY 1 UNTIL PROPERTY-INDEX > LK-STATE-PROPERTY-COUNT
        IF LK-PROPERTY-NAME = LK-STATE-PROPERTY-NAME(PROPERTY-INDEX)
            MOVE LK-PROPERTY-VALUE TO LK-STATE-PROPERTY-VALUE(PROPERTY-INDEX)
            EXIT PERFORM
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Blocks-Description-SetValue.
