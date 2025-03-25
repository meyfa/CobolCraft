*> --- Parse-Recipe-Shaped ---
*> Parse a recipe of type "minecraft:crafting_shaped" from JSON.
IDENTIFICATION DIVISION.
PROGRAM-ID. Parse-Recipe-Shaped.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-RECIPES.
    COPY DD-TAGS.
    *> index of the item registry in the tags table (initialized on first use)
    01 TAGS-ITEM-REGISTRY-INDEX     BINARY-LONG UNSIGNED        VALUE 0.
    01 AT-END                       BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(128).
    01 TEMP-INT32                   BINARY-LONG.
    01 TAG-INDEX                    BINARY-LONG UNSIGNED.
    01 MULTI-CHOICE-SLOTS           BINARY-CHAR UNSIGNED.
    01 MULTI-CHOICE-SLOT-INDEX      BINARY-CHAR UNSIGNED.
    01 ROW-INDEX                    BINARY-LONG UNSIGNED.
    01 COL-INDEX                    BINARY-LONG UNSIGNED.
    01 KEY-CHAR                     PIC X.
    01 KEY-INDEX                    BINARY-LONG UNSIGNED.
    01 CHOICE-INDEX                 BINARY-LONG UNSIGNED.
    01 INPUT-INDEX                  BINARY-LONG UNSIGNED.
    01 RECIPE-WIDTH                 BINARY-LONG UNSIGNED.
    01 RECIPE-SYMMETRICAL           BINARY-CHAR UNSIGNED.
LOCAL-STORAGE SECTION.
    *> result
    01 RESULT.
        02 RESULT-ID                BINARY-LONG                 VALUE 0.
        02 RESULT-COUNT             BINARY-LONG                 VALUE 0.
    *> key
    01 PATTERN-KEYS.
        02 PATTERN-KEY-COUNT        BINARY-LONG UNSIGNED        VALUE 0.
        02 PATTERN-KEY              OCCURS 9 TIMES.
            03 PATTERN-KEY-CHAR     PIC X.
            *> each key can resolve to multiple items (e.g., when the identifier is a tag).
            03 PATTERN-KEY-OPTIONS  BINARY-LONG UNSIGNED        VALUE 0.
            03 PATTERN-KEY-IDS.
                04 PATTERN-KEY-ID   OCCURS TAGS-PER-REGISTRY-CAPACITY TIMES BINARY-LONG.
    *> pattern; first as text (such as rows like "/_/" or "## "), then as resolved item IDs
    01 PATTERN-TEXT.
        02 PATTERN-TEXT-ROW         OCCURS 3 TIMES PIC X(3).
    01 PATTERN-SLOTS.
        02 PATTERN-SLOT             OCCURS 9 TIMES.
            03 PATTERN-SLOT-OPTIONS BINARY-LONG UNSIGNED        VALUE 0.
            03 PATTERN-SLOT-IDS.
                04 PATTERN-SLOT-ID  OCCURS TAGS-PER-REGISTRY-CAPACITY TIMES BINARY-LONG.
LINKAGE SECTION.
    01 LK-JSON                      PIC X ANY LENGTH.
    01 LK-OFFSET                    BINARY-LONG UNSIGNED.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE.
    IF TAGS-ITEM-REGISTRY-INDEX = 0
        PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-REGISTRY-COUNT
            IF TAGS-REGISTRY-NAME(TAG-INDEX) = "minecraft:item"
                MOVE TAG-INDEX TO TAGS-ITEM-REGISTRY-INDEX
                EXIT PERFORM
            END-IF
        END-PERFORM
    END-IF

    CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE STR
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        EVALUATE STR
            WHEN "key"
                PERFORM ParseKey
            WHEN "pattern"
                PERFORM ParsePattern
            WHEN "result"
                CALL "Parse-Recipe-Result" USING LK-JSON LK-OFFSET LK-FAILURE RESULT
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

    *> Convert text pattern to ID sets using the key. At the same time, count the number of slots with multiple
    *> choices.
    MOVE 0 TO MULTI-CHOICE-SLOTS

    PERFORM VARYING INPUT-INDEX FROM 1 BY 1 UNTIL INPUT-INDEX > 9
        MOVE PATTERN-TEXT(INPUT-INDEX:1) TO KEY-CHAR
        IF KEY-CHAR = " "
            *> one option: air
            MOVE 1 TO PATTERN-SLOT-OPTIONS(INPUT-INDEX)
            MOVE 0 TO PATTERN-SLOT-ID(INPUT-INDEX, 1)
        ELSE
            *> lookup the key
            PERFORM VARYING KEY-INDEX FROM 1 BY 1 UNTIL KEY-INDEX > PATTERN-KEY-COUNT
                IF PATTERN-KEY-CHAR(KEY-INDEX) = KEY-CHAR
                    EXIT PERFORM
                END-IF
            END-PERFORM
            IF KEY-INDEX > PATTERN-KEY-COUNT
                MOVE 1 TO LK-FAILURE
                GOBACK
            END-IF

            *> copy the key's ID set to the pattern slot
            MOVE PATTERN-KEY-OPTIONS(KEY-INDEX) TO PATTERN-SLOT-OPTIONS(INPUT-INDEX)
            MOVE PATTERN-KEY-IDS(KEY-INDEX) TO PATTERN-SLOT-IDS(INPUT-INDEX)

            IF PATTERN-KEY-OPTIONS(KEY-INDEX) > 1
                ADD 1 TO MULTI-CHOICE-SLOTS
                MOVE INPUT-INDEX TO MULTI-CHOICE-SLOT-INDEX
            END-IF
        END-IF
    END-PERFORM

    *> Since we may have multiple keys that each resolve to one or more items, we would need to compute the cartesian
    *> product of all the item sets and register a recipe each.
    *> While possible in theory (e.g., iteratively incrementing an index array), the combinatorial explosion makes this
    *> approach infeasible: for example, the barrel consists of 6 planks and 2 slabs, which may each be one of 12
    *> variants (in 1.21.4), resulting in 12^8 = 429,981,696 recipes.

    EVALUATE MULTI-CHOICE-SLOTS
        WHEN 0 *> easy! all slots are single-item
            ADD 1 TO RECIPES-SHAPED-COUNT
            PERFORM VARYING INPUT-INDEX FROM 1 BY 1 UNTIL INPUT-INDEX > 9
                MOVE PATTERN-SLOT-ID(INPUT-INDEX, 1) TO RECIPE-SHAPED-INPUT(RECIPES-SHAPED-COUNT, INPUT-INDEX)
            END-PERFORM
            MOVE RESULT-ID TO RECIPE-SHAPED-OUTPUT-ID(RECIPES-SHAPED-COUNT)
            MOVE RESULT-COUNT TO RECIPE-SHAPED-OUTPUT-COUNT(RECIPES-SHAPED-COUNT)

            IF RECIPE-SYMMETRICAL = 0
                PERFORM MirrorShapedRecipe
            END-IF

        WHEN 1 *> vary the single multi-item key
            MOVE 0 TO CHOICE-INDEX
            *> We have remembered the index of this slot above. Loop through all of its options.
            PERFORM PATTERN-SLOT-OPTIONS(MULTI-CHOICE-SLOT-INDEX) TIMES
                ADD 1 TO CHOICE-INDEX

                *> Construct the recipe
                ADD 1 TO RECIPES-SHAPED-COUNT
                PERFORM VARYING INPUT-INDEX FROM 1 BY 1 UNTIL INPUT-INDEX > 9
                    IF INPUT-INDEX = MULTI-CHOICE-SLOT-INDEX
                        MOVE PATTERN-SLOT-ID(INPUT-INDEX, CHOICE-INDEX) TO RECIPE-SHAPED-INPUT(RECIPES-SHAPED-COUNT, INPUT-INDEX)
                    ELSE
                        MOVE PATTERN-SLOT-ID(INPUT-INDEX, 1) TO RECIPE-SHAPED-INPUT(RECIPES-SHAPED-COUNT, INPUT-INDEX)
                    END-IF
                END-PERFORM
                MOVE RESULT-ID TO RECIPE-SHAPED-OUTPUT-ID(RECIPES-SHAPED-COUNT)
                MOVE RESULT-COUNT TO RECIPE-SHAPED-OUTPUT-COUNT(RECIPES-SHAPED-COUNT)

                IF RECIPE-SYMMETRICAL = 0
                    PERFORM MirrorShapedRecipe
                END-IF
            END-PERFORM

        WHEN OTHER *> too many multi-item keys - store recipe as a "complex" recipe (our own invention)
            ADD 1 TO RECIPES-COMPLEX-COUNT
            PERFORM VARYING INPUT-INDEX FROM 1 BY 1 UNTIL INPUT-INDEX > 9
                MOVE PATTERN-SLOT-OPTIONS(INPUT-INDEX) TO RECIPE-COMPLEX-INPUT-OPTIONS(RECIPES-COMPLEX-COUNT, INPUT-INDEX)
                PERFORM VARYING CHOICE-INDEX FROM 1 BY 1 UNTIL CHOICE-INDEX > PATTERN-SLOT-OPTIONS(INPUT-INDEX)
                    MOVE PATTERN-SLOT-ID(INPUT-INDEX, CHOICE-INDEX) TO RECIPE-COMPLEX-INPUT-ID(RECIPES-COMPLEX-COUNT, INPUT-INDEX, CHOICE-INDEX)
                END-PERFORM
            END-PERFORM
            MOVE RESULT-ID TO RECIPE-COMPLEX-OUTPUT-ID(RECIPES-COMPLEX-COUNT)
            MOVE RESULT-COUNT TO RECIPE-COMPLEX-OUTPUT-COUNT(RECIPES-COMPLEX-COUNT)

            IF RECIPE-SYMMETRICAL = 0
                PERFORM MirrorComplexRecipe
            END-IF
    END-EVALUATE

    GOBACK.

ParseKey.
    CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT
        *> key: single character, used in pattern to represent an ingredient
        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE STR
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        ADD 1 TO PATTERN-KEY-COUNT
        MOVE STR(1:1) TO PATTERN-KEY-CHAR(PATTERN-KEY-COUNT)

        *> value: array of ingredients, or a single ingredient, or a tag reference
        CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE STR
        IF LK-FAILURE = 0
            *> it was a string
            IF STR(1:1) = "#"
                *> a tag, resolve it
                PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-REGISTRY-LENGTH(TAGS-ITEM-REGISTRY-INDEX)
                    IF TAGS-REGISTRY-TAG-NAME(TAGS-ITEM-REGISTRY-INDEX, TAG-INDEX) = STR(2:)
                        PERFORM TAGS-REGISTRY-TAG-LENGTH(TAGS-ITEM-REGISTRY-INDEX, TAG-INDEX) TIMES
                            COMPUTE TEMP-INT32 = PATTERN-KEY-OPTIONS(PATTERN-KEY-COUNT) + 1
                            MOVE TEMP-INT32 TO PATTERN-KEY-OPTIONS(PATTERN-KEY-COUNT)
                            MOVE TAGS-REGISTRY-TAG-ENTRY(TAGS-ITEM-REGISTRY-INDEX, TAG-INDEX, TEMP-INT32) TO PATTERN-KEY-ID(PATTERN-KEY-COUNT, TEMP-INT32)
                        END-PERFORM
                        EXIT PERFORM
                    END-IF
                END-PERFORM
                IF PATTERN-KEY-OPTIONS(PATTERN-KEY-COUNT) = 0
                    DISPLAY "Unknown tag reference: " FUNCTION TRIM(STR)
                    MOVE 1 TO LK-FAILURE
                END-IF
            ELSE
                *> just a single item
                CALL "Registries-Lookup" USING "minecraft:item" STR TEMP-INT32
                IF TEMP-INT32 <= 0
                    MOVE 1 TO LK-FAILURE
                    GOBACK
                END-IF
                MOVE 1 TO PATTERN-KEY-OPTIONS(PATTERN-KEY-COUNT)
                MOVE TEMP-INT32 TO PATTERN-KEY-ID(PATTERN-KEY-COUNT, 1)
            END-IF
        ELSE
            *> attempt to parse as an array
            CALL "JsonParse-ArrayStart" USING LK-JSON LK-OFFSET LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK *> what else could it be?
            END-IF

            PERFORM UNTIL EXIT
                CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE STR
                IF LK-FAILURE NOT = 0
                    GOBACK
                END-IF

                ADD 1 TO PATTERN-KEY-OPTIONS(PATTERN-KEY-COUNT)
                CALL "Registries-Lookup" USING "minecraft:item" STR TEMP-INT32
                IF TEMP-INT32 <= 0
                    MOVE 1 TO LK-FAILURE
                    GOBACK
                END-IF
                MOVE TEMP-INT32 TO PATTERN-KEY-ID(PATTERN-KEY-COUNT, PATTERN-KEY-OPTIONS(PATTERN-KEY-COUNT))

                CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET AT-END
                IF AT-END NOT = 0
                    EXIT PERFORM
                END-IF
            END-PERFORM

            CALL "JsonEncode-ArrayEnd" USING LK-JSON LK-OFFSET LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF
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

ParsePattern.
    CALL "JsonParse-ArrayStart" USING LK-JSON LK-OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    MOVE 1 TO TEMP-INT32

    PERFORM UNTIL EXIT
        CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE PATTERN-TEXT-ROW(TEMP-INT32)
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
        ADD 1 TO TEMP-INT32
        CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET AT-END
        IF AT-END NOT = 0
            EXIT PERFORM
        END-IF
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING LK-JSON LK-OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    COMPUTE RECIPE-WIDTH = FUNCTION MAX(
        FUNCTION STORED-CHAR-LENGTH(PATTERN-TEXT-ROW(1)),
        FUNCTION STORED-CHAR-LENGTH(PATTERN-TEXT-ROW(2)),
        FUNCTION STORED-CHAR-LENGTH(PATTERN-TEXT-Row(3))
    )

    *> Horizontally asymmetrical recipes must be mirrored when adding to the recipe table.
    IF RECIPE-WIDTH <= 1 OR (PATTERN-TEXT(1:1) = PATTERN-TEXT(RECIPE-WIDTH:1) AND
            PATTERN-TEXT(4:1) = PATTERN-TEXT(RECIPE-WIDTH + 3:1) AND
            PATTERN-TEXT(7:1) = PATTERN-TEXT(RECIPE-WIDTH + 6:1))
        MOVE 1 TO RECIPE-SYMMETRICAL
    ELSE
        MOVE 0 TO RECIPE-SYMMETRICAL
    END-IF
    .

MirrorShapedRecipe.
    MOVE RECIPES-SHAPED-COUNT TO TEMP-INT32
    ADD 1 TO RECIPES-SHAPED-COUNT

    *> copy the recipe (output; as well as any columns not handled by the following code - i.e.,
    *> the middle column of an Nx3 recipe)
    MOVE RECIPE-SHAPED(TEMP-INT32) TO RECIPE-SHAPED(RECIPES-SHAPED-COUNT)

    MOVE RECIPE-SHAPED-INPUT(TEMP-INT32, 1) TO RECIPE-SHAPED-INPUT(RECIPES-SHAPED-COUNT, RECIPE-WIDTH)
    MOVE RECIPE-SHAPED-INPUT(TEMP-INT32, RECIPE-WIDTH) TO RECIPE-SHAPED-INPUT(RECIPES-SHAPED-COUNT, 1)

    MOVE RECIPE-SHAPED-INPUT(TEMP-INT32, 4) TO RECIPE-SHAPED-INPUT(RECIPES-SHAPED-COUNT, RECIPE-WIDTH + 3)
    MOVE RECIPE-SHAPED-INPUT(TEMP-INT32, RECIPE-WIDTH + 3) TO RECIPE-SHAPED-INPUT(RECIPES-SHAPED-COUNT, 4)

    MOVE RECIPE-SHAPED-INPUT(TEMP-INT32, 7) TO RECIPE-SHAPED-INPUT(RECIPES-SHAPED-COUNT, RECIPE-WIDTH + 6)
    MOVE RECIPE-SHAPED-INPUT(TEMP-INT32, RECIPE-WIDTH + 6) TO RECIPE-SHAPED-INPUT(RECIPES-SHAPED-COUNT, 7)
    .

MirrorComplexRecipe.
    MOVE RECIPES-COMPLEX-COUNT TO TEMP-INT32
    ADD 1 TO RECIPES-COMPLEX-COUNT

    MOVE RECIPE-COMPLEX(TEMP-INT32) TO RECIPE-COMPLEX(RECIPES-COMPLEX-COUNT)

    MOVE RECIPE-COMPLEX-INPUT(TEMP-INT32, 1) TO RECIPE-COMPLEX-INPUT(RECIPES-COMPLEX-COUNT, RECIPE-WIDTH)
    MOVE RECIPE-COMPLEX-INPUT(TEMP-INT32, RECIPE-WIDTH) TO RECIPE-COMPLEX-INPUT(RECIPES-COMPLEX-COUNT, 1)

    MOVE RECIPE-COMPLEX-INPUT(TEMP-INT32, 4) TO RECIPE-COMPLEX-INPUT(RECIPES-COMPLEX-COUNT, RECIPE-WIDTH + 3)
    MOVE RECIPE-COMPLEX-INPUT(TEMP-INT32, RECIPE-WIDTH + 3) TO RECIPE-COMPLEX-INPUT(RECIPES-COMPLEX-COUNT, 4)

    MOVE RECIPE-COMPLEX-INPUT(TEMP-INT32, 7) TO RECIPE-COMPLEX-INPUT(RECIPES-COMPLEX-COUNT, RECIPE-WIDTH + 6)
    MOVE RECIPE-COMPLEX-INPUT(TEMP-INT32, RECIPE-WIDTH + 6) TO RECIPE-COMPLEX-INPUT(RECIPES-COMPLEX-COUNT, 7)
    .

END PROGRAM Parse-Recipe-Shaped.
