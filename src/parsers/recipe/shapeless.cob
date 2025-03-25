*> --- Parse-Recipe-Shapeless ---
*> Parse a recipe of type "minecraft:crafting_shapeless" from JSON.
IDENTIFICATION DIVISION.
PROGRAM-ID. Parse-Recipe-Shapeless.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-RECIPES.
    COPY DD-TAGS.
    *> index of the item registry in the tags table (initialized on first use)
    01 TAGS-ITEM-REGISTRY-INDEX     BINARY-LONG UNSIGNED        VALUE 0.
    01 AT-END                       BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(128).
LOCAL-STORAGE SECTION.
    *> result
    01 RESULT.
        02 RESULT-ID                BINARY-LONG                 VALUE 0.
        02 RESULT-COUNT             BINARY-LONG                 VALUE 0.
    *> ingredients
    01 INGREDIENTS-COUNT            BINARY-LONG UNSIGNED        VALUE 0.
    01 INGREDIENTS-INDEX            BINARY-LONG UNSIGNED        VALUE 0.
    01 INGREDIENT-NAMES.
        02 INGREDIENT-NAME          OCCURS 9 TIMES PIC X(128)   VALUE SPACES.
    01 INGREDIENT-IDS.
        02 INGREDIENT-ID            OCCURS 9 TIMES BINARY-LONG  VALUE 0.
    *> An ingredient can be an array or a tag reference, in which case the resolved values are stored here.
    *> The ingredient will then not be included in the ingredient count.
    *> TODO support more than one tag/array per recipe
    01 INGREDIENT-CHOICE-COUNT      BINARY-LONG UNSIGNED        VALUE 0.
    01 INGREDIENT-CHOICE-INDEX      BINARY-LONG UNSIGNED        VALUE 0.
    01 INGREDIENT-CHOICE-IDS.
        02 INGREDIENT-CHOICE-ID     OCCURS TAGS-PER-REGISTRY-CAPACITY TIMES BINARY-LONG VALUE 0.
    01 INGREDIENT-CHOICE-COMBINED-IDS.
        02 INGREDIENT-CHOICE-COMBINED-ID OCCURS 9 TIMES BINARY-LONG VALUE 0.
    *> searching for tagged items
    01 TAG-INDEX                    BINARY-LONG UNSIGNED        VALUE 0.
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
            WHEN "ingredients"
                PERFORM ParseIngredients
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

    *> resolve ingredient IDs
    PERFORM VARYING INGREDIENTS-INDEX FROM 1 BY 1 UNTIL INGREDIENTS-INDEX > INGREDIENTS-COUNT
        MOVE INGREDIENT-NAME(INGREDIENTS-INDEX) TO STR
        CALL "Registries-Lookup" USING "minecraft:item" STR INGREDIENT-ID(INGREDIENTS-INDEX)
        IF INGREDIENT-ID(INGREDIENTS-INDEX) <= 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
    END-PERFORM

    *> add to recipe list
    IF INGREDIENT-CHOICE-COUNT = 0
        *> default: simple shapeless recipe

        *> sort the ingredients by ID, descending, such that lookup can be a string search
        SORT INGREDIENT-ID ON DESCENDING KEY INGREDIENT-ID

        ADD 1 TO RECIPES-SHAPELESS-COUNT
        MOVE INGREDIENT-IDS TO RECIPE-SHAPELESS-INPUTS(RECIPES-SHAPELESS-COUNT)
        MOVE RESULT-ID TO RECIPE-SHAPELESS-OUTPUT-ID(RECIPES-SHAPELESS-COUNT)
        MOVE RESULT-COUNT TO RECIPE-SHAPELESS-OUTPUT-COUNT(RECIPES-SHAPELESS-COUNT)
    ELSE
        *> one recipe per choice
        ADD 1 TO INGREDIENTS-COUNT
        PERFORM VARYING INGREDIENT-CHOICE-INDEX FROM 1 BY 1 UNTIL INGREDIENT-CHOICE-INDEX > INGREDIENT-CHOICE-COUNT
            MOVE INGREDIENT-IDS TO INGREDIENT-CHOICE-COMBINED-IDS
            MOVE INGREDIENT-CHOICE-ID(INGREDIENT-CHOICE-INDEX) TO INGREDIENT-CHOICE-COMBINED-ID(INGREDIENTS-COUNT)

            SORT INGREDIENT-CHOICE-COMBINED-ID ON DESCENDING KEY INGREDIENT-CHOICE-COMBINED-ID

            ADD 1 TO RECIPES-SHAPELESS-COUNT
            MOVE INGREDIENT-CHOICE-COMBINED-IDS TO RECIPE-SHAPELESS-INPUTS(RECIPES-SHAPELESS-COUNT)
            MOVE RESULT-ID TO RECIPE-SHAPELESS-OUTPUT-ID(RECIPES-SHAPELESS-COUNT)
            MOVE RESULT-COUNT TO RECIPE-SHAPELESS-OUTPUT-COUNT(RECIPES-SHAPELESS-COUNT)
        END-PERFORM
    END-IF

    GOBACK.

ParseIngredients.
    CALL "JsonParse-ArrayStart" USING LK-JSON LK-OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT
        CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE STR

        IF LK-FAILURE = 0
            *> resolve tag as a choice
            IF STR(1:1) = "#"
                IF INGREDIENT-CHOICE-COUNT > 0
                    *> this is not currently needed by any recipes, so it's not implemented
                    DISPLAY "Recipe has more than one ingredient choice"
                    MOVE 1 TO LK-FAILURE
                END-IF
                PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-REGISTRY-LENGTH(TAGS-ITEM-REGISTRY-INDEX)
                    IF TAGS-REGISTRY-TAG-NAME(TAGS-ITEM-REGISTRY-INDEX, TAG-INDEX) = STR(2:)
                        PERFORM VARYING INGREDIENT-CHOICE-COUNT FROM 0 BY 1 UNTIL INGREDIENT-CHOICE-COUNT >= TAGS-REGISTRY-TAG-LENGTH(TAGS-ITEM-REGISTRY-INDEX, TAG-INDEX)
                            MOVE TAGS-REGISTRY-TAG-ENTRY(TAGS-ITEM-REGISTRY-INDEX, TAG-INDEX, INGREDIENT-CHOICE-COUNT + 1) TO INGREDIENT-CHOICE-ID(INGREDIENT-CHOICE-COUNT + 1)
                        END-PERFORM
                        EXIT PERFORM
                    END-IF
                END-PERFORM
                IF INGREDIENT-CHOICE-COUNT = 0
                    DISPLAY "Unknown tag reference: " FUNCTION TRIM(STR)
                    MOVE 1 TO LK-FAILURE
                END-IF
            ELSE
                ADD 1 TO INGREDIENTS-COUNT
                MOVE STR TO INGREDIENT-NAME(INGREDIENTS-COUNT)
            END-IF
        ELSE
            *> alternatives are represented as arrays of strings
            CALL "JsonParse-ArrayStart" USING LK-JSON LK-OFFSET LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            PERFORM UNTIL EXIT
                CALL "JsonParse-String" USING LK-JSON LK-OFFSET LK-FAILURE STR
                IF LK-FAILURE NOT = 0
                    GOBACK
                END-IF

                ADD 1 TO INGREDIENT-CHOICE-COUNT
                CALL "Registries-Lookup" USING "minecraft:item" STR INGREDIENT-CHOICE-ID(INGREDIENT-CHOICE-COUNT)
                IF INGREDIENT-CHOICE-ID(INGREDIENT-CHOICE-COUNT) <= 0
                    MOVE 1 TO LK-FAILURE
                    GOBACK
                END-IF

                CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET AT-END
                IF AT-END NOT = 0
                    EXIT PERFORM
                END-IF
            END-PERFORM

            CALL "JsonParse-ArrayEnd" USING LK-JSON LK-OFFSET LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF
        END-IF

        CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET AT-END
        IF AT-END NOT = 0
            EXIT PERFORM
        END-IF
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING LK-JSON LK-OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF
    .

END PROGRAM Parse-Recipe-Shapeless.
