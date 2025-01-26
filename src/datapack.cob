*> --- Datapack-Load ---
*> Load a datapack, including registry contents and tags.
IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-Load.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-TAGS.
    COPY DD-RECIPES.
    01 REGISTRY-COUNT               BINARY-LONG UNSIGNED.
    01 REGISTRY-INDEX               BINARY-LONG UNSIGNED.
    01 REGISTRY-NAME                PIC X(255).
LINKAGE SECTION.
    01 LK-ROOT-PATH                 PIC X ANY LENGTH.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-ROOT-PATH LK-PACK-NAME LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    MOVE 0 TO TAGS-REGISTRY-COUNT
    MOVE 0 TO RECIPES-SHAPELESS-COUNT

    *> For each known registry, load the contents from the datapack (if it exists).
    CALL "Registries-GetCount" USING REGISTRY-COUNT
    PERFORM VARYING REGISTRY-INDEX FROM 1 BY 1 UNTIL REGISTRY-INDEX > REGISTRY-COUNT
        CALL "Registries-Iterate-Name" USING REGISTRY-INDEX REGISTRY-NAME
        *> We want to remove the "minecraft:" prefix from registry names to match the directory structure
        IF REGISTRY-NAME(1:10) = "minecraft:"
            CALL "Datapack-LoadRegistry" USING LK-ROOT-PATH LK-PACK-NAME REGISTRY-NAME(11:) REGISTRY-INDEX LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF
            CALL "Datapack-LoadTags" USING LK-ROOT-PATH LK-PACK-NAME REGISTRY-NAME(11:) LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF
        END-IF
    END-PERFORM

    CALL "Datapack-LoadRecipes" USING LK-ROOT-PATH LK-PACK-NAME LK-FAILURE

    GOBACK.

END PROGRAM Datapack-Load.

*> --- Datapack-LoadRegistry ---
*> Load content from a datapack path, adding entries to the registry.
IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-LoadRegistry.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 DIR-PATH                     PIC X(255).
    01 DIR-PATH-LENGTH              BINARY-LONG UNSIGNED.
    01 DIR-HANDLE                   PIC X(8).
    01 EOF                          BINARY-CHAR UNSIGNED.
    01 DIR-ENTRY                    PIC X(255).
    01 ENTRY-ID                     BINARY-LONG UNSIGNED.
    01 ENTRY-NAME                   PIC X(255).
LINKAGE SECTION.
    01 LK-ROOT-PATH                 PIC X ANY LENGTH.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-REGISTRY-NAME             PIC X ANY LENGTH.
    01 LK-REGISTRY-INDEX            BINARY-LONG UNSIGNED.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-ROOT-PATH LK-PACK-NAME LK-REGISTRY-NAME LK-REGISTRY-INDEX LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    INITIALIZE DIR-PATH
    STRING FUNCTION TRIM(LK-ROOT-PATH) FUNCTION TRIM(LK-PACK-NAME) "/" FUNCTION TRIM(LK-REGISTRY-NAME) INTO DIR-PATH
    MOVE FUNCTION STORED-CHAR-LENGTH(DIR-PATH) TO DIR-PATH-LENGTH

    CALL "OpenDirectory" USING DIR-PATH DIR-PATH-LENGTH DIR-HANDLE GIVING LK-FAILURE
    IF LK-FAILURE NOT = 0
        *> The directory is likely missing, which isn't an error.
        MOVE 0 TO LK-FAILURE
        GOBACK
    END-IF

    *> The entry protocol IDs will be numbered starting from the registry length.
    CALL "Registries-GetRegistryLength" USING LK-REGISTRY-INDEX ENTRY-ID

    PERFORM UNTIL EXIT
        CALL "ReadDirectory" USING DIR-HANDLE DIR-ENTRY GIVING EOF
        IF EOF NOT = 0
            EXIT PERFORM
        END-IF

        *> Prepend the namespace and remove the file extension
        INITIALIZE ENTRY-NAME
        STRING FUNCTION TRIM(LK-PACK-NAME) ":" FUNCTION TRIM(DIR-ENTRY) INTO ENTRY-NAME
        CALL "TrimFileExt" USING ENTRY-NAME

        *> Register the entry
        CALL "Registries-CreateEntry" USING LK-REGISTRY-INDEX ENTRY-NAME ENTRY-ID
        ADD 1 TO ENTRY-ID
    END-PERFORM

    CALL "CloseDirectory" USING DIR-HANDLE

    GOBACK.

END PROGRAM Datapack-LoadRegistry.

*> --- Datapack-LoadTags ---
*> Load tag information from a datapack path. The associated registry must have been loaded first.
IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-LoadTags.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-TAGS.
    01 DIR-PATH                     PIC X(255).
    01 DIR-PATH-LENGTH              BINARY-LONG UNSIGNED.
    01 IS-DIRECTORY                 BINARY-CHAR UNSIGNED.
    01 TAG-INDEX                    BINARY-LONG UNSIGNED.
    01 TAG-ENTRY-INDEX              BINARY-LONG UNSIGNED.
    01 TAG-ENTRY-ID                 BINARY-LONG.
LINKAGE SECTION.
    01 LK-ROOT-PATH                 PIC X ANY LENGTH.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-REGISTRY-NAME             PIC X ANY LENGTH.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-ROOT-PATH LK-PACK-NAME LK-REGISTRY-NAME LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    INITIALIZE DIR-PATH
    STRING FUNCTION TRIM(LK-ROOT-PATH) FUNCTION TRIM(LK-PACK-NAME) "/tags/" FUNCTION TRIM(LK-REGISTRY-NAME) INTO DIR-PATH
    MOVE FUNCTION STORED-CHAR-LENGTH(DIR-PATH) TO DIR-PATH-LENGTH

    CALL "IsDirectory" USING DIR-PATH DIR-PATH-LENGTH IS-DIRECTORY GIVING LK-FAILURE
    *> Ignore missing tag directories
    IF LK-FAILURE NOT = 0 OR IS-DIRECTORY = 0
        MOVE 0 TO LK-FAILURE
        GOBACK
    END-IF

    *> setup loading of the current registry
    INITIALIZE TAGS-CURRENT
    STRING FUNCTION TRIM(LK-PACK-NAME) ":" FUNCTION TRIM(LK-REGISTRY-NAME) INTO TAGS-CURRENT-NAME

    *> load tags for the current registry
    CALL "Datapack-LoadTagDir" USING LK-PACK-NAME OMITTED DIR-PATH LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    *> expand references ("#minecraft:chest_armor" -> "minecraft:leather_chestplate", "minecraft:iron_chestplate", ...)
    CALL "Datapack-ExpandTagRefs" USING LK-PACK-NAME

    *> fill the numeric tag list
    ADD 1 TO TAGS-REGISTRY-COUNT
    MOVE TAGS-CURRENT-NAME TO TAGS-REGISTRY-NAME(TAGS-REGISTRY-COUNT)
    MOVE TAGS-CURRENT-LENGTH TO TAGS-REGISTRY-LENGTH(TAGS-REGISTRY-COUNT)
    PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-CURRENT-LENGTH
        MOVE TAGS-CURRENT-TAG-NAME(TAG-INDEX) TO TAGS-REGISTRY-TAG-NAME(TAGS-REGISTRY-COUNT, TAG-INDEX)
        MOVE TAGS-CURRENT-TAG-LENGTH(TAG-INDEX) TO TAGS-REGISTRY-TAG-LENGTH(TAGS-REGISTRY-COUNT, TAG-INDEX)
        PERFORM VARYING TAG-ENTRY-INDEX FROM 1 BY 1 UNTIL TAG-ENTRY-INDEX > TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)
            CALL "Registries-Get-EntryId" USING TAGS-CURRENT-NAME TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX) TAG-ENTRY-ID
            EVALUATE TAG-ENTRY-ID
                WHEN < 0
                    DISPLAY "Failure getting ID of '" FUNCTION TRIM(TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX)) "' "
                        "in registry '" FUNCTION TRIM(TAGS-CURRENT-NAME) "'"
                WHEN OTHER
                    MOVE TAG-ENTRY-ID TO TAGS-REGISTRY-TAG-ENTRY(TAGS-REGISTRY-COUNT, TAG-INDEX, TAG-ENTRY-INDEX)
            END-EVALUATE
        END-PERFORM
    END-PERFORM

    GOBACK.

END PROGRAM Datapack-LoadTags.

IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-LoadTagDir IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-TAGS.
LOCAL-STORAGE SECTION.
    01 TAG-NAME                     PIC X(255).
    01 DIR-PATH-LENGTH              BINARY-LONG UNSIGNED.
    01 DIR-HANDLE                   PIC X(8).
    01 EOF                          BINARY-CHAR UNSIGNED.
    01 DIR-ENTRY                    PIC X(255).
    01 ENTRY-PATH                   PIC X(255).
    01 ENTRY-PATH-LENGTH            BINARY-LONG UNSIGNED.
    01 ENTRY-IS-DIR                 BINARY-CHAR UNSIGNED.
    01 ENTRY-FAILURE                BINARY-CHAR UNSIGNED.
    01 ENTRY-TAG-NAME               PIC X(255).
LINKAGE SECTION.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-TAG-NAME                  PIC X ANY LENGTH.
    01 LK-TAG-PATH                  PIC X ANY LENGTH.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-PACK-NAME OPTIONAL LK-TAG-NAME LK-TAG-PATH LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    IF LK-TAG-NAME IS OMITTED
        MOVE SPACES TO TAG-NAME
    ELSE
        MOVE LK-TAG-NAME TO TAG-NAME
    END-IF

    MOVE FUNCTION STORED-CHAR-LENGTH(LK-TAG-PATH) TO DIR-PATH-LENGTH
    CALL "OpenDirectory" USING LK-TAG-PATH DIR-PATH-LENGTH DIR-HANDLE GIVING LK-FAILURE
    IF LK-FAILURE NOT = 0
        DISPLAY "Failure opening tag directory: " FUNCTION TRIM(LK-TAG-PATH)
        GOBACK
    END-IF

    *> Note: A path such as tags/block/mineable/axe.json should produce the tag "minecraft:mineable/axe",
    *> not "minecraft:block/mineable/axe". However, "tags/item/chest_armor.json" should produce "minecraft:chest_armor".

    PERFORM UNTIL EXIT
        CALL "ReadDirectory" USING DIR-HANDLE DIR-ENTRY GIVING EOF
        IF EOF NOT = 0
            EXIT PERFORM
        END-IF

        INITIALIZE ENTRY-PATH
        STRING FUNCTION TRIM(LK-TAG-PATH) "/" FUNCTION TRIM(DIR-ENTRY) INTO ENTRY-PATH
        MOVE FUNCTION STORED-CHAR-LENGTH(ENTRY-PATH) TO ENTRY-PATH-LENGTH
        CALL "IsDirectory" USING ENTRY-PATH ENTRY-PATH-LENGTH ENTRY-IS-DIR GIVING ENTRY-FAILURE

        EVALUATE TRUE
            WHEN ENTRY-FAILURE NOT = 0
                EXIT PERFORM

            WHEN ENTRY-IS-DIR = 0
                INITIALIZE ENTRY-TAG-NAME
                EVALUATE TAG-NAME
                    WHEN SPACES
                        STRING FUNCTION TRIM(LK-PACK-NAME) ":" FUNCTION TRIM(DIR-ENTRY) INTO ENTRY-TAG-NAME
                    WHEN OTHER
                        STRING FUNCTION TRIM(LK-PACK-NAME) ":" FUNCTION TRIM(TAG-NAME) "/" FUNCTION TRIM(DIR-ENTRY) INTO ENTRY-TAG-NAME
                END-EVALUATE
                CALL "TrimFileExt" USING ENTRY-TAG-NAME
                CALL "Datapack-LoadTagFile" USING LK-PACK-NAME ENTRY-TAG-NAME ENTRY-PATH LK-FAILURE

            WHEN ENTRY-IS-DIR NOT = 0
                CALL "Datapack-LoadTagDir" USING LK-PACK-NAME DIR-ENTRY ENTRY-PATH LK-FAILURE
        END-EVALUATE

        IF LK-FAILURE NOT = 0
            EXIT PERFORM
        END-IF
    END-PERFORM

    CALL "CloseDirectory" USING DIR-HANDLE

    GOBACK.

END PROGRAM Datapack-LoadTagDir.

IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-LoadTagFile.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-TAGS.
    01 JSON-BUFFER                  PIC X(64000).
    01 JSON-LENGTH                  BINARY-LONG UNSIGNED.
    01 JSON-POS                     BINARY-LONG UNSIGNED.
    01 STR                          PIC X(255).
    01 AT-END                       BINARY-CHAR UNSIGNED.
    01 MAX-TAG-LENGTH               BINARY-LONG UNSIGNED VALUE 0.
LINKAGE SECTION.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-TAG-NAME                  PIC X ANY LENGTH.
    01 LK-TAG-PATH                  PIC X ANY LENGTH.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-PACK-NAME LK-TAG-NAME LK-TAG-PATH LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    CALL "Files-ReadAll" USING LK-TAG-PATH JSON-BUFFER JSON-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0
        DISPLAY "Failure reading tag file: " FUNCTION TRIM(LK-TAG-PATH)
        GOBACK
    END-IF

    ADD 1 TO TAGS-CURRENT-LENGTH
    MOVE LK-TAG-NAME TO TAGS-CURRENT-TAG-NAME(TAGS-CURRENT-LENGTH)

    MOVE 1 TO JSON-POS

    *> start
    CALL "JsonParse-ObjectStart" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    *> Expect exactly one "values": [...] entry
    CALL "JsonParse-ObjectKey" USING JSON-BUFFER JSON-POS LK-FAILURE STR
    IF LK-FAILURE NOT = 0 OR STR NOT = "values"
        DISPLAY "Unexpected JSON data in tag file: " FUNCTION TRIM(LK-TAG-PATH)
        GOBACK
    END-IF

    *> start values array
    CALL "JsonParse-ArrayStart" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    *> check if empty
    CALL "JsonParse-ArrayEnd" USING JSON-BUFFER JSON-POS AT-END
    IF AT-END NOT = 0
        MOVE 0 TO AT-END
        PERFORM UNTIL AT-END NOT = 0
            CALL "JsonParse-String" USING JSON-BUFFER JSON-POS LK-FAILURE STR
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            ADD 1 TO TAGS-CURRENT-TAG-LENGTH(TAGS-CURRENT-LENGTH)
            MOVE STR TO TAGS-CURRENT-TAG-ENTRY(TAGS-CURRENT-LENGTH, TAGS-CURRENT-TAG-LENGTH(TAGS-CURRENT-LENGTH))

            CALL "JsonParse-Comma" USING JSON-BUFFER JSON-POS AT-END
        END-PERFORM

        *> end values array
        CALL "JsonParse-ArrayEnd" USING JSON-BUFFER JSON-POS LK-FAILURE
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
    END-IF

    *> end
    CALL "JsonParse-ObjectEnd" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    GOBACK.

END PROGRAM Datapack-LoadTagFile.

IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-ExpandTagRefs.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-TAGS.
    01 TAG-INDEX                    BINARY-LONG UNSIGNED.
    01 TAG-ENTRY-INDEX              BINARY-LONG UNSIGNED.
    01 TAG-REFERENCES-COUNT         BINARY-LONG UNSIGNED.
    01 TAG-SEEN-REFERENCES          BINARY-LONG UNSIGNED.
    01 OTHER-TAG-NAME               PIC X(255).
    01 OTHER-TAG-INDEX              BINARY-LONG UNSIGNED.
    01 STOP-RESOLVING               BINARY-CHAR UNSIGNED.
    01 OTHER-ENTRY-INDEX            BINARY-LONG UNSIGNED.
    01 ENTRY-NAME                   PIC X(255).
    01 EXISTING-ENTRY-INDEX         BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-PACK-NAME.
    *> Mark which tags have references
    PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-CURRENT-LENGTH
        PERFORM VARYING TAG-ENTRY-INDEX FROM 1 BY 1 UNTIL TAG-ENTRY-INDEX > TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)
            IF TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX)(1:1) = "#"
                ADD 1 TO TAGS-CURRENT-REFERENCES(TAG-INDEX)
            END-IF
        END-PERFORM
    END-PERFORM

    *> Resolve references until no further resolution is possible
    MOVE 0 TO STOP-RESOLVING
    PERFORM UNTIL STOP-RESOLVING = 1
        MOVE 1 TO STOP-RESOLVING
        PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-CURRENT-LENGTH
            IF TAGS-CURRENT-REFERENCES(TAG-INDEX) > 0
                PERFORM ExpandTag
                IF TAGS-CURRENT-REFERENCES(TAG-INDEX) = 0
                    MOVE 0 TO STOP-RESOLVING
                END-IF
            END-IF
        END-PERFORM
    END-PERFORM

    *> Verify that all references have been resolved
    PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-CURRENT-LENGTH
        IF TAGS-CURRENT-REFERENCES(TAG-INDEX) > 0
            PERFORM VARYING TAG-ENTRY-INDEX FROM 1 BY 1 UNTIL TAG-ENTRY-INDEX > TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)
                IF TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX)(1:1) = "#"
                    DISPLAY "Unresolved tag reference: " FUNCTION TRIM(TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX))
                        " in " FUNCTION TRIM(TAGS-CURRENT-TAG-NAME(TAG-INDEX)) " of " FUNCTION TRIM(TAGS-CURRENT-NAME)
                END-IF
            END-PERFORM
        END-IF
    END-PERFORM

    GOBACK.

ExpandTag.
    MOVE TAGS-CURRENT-REFERENCES(TAG-INDEX) TO TAG-REFERENCES-COUNT
    MOVE 0 TO TAG-SEEN-REFERENCES
    PERFORM VARYING TAG-ENTRY-INDEX FROM 1 BY 1 UNTIL TAG-ENTRY-INDEX > TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)
        IF TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX)(1:1) = "#"
            ADD 1 TO TAG-SEEN-REFERENCES
            MOVE TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX)(2:) TO OTHER-TAG-NAME
            PERFORM VARYING OTHER-TAG-INDEX FROM 1 BY 1 UNTIL OTHER-TAG-INDEX > TAGS-CURRENT-LENGTH
                IF TAGS-CURRENT-TAG-NAME(OTHER-TAG-INDEX) = OTHER-TAG-NAME
                    *> can only expand tags that have already been resolved
                    IF TAGS-CURRENT-REFERENCES(OTHER-TAG-INDEX) = 0
                        PERFORM ExpandOtherTag
                        SUBTRACT 1 FROM TAGS-CURRENT-REFERENCES(TAG-INDEX)
                        *> since the tag was expanded, the loop must continue at the same index
                        SUBTRACT 1 FROM TAG-ENTRY-INDEX
                    END-IF
                    EXIT PERFORM
                END-IF
            END-PERFORM
        END-IF
        *> more references to look at?
        IF TAG-SEEN-REFERENCES >= TAG-REFERENCES-COUNT
            EXIT PERFORM
        END-IF
    END-PERFORM
    EXIT PARAGRAPH.

ExpandOtherTag.
    *> swap the last entry into the current position
    MOVE TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)) TO TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX)
    SUBTRACT 1 FROM TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)

    *> copy the other tag's entries to the end of the current tag's entries - but avoid duplicates!
    PERFORM VARYING OTHER-ENTRY-INDEX FROM 1 BY 1 UNTIL OTHER-ENTRY-INDEX > TAGS-CURRENT-TAG-LENGTH(OTHER-TAG-INDEX)
        MOVE TAGS-CURRENT-TAG-ENTRY(OTHER-TAG-INDEX, OTHER-ENTRY-INDEX) TO ENTRY-NAME
        PERFORM InsertEntry
    END-PERFORM

    EXIT PARAGRAPH.

InsertEntry.
    PERFORM VARYING EXISTING-ENTRY-INDEX FROM 1 BY 1 UNTIL EXISTING-ENTRY-INDEX > TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)
        IF TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, EXISTING-ENTRY-INDEX) = ENTRY-NAME
            EXIT PARAGRAPH
        END-IF
    END-PERFORM

    ADD 1 TO TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)
    MOVE ENTRY-NAME TO TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAGS-CURRENT-TAG-LENGTH(TAG-INDEX))

    EXIT PARAGRAPH.

END PROGRAM Datapack-ExpandTagRefs.

*> --- Datapack-LoadRecipes ---
*> Load crafting recipes from a datapack path.
IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-LoadRecipes.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 DIR-PATH                     PIC X(255).
    01 DIR-PATH-LENGTH              BINARY-LONG UNSIGNED.
    01 DIR-HANDLE                   PIC X(8).
    01 EOF                          BINARY-CHAR UNSIGNED.
    01 DIR-ENTRY                    PIC X(255).
    01 ENTRY-PATH                   PIC X(255).
LINKAGE SECTION.
    01 LK-ROOT-PATH                 PIC X ANY LENGTH.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-ROOT-PATH LK-PACK-NAME LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    INITIALIZE DIR-PATH
    STRING FUNCTION TRIM(LK-ROOT-PATH) FUNCTION TRIM(LK-PACK-NAME) "/recipe" INTO DIR-PATH
    MOVE FUNCTION STORED-CHAR-LENGTH(DIR-PATH) TO DIR-PATH-LENGTH

    CALL "OpenDirectory" USING DIR-PATH DIR-PATH-LENGTH DIR-HANDLE GIVING LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT
        CALL "ReadDirectory" USING DIR-HANDLE DIR-ENTRY GIVING EOF
        IF EOF NOT = 0
            EXIT PERFORM
        END-IF

        INITIALIZE ENTRY-PATH
        STRING FUNCTION TRIM(LK-ROOT-PATH) FUNCTION TRIM(LK-PACK-NAME) "/recipe/" FUNCTION TRIM(DIR-ENTRY) INTO ENTRY-PATH

        CALL "Datapack-LoadRecipe" USING ENTRY-PATH LK-FAILURE
        IF LK-FAILURE NOT = 0
            DISPLAY "Failure loading recipe: " FUNCTION TRIM(ENTRY-PATH)
            GOBACK
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM Datapack-LoadRecipes.

*> --- Datapack-LoadRecipe ---
*> Load a crafting recipe from a datapack path.
IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-LoadRecipe.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-RECIPES.
    COPY DD-TAGS.
    01 JSON-BUFFER                  PIC X(4096).
    01 JSON-LENGTH                  BINARY-LONG UNSIGNED.
    01 JSON-POS                     BINARY-LONG UNSIGNED.
    01 AT-END                       BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(256).
    *> index of the item registry in the tags table (initialized on first use)
    01 TAGS-ITEM-REGISTRY-INDEX     BINARY-LONG UNSIGNED        VALUE 0.
LOCAL-STORAGE SECTION.
    01 RECIPE-TYPE                  PIC X(128)                  VALUE SPACES.
    *> result
    01 RESULT-NAME                  PIC X(128)                  VALUE SPACES.
    01 RESULT-ID                    BINARY-LONG                 VALUE 0.
    01 RESULT-COUNT                 BINARY-LONG                 VALUE 0.
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
        02 INGREDIENT-CHOICE-ID     OCCURS 16 TIMES BINARY-LONG VALUE 0.
    01 INGREDIENT-CHOICE-COMBINED-IDS.
        02 INGREDIENT-CHOICE-COMBINED-ID OCCURS 9 TIMES BINARY-LONG VALUE 0.
    *> searching for tagged items
    01 TAG-INDEX                    BINARY-LONG UNSIGNED        VALUE 0.
LINKAGE SECTION.
    01 LK-RECIPE-PATH               PIC X ANY LENGTH.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-RECIPE-PATH LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    IF TAGS-ITEM-REGISTRY-INDEX = 0
        PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-REGISTRY-COUNT
            IF TAGS-REGISTRY-NAME(TAG-INDEX) = "minecraft:item"
                MOVE TAG-INDEX TO TAGS-ITEM-REGISTRY-INDEX
                EXIT PERFORM
            END-IF
        END-PERFORM
    END-IF

    *> Read the recipe file
    CALL "Files-ReadAll" USING LK-RECIPE-PATH JSON-BUFFER JSON-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0
        DISPLAY "Failure reading recipe file: " FUNCTION TRIM(LK-RECIPE-PATH)
        GOBACK
    END-IF

    *> Parse the recipe file
    MOVE 1 TO JSON-POS
    CALL "JsonParse-ObjectStart" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSON-BUFFER JSON-POS LK-FAILURE STR
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        EVALUATE STR
            WHEN "type"
                PERFORM ParseType
            WHEN "key"
                PERFORM ParseKey
            WHEN "pattern"
                PERFORM ParsePattern
            WHEN "ingredients"
                PERFORM ParseIngredients
            WHEN "result"
                PERFORM ParseResult
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSON-BUFFER JSON-POS LK-FAILURE
        END-EVALUATE

        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        CALL "JsonParse-Comma" USING JSON-BUFFER JSON-POS AT-END
        IF AT-END NOT = 0
            EXIT PERFORM
        END-IF
    END-PERFORM

    EVALUATE RECIPE-TYPE
        WHEN "minecraft:crafting_shapeless"
            PERFORM AddShapelessRecipe
        WHEN "minecraft:crafting_shaped"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_transmute"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_decorated_pot"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_armordye"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_bannerduplicate"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_bookcloning"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_firework_rocket"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_firework_star"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_firework_star_fade"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_mapcloning"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_mapextending"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_repairitem"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_shielddecoration"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:crafting_special_tippedarrow"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:smelting"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:blasting"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:smoking"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:campfire_cooking"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:stonecutting"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:smithing_transform"
            *> TODO implement
            CONTINUE
        WHEN "minecraft:smithing_trim"
            *> TODO implement
            CONTINUE
        WHEN OTHER
            DISPLAY "Unknown recipe type: " FUNCTION TRIM(RECIPE-TYPE)
            MOVE 1 TO LK-FAILURE
    END-EVALUATE

    *> End of the recipe file
    CALL "JsonParse-ObjectEnd" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    GOBACK.

ParseType.
    CALL "JsonParse-String" USING JSON-BUFFER JSON-POS LK-FAILURE RECIPE-TYPE
    .

ParseKey.
    *> TODO implement
    CALL "JsonParse-SkipValue" USING JSON-BUFFER JSON-POS LK-FAILURE
    .

ParsePattern.
    *> TODO implement
    CALL "JsonParse-SkipValue" USING JSON-BUFFER JSON-POS LK-FAILURE
    .

ParseIngredients.
    CALL "JsonParse-ArrayStart" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT
        CALL "JsonParse-String" USING JSON-BUFFER JSON-POS LK-FAILURE STR

        IF LK-FAILURE = 0
            *> resolve tag as a choice
            IF STR(1:1) = "#"
                IF INGREDIENT-CHOICE-COUNT > 0
                    *> this is not currently needed by any recipes, so it's not implemented
                    DISPLAY "Recipe has more than one ingredient choice: " FUNCTION TRIM(LK-RECIPE-PATH)
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
                    DISPLAY "Unknown tag reference: " FUNCTION TRIM(STR) " in " FUNCTION TRIM(LK-RECIPE-PATH)
                    MOVE 1 TO LK-FAILURE
                END-IF
            ELSE
                ADD 1 TO INGREDIENTS-COUNT
                MOVE STR TO INGREDIENT-NAME(INGREDIENTS-COUNT)
            END-IF
        ELSE
            *> alternatives are represented as arrays of strings
            CALL "JsonParse-ArrayStart" USING JSON-BUFFER JSON-POS LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            PERFORM UNTIL EXIT
                CALL "JsonParse-String" USING JSON-BUFFER JSON-POS LK-FAILURE STR
                IF LK-FAILURE NOT = 0
                    GOBACK
                END-IF

                ADD 1 TO INGREDIENT-CHOICE-COUNT
                CALL "Registries-Get-EntryId" USING "minecraft:item" STR INGREDIENT-CHOICE-ID(INGREDIENT-CHOICE-COUNT)
                IF INGREDIENT-CHOICE-ID(INGREDIENT-CHOICE-COUNT) <= 0
                    MOVE 1 TO LK-FAILURE
                    GOBACK
                END-IF

                CALL "JsonParse-Comma" USING JSON-BUFFER JSON-POS AT-END
                IF AT-END NOT = 0
                    EXIT PERFORM
                END-IF
            END-PERFORM

            CALL "JsonParse-ArrayEnd" USING JSON-BUFFER JSON-POS LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF
        END-IF

        CALL "JsonParse-Comma" USING JSON-BUFFER JSON-POS AT-END
        IF AT-END NOT = 0
            EXIT PERFORM
        END-IF
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF
    .

ParseResult.
    *> some recipe types just have a string
    CALL "JsonParse-String" USING JSON-BUFFER JSON-POS LK-FAILURE RESULT-NAME
    IF LK-FAILURE = 0
        MOVE 1 TO RESULT-COUNT
        GOBACK
    END-IF
    MOVE 0 TO LK-FAILURE

    CALL "JsonParse-ObjectStart" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSON-BUFFER JSON-POS LK-FAILURE STR
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        EVALUATE STR
            WHEN "count"
                CALL "JsonParse-Integer" USING JSON-BUFFER JSON-POS LK-FAILURE RESULT-COUNT
            WHEN "id"
                CALL "JsonParse-String" USING JSON-BUFFER JSON-POS LK-FAILURE RESULT-NAME
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSON-BUFFER JSON-POS LK-FAILURE
        END-EVALUATE

        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        CALL "JsonParse-Comma" USING JSON-BUFFER JSON-POS AT-END
        IF AT-END NOT = 0
            EXIT PERFORM
        END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF
    .

AddShapelessRecipe.
    CALL "Registries-Get-EntryId" USING "minecraft:item" RESULT-NAME RESULT-ID

    IF RESULT-ID <= 0 OR RESULT-COUNT <= 0
        DISPLAY "Invalid recipe result: " FUNCTION TRIM(RESULT-ID) " x" FUNCTION TRIM(RESULT-COUNT)
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> resolve ingredient IDs
    PERFORM VARYING INGREDIENTS-INDEX FROM 1 BY 1 UNTIL INGREDIENTS-INDEX > INGREDIENTS-COUNT
        MOVE INGREDIENT-NAME(INGREDIENTS-INDEX) TO STR
        CALL "Registries-Get-EntryId" USING "minecraft:item" STR INGREDIENT-ID(INGREDIENTS-INDEX)
        IF INGREDIENT-ID(INGREDIENTS-INDEX) <= 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
    END-PERFORM

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
    .

END PROGRAM Datapack-LoadRecipe.
