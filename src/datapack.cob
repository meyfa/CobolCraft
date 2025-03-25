*> --- Datapack-Load ---
*> Load a datapack, including registry contents and tags.
IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-Load.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-TAGS.
    COPY DD-RECIPES.
    01 REGISTRY-COUNT               BINARY-LONG UNSIGNED.
    01 REGISTRY-ID                  BINARY-LONG UNSIGNED.
    01 REGISTRY-NAME                PIC X(255).
LINKAGE SECTION.
    01 LK-ROOT-PATH                 PIC X ANY LENGTH.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-ROOT-PATH LK-PACK-NAME LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    INITIALIZE TAGS
    INITIALIZE RECIPES

    DISPLAY "    Loading entries...          " WITH NO ADVANCING

    *> For each known registry, load the contents from the datapack (if it exists).
    CALL "Registries-Count" USING REGISTRY-COUNT
    PERFORM VARYING REGISTRY-ID FROM 0 BY 1 UNTIL REGISTRY-ID >= REGISTRY-COUNT
        CALL "Registries-Name" USING REGISTRY-ID REGISTRY-NAME
        *> We want to remove the "minecraft:" prefix from registry names to match the directory structure
        IF REGISTRY-NAME(1:10) = "minecraft:"
            CALL "Datapack-LoadRegistry" USING LK-ROOT-PATH LK-PACK-NAME REGISTRY-NAME(11:) REGISTRY-ID LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF
            CALL "Datapack-LoadTags" USING LK-ROOT-PATH LK-PACK-NAME REGISTRY-NAME(11:) LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF
        END-IF
    END-PERFORM

    DISPLAY "done"

    DISPLAY "    Loading recipes...          " WITH NO ADVANCING
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
    01 ENTRY-NAME                   PIC X(255).
LINKAGE SECTION.
    01 LK-ROOT-PATH                 PIC X ANY LENGTH.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-REGISTRY-NAME             PIC X ANY LENGTH.
    01 LK-REGISTRY-ID               BINARY-LONG UNSIGNED.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-ROOT-PATH LK-PACK-NAME LK-REGISTRY-NAME LK-REGISTRY-ID LK-FAILURE.
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
        CALL "Registries-CreateEntry" USING LK-REGISTRY-ID ENTRY-NAME
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
            CALL "Registries-Lookup" USING TAGS-CURRENT-NAME TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX) TAG-ENTRY-ID
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
    .

ExpandOtherTag.
    *> swap the last entry into the current position
    MOVE TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)) TO TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAG-ENTRY-INDEX)
    SUBTRACT 1 FROM TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)

    *> copy the other tag's entries to the end of the current tag's entries - but avoid duplicates!
    PERFORM VARYING OTHER-ENTRY-INDEX FROM 1 BY 1 UNTIL OTHER-ENTRY-INDEX > TAGS-CURRENT-TAG-LENGTH(OTHER-TAG-INDEX)
        MOVE TAGS-CURRENT-TAG-ENTRY(OTHER-TAG-INDEX, OTHER-ENTRY-INDEX) TO ENTRY-NAME
        PERFORM InsertEntry
    END-PERFORM
    .

InsertEntry.
    PERFORM VARYING EXISTING-ENTRY-INDEX FROM 1 BY 1 UNTIL EXISTING-ENTRY-INDEX > TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)
        IF TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, EXISTING-ENTRY-INDEX) = ENTRY-NAME
            EXIT PARAGRAPH
        END-IF
    END-PERFORM
    ADD 1 TO TAGS-CURRENT-TAG-LENGTH(TAG-INDEX)
    MOVE ENTRY-NAME TO TAGS-CURRENT-TAG-ENTRY(TAG-INDEX, TAGS-CURRENT-TAG-LENGTH(TAG-INDEX))
    .

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
    01 RECIPE-SKIPPED               BINARY-CHAR UNSIGNED.
    01 DISPLAY-INT                  PIC -(9)9.
LOCAL-STORAGE SECTION.
    *> counters for number of recipe files read vs. able to be loaded
    01 RECIPE-FILE-COUNT            BINARY-LONG UNSIGNED.
    01 RECIPE-FILES-LOADED          BINARY-LONG UNSIGNED.
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

        ADD 1 TO RECIPE-FILE-COUNT

        INITIALIZE ENTRY-PATH
        STRING FUNCTION TRIM(LK-ROOT-PATH) FUNCTION TRIM(LK-PACK-NAME) "/recipe/" FUNCTION TRIM(DIR-ENTRY) INTO ENTRY-PATH

        CALL "Datapack-LoadRecipe" USING ENTRY-PATH LK-FAILURE RECIPE-SKIPPED
        IF LK-FAILURE NOT = 0
            DISPLAY "Failure loading recipe: " FUNCTION TRIM(ENTRY-PATH)
            GOBACK
        END-IF

        IF RECIPE-SKIPPED = 0
            ADD 1 TO RECIPE-FILES-LOADED
        END-IF
    END-PERFORM

    CALL "CloseDirectory" USING DIR-HANDLE

    MOVE RECIPE-FILES-LOADED TO DISPLAY-INT
    DISPLAY FUNCTION TRIM(DISPLAY-INT) WITH NO ADVANCING
    DISPLAY " / " WITH NO ADVANCING
    MOVE RECIPE-FILE-COUNT TO DISPLAY-INT
    DISPLAY FUNCTION TRIM(DISPLAY-INT) WITH NO ADVANCING
    DISPLAY " recipes files loaded"

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
    01 RECIPE-TYPE                  PIC X(128).
LINKAGE SECTION.
    01 LK-RECIPE-PATH               PIC X ANY LENGTH.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.
    01 LK-SKIPPED                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-RECIPE-PATH LK-FAILURE LK-SKIPPED.
    MOVE 0 TO LK-FAILURE
    MOVE 1 TO LK-SKIPPED

    CALL "Files-ReadAll" USING LK-RECIPE-PATH JSON-BUFFER JSON-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0
        DISPLAY "Failure reading recipe file: " FUNCTION TRIM(LK-RECIPE-PATH)
        GOBACK
    END-IF

    *> Find just the specific parser to use

    MOVE 1 TO JSON-POS
    CALL "JsonParse-ObjectStart" USING JSON-BUFFER JSON-POS LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    CALL "JsonParse-FindValue" USING JSON-BUFFER JSON-POS "type" LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    CALL "JsonParse-String" USING JSON-BUFFER JSON-POS LK-FAILURE RECIPE-TYPE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    *> The parser starts from the beginning of the object for simplicity of implementation
    MOVE 1 TO JSON-POS

    EVALUATE RECIPE-TYPE
        WHEN "minecraft:crafting_shapeless"
            MOVE 0 TO LK-SKIPPED
            CALL "Parse-Recipe-Shapeless" USING JSON-BUFFER JSON-POS LK-FAILURE
        WHEN "minecraft:crafting_shaped"
            MOVE 0 TO LK-SKIPPED
            CALL "Parse-Recipe-Shaped" USING JSON-BUFFER JSON-POS LK-FAILURE
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

    GOBACK.

END PROGRAM Datapack-LoadRecipe.
