*> --- CG-BlocksLootTable ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> output buffer
    01 BUFFER                       PIC X(10000000).
    01 BUFFERLEN                    BINARY-LONG UNSIGNED.
    *> template
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==MAIN==.

PROCEDURE DIVISION.
    SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-Main"
    CALL "Codegen-TemplateLoad" USING "blocks_loot_table/main.tpl.cob" REPLACE-PTR MAIN-TPL

    CALL "Codegen-Start" USING "blocks_loot_table.cob"

    MOVE 0 TO BUFFERLEN
    CALL "Codegen-TemplateEval" USING MAIN-TPL BUFFER BUFFERLEN

    *> The loot table code is quite large and complex, so there may be redundant MOVEs and other optimizations possible.
    CALL "Codegen-Optimize" USING BUFFER(1:BUFFERLEN)

    CALL "Codegen-Append" USING BUFFER(1:BUFFERLEN)
    CALL "Codegen-End"

    GOBACK.

END PROGRAM CG-BlocksLootTable.

*> --- CG-BlocksLootTable-Main ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-Main.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==REGISTRATION==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CALLBACK==.
    *> List of files in the directory
    COPY DD-CODEGEN-DIR-LIST REPLACING LEADING ==PREFIX== BY ==LOOT-TABLE==.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    *> dynamic variables
    01 DIR-INDEX                    BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-Register"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/registration.tpl.cob" REPLACE-PTR REGISTRATION-TPL
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-Callback"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/callback.tpl.cob" REPLACE-PTR CALLBACK-TPL

        CALL "Codegen-ReadDataDirectory" USING "generated/data/minecraft/loot_table/blocks" LOOT-TABLE-DIR

        MOVE 1 TO INIT-DONE
    END-IF

    EVALUATE LK-VARNAME
        WHEN "registrations"
            PERFORM VARYING DIR-INDEX FROM 1 BY 1 UNTIL DIR-INDEX > LOOT-TABLE-DIR-SIZE
                MOVE LOOT-TABLE-DIR-NAME(DIR-INDEX) TO CURRENT-FILENAME
                CALL "Codegen-TemplateEval" USING REGISTRATION-TPL LK-BUFFER LK-LENGTH
            END-PERFORM

        WHEN "callbacks"
            PERFORM VARYING DIR-INDEX FROM 1 BY 1 UNTIL DIR-INDEX > LOOT-TABLE-DIR-SIZE
                MOVE LOOT-TABLE-DIR-NAME(DIR-INDEX) TO CURRENT-FILENAME
                CALL "Codegen-TemplateEval" USING CALLBACK-TPL LK-BUFFER LK-LENGTH
            END-PERFORM
    END-EVALUATE

    GOBACK.

END PROGRAM CG-BlocksLootTable-Main.

*> --- CG-BlocksLootTable-Register ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-Register.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    01 BLOCK-NAME                   PIC X(64).
    01 PROGID                       PIC X(30).
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    MOVE CURRENT-FILENAME TO BLOCK-NAME
    CALL "TrimFileExt" USING BLOCK-NAME

    EVALUATE LK-VARNAME
        WHEN "progid"
            CALL "Codegen-ProgramId" USING "CB-" BLOCK-NAME PROGID
            STRING PROGID INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD FUNCTION STORED-CHAR-LENGTH(PROGID) TO LK-LENGTH
        WHEN "block-name"
            STRING "minecraft:" FUNCTION TRIM(BLOCK-NAME) INTO LK-BUFFER(LK-LENGTH + 1:)
            COMPUTE LK-LENGTH = LK-LENGTH + 10 + FUNCTION STORED-CHAR-LENGTH(BLOCK-NAME)
    END-EVALUATE

    GOBACK.

END PROGRAM CG-BlocksLootTable-Register.

*> --- CG-BlocksLootTable-Callback ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-Callback.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==POOL==.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    *> dynamic variables
    01 BLOCK-NAME                   PIC X(64).
    01 PROGID                       PIC X(30).
    01 FILEPATH                     PIC X(255).
    01 JSONLEN                      BINARY-LONG UNSIGNED.
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-Pool"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/pool.tpl.cob" REPLACE-PTR POOL-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    EVALUATE LK-VARNAME
        WHEN "progid"
            MOVE CURRENT-FILENAME TO BLOCK-NAME
            CALL "TrimFileExt" USING BLOCK-NAME
            CALL "Codegen-ProgramId" USING "CB-" BLOCK-NAME PROGID
            STRING PROGID INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD FUNCTION STORED-CHAR-LENGTH(PROGID) TO LK-LENGTH
        WHEN "body"
            PERFORM ReplaceBody
    END-EVALUATE

    GOBACK.

ReplaceBody.
    INITIALIZE FILEPATH
    STRING "generated/data/minecraft/loot_table/blocks/" FUNCTION TRIM(CURRENT-FILENAME) INTO FILEPATH
    CALL "Codegen-ReadDataFile" USING FILEPATH JSONBUF JSONLEN

    MOVE 1 TO JSONPOS
    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "pools"
                PERFORM ParsePools
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    .

ParsePools.
    CALL "JsonParse-ArrayStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "Codegen-TemplateEval" USING POOL-TPL LK-BUFFER LK-LENGTH
        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-Callback: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-Callback.

*> --- CG-BlocksLootTable-Pool ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-Pool.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    01 FUNCTION-POSITIONS                                       EXTERNAL.
        02 FUNCTION-COUNT           BINARY-LONG UNSIGNED.
        02 FUNCTION-POSITION        BINARY-LONG UNSIGNED OCCURS 0 TO 16 TIMES DEPENDING ON FUNCTION-COUNT.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 JSONPOS-BACKUP               BINARY-LONG UNSIGNED.
    *> offsets into the JSON for the different pool entries
    01 POOL-ENTRY-COUNT             BINARY-LONG UNSIGNED.
    01 POOL-ENTRY-OFFSET            BINARY-LONG UNSIGNED
        OCCURS 0 TO 16 TIMES DEPENDING ON POOL-ENTRY-COUNT.
    01 POOL-ENTRY-INDEX             BINARY-LONG UNSIGNED.
    01 POOL-ROLLS                   FLOAT-LONG.
    01 POOL-BONUS-ROLLS             FLOAT-LONG.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    MOVE 0 TO POOL-ENTRY-COUNT
    MOVE 0 TO FUNCTION-COUNT

    EVALUATE LK-VARNAME
        WHEN "conditions"
            PERFORM ReplaceConditions
        WHEN "body"
            PERFORM ReplaceBody
    END-EVALUATE

    GOBACK.

ReplaceConditions.
    MOVE JSONPOS TO JSONPOS-BACKUP

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    CALL "JsonParse-FindValue" USING JSONBUF JSONPOS "conditions" FAILURE
    IF FAILURE = 0
        CALL "CG-BlocksLootTable-Conditions" USING LK-BUFFER LK-LENGTH
    END-IF

    MOVE JSONPOS-BACKUP TO JSONPOS
    .

ReplaceBody.
    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "rolls"
                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE POOL-ROLLS
                PERFORM AssertOk

            WHEN "bonus_rolls"
                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE POOL-BONUS-ROLLS
                PERFORM AssertOk

            WHEN "entries"
                *> For codegen, we need the other properties before we can parse the entries.
                *> So we store the offset and parse the entries later.
                CALL "JsonParse-ArrayStart" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

                PERFORM UNTIL EXIT
                    ADD 1 TO POOL-ENTRY-COUNT
                    MOVE JSONPOS TO POOL-ENTRY-OFFSET(POOL-ENTRY-COUNT)
                    CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                    PERFORM AssertOk
                    CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
                    IF FAILURE > 0 EXIT PERFORM END-IF
                END-PERFORM

                CALL "JsonParse-ArrayEnd" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

            WHEN "functions"
                CALL "JsonParse-ArrayStart" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

                PERFORM UNTIL EXIT
                    ADD 1 TO FUNCTION-COUNT
                    MOVE JSONPOS TO FUNCTION-POSITION(FUNCTION-COUNT)
                    CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                    PERFORM AssertOk
                    CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
                    IF FAILURE > 0 EXIT PERFORM END-IF
                END-PERFORM

                CALL "JsonParse-ArrayEnd" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

            WHEN "conditions"
                *> already handled
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

            WHEN OTHER
                COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-Pool: Unexpected key: " FUNCTION TRIM(STR)==.
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    *> In practice, currently (1.21.4) all data files match these conditions. To keep it simple, error out if not.
    COPY ASSERT REPLACING COND BY ==POOL-ROLLS = 1.0 AND POOL-BONUS-ROLLS = 0.0 AND POOL-ENTRY-COUNT = 1==,
        MSG BY =="CG-BlocksLootTable-Pool: Unexpected pool data: " FUNCTION TRIM(CURRENT-FILENAME)==.

    *> Generate the pool code.
    MOVE JSONPOS TO JSONPOS-BACKUP
    PERFORM ParsePoolEntries
    MOVE JSONPOS-BACKUP TO JSONPOS
    .

ParsePoolEntries.
    PERFORM VARYING POOL-ENTRY-INDEX FROM 1 BY 1 UNTIL POOL-ENTRY-INDEX > POOL-ENTRY-COUNT
        MOVE POOL-ENTRY-OFFSET(POOL-ENTRY-INDEX) TO JSONPOS
        CALL "CG-BlocksLootTable-Entry" USING LK-BUFFER LK-LENGTH
    END-PERFORM
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-Pool: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-Pool.

*> --- CG-BlocksLootTable-Conditions ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-Conditions IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-LENGTH.
    CALL "JsonParse-ArrayStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "CG-BlocksLootTable-Condition" USING LK-BUFFER LK-LENGTH
        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    GOBACK.

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-Conditions: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-Conditions.

*> --- CG-BlocksLootTable-Condition ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-Condition IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-INVERTED==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-EXPLOSION==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-MATCH-TOOL==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-BLOCK-STATE-PROPERTY==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-RANDOM-CHANCE==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-TABLE-BONUS==.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    01 CONDITION-CHANCE             FLOAT-LONG                  EXTERNAL.
    01 CONDITION-CHANCES-POS        BINARY-LONG UNSIGNED        EXTERNAL.
    01 CONDITION-ENCHANTMENT        PIC X(64)                   EXTERNAL.
    01 CONDITION-TERM-POS           BINARY-LONG UNSIGNED        EXTERNAL.
    01 CONDITION-BLOCK              PIC X(64)                   EXTERNAL.
    01 CONDITION-PROPERTIES-POS     BINARY-LONG UNSIGNED        EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 CONDITION-TYPE               PIC X(64).
LINKAGE SECTION.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-CondInvert"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/condition-inverted.tpl.cob" REPLACE-PTR CONDITION-INVERTED-TPL
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/condition-explosion.tpl.cob" OMITTED CONDITION-EXPLOSION-TPL
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/condition-match-tool.tpl.cob" OMITTED CONDITION-MATCH-TOOL-TPL
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-CondBlock"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/condition-block-state-property.tpl.cob" REPLACE-PTR CONDITION-BLOCK-STATE-PROPERTY-TPL
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-CondChance"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/condition-random-chance.tpl.cob" REPLACE-PTR CONDITION-RANDOM-CHANCE-TPL
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-CondTable"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/condition-table-bonus.tpl.cob" REPLACE-PTR CONDITION-TABLE-BONUS-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    INITIALIZE CONDITION-TYPE

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "condition"
                CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE CONDITION-TYPE
                PERFORM AssertOk
            WHEN "chance"
                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE CONDITION-CHANCE
                PERFORM AssertOk
            WHEN "chances"
                MOVE JSONPOS TO CONDITION-CHANCES-POS
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
            WHEN "enchantment"
                CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE CONDITION-ENCHANTMENT
                PERFORM AssertOk
            WHEN "term"
            WHEN "terms"
                MOVE JSONPOS TO CONDITION-TERM-POS
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
            WHEN "block"
                CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE CONDITION-BLOCK
                PERFORM AssertOk
            WHEN "properties"
                MOVE JSONPOS TO CONDITION-PROPERTIES-POS
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    EVALUATE CONDITION-TYPE
        WHEN "minecraft:inverted"
            CALL "Codegen-TemplateEval" USING CONDITION-INVERTED-TPL LK-BUFFER LK-LENGTH

        WHEN "minecraft:any_of"
            CALL "CG-BlocksLootTable-CondAnyOf" USING LK-BUFFER LK-LENGTH

        WHEN "minecraft:survives_explosion"
            CALL "Codegen-TemplateEval" USING CONDITION-EXPLOSION-TPL LK-BUFFER LK-LENGTH

        WHEN "minecraft:match_tool"
            CALL "Codegen-TemplateEval" USING CONDITION-MATCH-TOOL-TPL LK-BUFFER LK-LENGTH

        WHEN "minecraft:block_state_property"
            CALL "Codegen-TemplateEval" USING CONDITION-BLOCK-STATE-PROPERTY-TPL LK-BUFFER LK-LENGTH

        WHEN "minecraft:random_chance"
            CALL "Codegen-TemplateEval" USING CONDITION-RANDOM-CHANCE-TPL LK-BUFFER LK-LENGTH

        WHEN "minecraft:table_bonus"
            CALL "Codegen-TemplateEval" USING CONDITION-TABLE-BONUS-TPL LK-BUFFER LK-LENGTH

        WHEN "minecraft:location_check"
            *> TODO implement
            CONTINUE

        WHEN "minecraft:entity_properties"
            *> TODO implement
            *> Note: Even without any predicate for the entity, this condition can still serve a purpose:
            *>       It detects that the block was mined/exploded directly by an entity, vs. destroyed by a piston etc.
            CONTINUE

        WHEN OTHER
            COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-Condition: Unexpected condition type: " FUNCTION TRIM(CONDITION-TYPE)==.
    END-EVALUATE

    GOBACK.

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-Condition: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-Condition.

*> --- CG-BlocksLootTable-CondInvert ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-CondInvert IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CODEGEN-JSON.
    01 CONDITION-TERM-POS           BINARY-LONG UNSIGNED        EXTERNAL.
LOCAL-STORAGE SECTION.
    01 JSONPOS-BACKUP               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "term"
            MOVE JSONPOS TO JSONPOS-BACKUP
            MOVE CONDITION-TERM-POS TO JSONPOS
            CALL "CG-BlocksLootTable-Condition" USING LK-BUFFER LK-LENGTH
            MOVE JSONPOS-BACKUP TO JSONPOS
    END-EVALUATE.

END PROGRAM CG-BlocksLootTable-CondInvert.

*> --- CG-BlocksLootTable-CondAnyOf ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-CondAnyOf IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-ANY-OF==.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    01 CONDITION-TERM-POS           BINARY-LONG UNSIGNED        EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
LOCAL-STORAGE SECTION.
    01 JSONPOS-BACKUP               BINARY-LONG UNSIGNED.
    01 ALT-INDEX                    BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "ReplaceCallback"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/condition-any-of.tpl.cob" REPLACE-PTR CONDITION-ANY-OF-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    MOVE JSONPOS TO JSONPOS-BACKUP
    MOVE CONDITION-TERM-POS TO JSONPOS

    CALL "JsonParse-ArrayStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        IF ALT-INDEX = 0
            CALL "CG-BlocksLootTable-Condition" USING LK-BUFFER LK-LENGTH
        ELSE
            CALL "Codegen-TemplateEval" USING CONDITION-ANY-OF-TPL LK-BUFFER LK-LENGTH
        END-IF

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF

        ADD 1 TO ALT-INDEX
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    MOVE JSONPOS-BACKUP TO JSONPOS

    GOBACK.

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-CondAnyOf: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

    *> --- ReplaceCallback ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. ReplaceCallback.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-VARNAME               PIC X ANY LENGTH.
        01 LK-BUFFER                PIC X ANY LENGTH.
        01 LK-LENGTH                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
        EVALUATE LK-VARNAME
            WHEN "term"
                CALL "CG-BlocksLootTable-Condition" USING LK-BUFFER LK-LENGTH
        END-EVALUATE.

    END PROGRAM ReplaceCallback.

END PROGRAM CG-BlocksLootTable-CondAnyOf.

*> --- CG-BlocksLootTable-CondBlock ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-CondBlock.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    01 CONDITION-BLOCK              PIC X(64)                   EXTERNAL.
    01 CONDITION-PROPERTIES-POS     BINARY-LONG UNSIGNED        EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
LOCAL-STORAGE SECTION.
    01 JSONPOS-BACKUP               BINARY-LONG UNSIGNED.
    01 PROPERTIES-STR               PIC X(1024).
    01 PROPERTIES-STR-LENGTH        BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "block-name"
            STRING FUNCTION TRIM(CONDITION-BLOCK) INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD FUNCTION STORED-CHAR-LENGTH(CONDITION-BLOCK) TO LK-LENGTH
        WHEN "properties"
            MOVE JSONPOS TO JSONPOS-BACKUP
            MOVE CONDITION-PROPERTIES-POS TO JSONPOS
            PERFORM ParseProperties
            MOVE JSONPOS-BACKUP TO JSONPOS
    END-EVALUATE

    GOBACK.

ParseProperties.
    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        *> separate properties with a space
        IF PROPERTIES-STR-LENGTH > 0
            ADD 1 TO PROPERTIES-STR-LENGTH
        END-IF

        STRING FUNCTION TRIM(STR) INTO PROPERTIES-STR(PROPERTIES-STR-LENGTH + 1:)
        ADD FUNCTION STORED-CHAR-LENGTH(STR) TO PROPERTIES-STR-LENGTH

        CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE STR
        IF FAILURE NOT = 0
            COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-CondBlock: Property ranges not yet supported: " FUNCTION TRIM(CURRENT-FILENAME)==.
        END-IF

        MOVE "=" TO PROPERTIES-STR(PROPERTIES-STR-LENGTH + 1:)
        ADD 1 TO PROPERTIES-STR-LENGTH

        STRING FUNCTION TRIM(STR) INTO PROPERTIES-STR(PROPERTIES-STR-LENGTH + 1:)
        ADD FUNCTION STORED-CHAR-LENGTH(STR) TO PROPERTIES-STR-LENGTH

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    *> Avoid compiler warning due to empty string literal
    IF PROPERTIES-STR-LENGTH = 0
        MOVE 1 TO PROPERTIES-STR-LENGTH
    END-IF

    STRING PROPERTIES-STR(1:PROPERTIES-STR-LENGTH) INTO LK-BUFFER(LK-LENGTH + 1:)
    ADD PROPERTIES-STR-LENGTH TO LK-LENGTH
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-CondBlock: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-CondBlock.

*> --- CG-BlocksLootTable-CondChance ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-CondChance.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    01 CONDITION-CHANCE             FLOAT-LONG                  EXTERNAL.
    01 CHANCE-FORMAT                PIC 9.9(9).
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "chance"
            MOVE CONDITION-CHANCE TO CHANCE-FORMAT
            STRING CHANCE-FORMAT INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 11 TO LK-LENGTH
    END-EVALUATE.

END PROGRAM CG-BlocksLootTable-CondChance.

*> --- CG-BlocksLootTable-CondTable ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-CondTable.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    01 CONDITION-ENCHANTMENT        PIC X(64)                   EXTERNAL.
    01 CONDITION-CHANCES-POS        BINARY-LONG UNSIGNED        EXTERNAL.
    *> dynamic variables
    01 CHANCE-FLOAT                 FLOAT-LONG.
    01 FAILURE                      BINARY-CHAR UNSIGNED.
LOCAL-STORAGE SECTION.
    01 JSONPOS-BACKUP               BINARY-LONG UNSIGNED.
    01 CHANCE-STR                   PIC 9.9(9).
    01 CHANCES-STR                  PIC X(1024).
    01 CHANCES-STR-LENGTH           BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "enchantment"
            STRING FUNCTION TRIM(CONDITION-ENCHANTMENT) INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD FUNCTION STORED-CHAR-LENGTH(CONDITION-ENCHANTMENT) TO LK-LENGTH
        WHEN "chances"
            MOVE JSONPOS TO JSONPOS-BACKUP
            MOVE CONDITION-CHANCES-POS TO JSONPOS
            PERFORM AppendChances
            MOVE JSONPOS-BACKUP TO JSONPOS
    END-EVALUATE

    GOBACK.

AppendChances.
    CALL "JsonParse-ArrayStart" USING JSONBUF CONDITION-CHANCES-POS FAILURE
    PERFORM AssertOk

    INITIALIZE CHANCES-STR
    PERFORM UNTIL EXIT
        CALL "JsonParse-Float" USING JSONBUF CONDITION-CHANCES-POS FAILURE CHANCE-FLOAT
        PERFORM AssertOk

        IF CHANCES-STR-LENGTH > 0
            ADD 1 TO CHANCES-STR-LENGTH
        END-IF

        MOVE CHANCE-FLOAT TO CHANCE-STR
        STRING CHANCE-STR INTO CHANCES-STR(CHANCES-STR-LENGTH + 1:)
        ADD LENGTH OF CHANCE-STR TO CHANCES-STR-LENGTH

        CALL "JsonParse-Comma" USING JSONBUF CONDITION-CHANCES-POS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING JSONBUF CONDITION-CHANCES-POS FAILURE
    PERFORM AssertOk

    STRING CHANCES-STR(1:CHANCES-STR-LENGTH) INTO LK-BUFFER(LK-LENGTH + 1:)
    ADD CHANCES-STR-LENGTH TO LK-LENGTH
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-CondTable: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-CondTable.

*> --- CG-BlocksLootTable-Entry ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-Entry IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==ENTRY-ITEM==.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 JSONPOS-BACKUP               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-EntryItem"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/entry-item.tpl.cob" REPLACE-PTR ENTRY-ITEM-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    MOVE JSONPOS TO JSONPOS-BACKUP

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    CALL "JsonParse-FindValue" USING JSONBUF JSONPOS "type" FAILURE
    PERFORM AssertOk
    CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE STR
    PERFORM AssertOk

    MOVE JSONPOS-BACKUP TO JSONPOS

    EVALUATE STR
        WHEN "minecraft:item"
            CALL "Codegen-TemplateEval" USING ENTRY-ITEM-TPL LK-BUFFER LK-LENGTH

        WHEN "minecraft:alternatives"
            CALL "CG-BlocksLootTable-Alternative" USING LK-BUFFER LK-LENGTH

        WHEN "minecraft:dynamic"
            *> TODO implement
            CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
            PERFORM AssertOk
            STRING "        CONTINUE" X"0A" INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 17 TO LK-LENGTH

        WHEN OTHER
            COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-Entry: Unexpected entry type: " FUNCTION TRIM(STR)==.
    END-EVALUATE

    GOBACK.

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-Entry: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-Entry.

*> --- CG-BlocksLootTable-Alternative ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-Alternative IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==ENTRY-ALTERNATIVE==.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
LOCAL-STORAGE SECTION.
    01 ALT-INDEX                    BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-EntryAlt"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/entry-alternative.tpl.cob" REPLACE-PTR ENTRY-ALTERNATIVE-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "children"
                PERFORM ParseChildren

            WHEN "conditions"
                *> TODO implement
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

            WHEN "type"
                *> already handled
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

            WHEN OTHER
                COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-Alternative: Unexpected key: " FUNCTION TRIM(STR)==.
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    GOBACK.

ParseChildren.
    CALL "JsonParse-ArrayStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        IF ALT-INDEX = 0
            CALL "CG-BlocksLootTable-Entry" USING LK-BUFFER LK-LENGTH
        ELSE
            CALL "Codegen-TemplateEval" USING ENTRY-ALTERNATIVE-TPL LK-BUFFER LK-LENGTH
        END-IF

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF

        ADD 1 TO ALT-INDEX
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-Alternative: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-Alternative.

*> --- CG-BlocksLootTable-EntryAlt ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-EntryAlt IS RECURSIVE.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "child"
            CALL "CG-BlocksLootTable-Entry" USING LK-BUFFER LK-LENGTH
    END-EVALUATE.

END PROGRAM CG-BlocksLootTable-EntryAlt.

*> --- CG-BlocksLootTable-EntryItem ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-EntryItem.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==FUNCTION-SET-COUNT==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==FUNCTION-LIMIT-COUNT==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==FUNCTION-EXPLOSION-DECAY==.
    *> last filename and read position, to avoid double parse
    01 LAST-FILENAME                PIC X(255).
    01 LAST-JSONPOS                 BINARY-LONG UNSIGNED.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    01 FUNCTION-POSITIONS                                       EXTERNAL.
        02 FUNCTION-COUNT           BINARY-LONG UNSIGNED.
        02 FUNCTION-POSITION        BINARY-LONG UNSIGNED OCCURS 0 TO 16 TIMES DEPENDING ON FUNCTION-COUNT.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 ITEM-NAME                    PIC X(64).
    01 JSONPOS-BACKUP               BINARY-LONG UNSIGNED.
    01 FUNCTION-INDEX               BINARY-LONG UNSIGNED.
    01 FN-NAME                      PIC X(64).
    01 FN-CONDITIONS-POS            BINARY-LONG UNSIGNED        EXTERNAL.
    01 FN-ADD                       BINARY-CHAR UNSIGNED        EXTERNAL.
    01 FN-COUNT-POS                 BINARY-LONG UNSIGNED        EXTERNAL.
    01 FN-LIMIT-MIN                 FLOAT-LONG                  EXTERNAL.
    01 FN-LIMIT-MAX                 FLOAT-LONG                  EXTERNAL.
LOCAL-STORAGE SECTION.
    01 CONDITION-JSONPOS            BINARY-LONG UNSIGNED.
    01 PREV-FUNCTION-COUNT          BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-FnSetCount"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/function-set-count.tpl.cob" REPLACE-PTR FUNCTION-SET-COUNT-TPL
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-FnLimit"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/function-limit-count.tpl.cob" REPLACE-PTR FUNCTION-LIMIT-COUNT-TPL
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-FnExplDecay"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/function-explosion-decay.tpl.cob" REPLACE-PTR FUNCTION-EXPLOSION-DECAY-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    IF CURRENT-FILENAME NOT = LAST-FILENAME OR JSONPOS NOT = LAST-JSONPOS
        PERFORM ParseEntryItem
        MOVE JSONPOS TO LAST-JSONPOS
        MOVE CURRENT-FILENAME TO LAST-FILENAME
    END-IF

    EVALUATE LK-VARNAME
        WHEN "conditions"
            IF CONDITION-JSONPOS > 0
                MOVE JSONPOS TO JSONPOS-BACKUP
                MOVE CONDITION-JSONPOS TO JSONPOS
                CALL "CG-BlocksLootTable-Conditions" USING LK-BUFFER LK-LENGTH
                MOVE JSONPOS-BACKUP TO JSONPOS
            END-IF

        *> TODO Use the numeric item ID in codegen output
        WHEN "item-name"
            STRING ITEM-NAME INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD FUNCTION STORED-CHAR-LENGTH(ITEM-NAME) TO LK-LENGTH

        WHEN "functions"
            MOVE JSONPOS TO JSONPOS-BACKUP
            PERFORM GenerateFunctions
            MOVE JSONPOS-BACKUP TO JSONPOS

            *> Remove the functions added by just this entry to avoid impacting other entries
            MOVE PREV-FUNCTION-COUNT TO FUNCTION-COUNT
    END-EVALUATE

    GOBACK.

ParseEntryItem.
    MOVE FUNCTION-COUNT TO PREV-FUNCTION-COUNT

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "name"
                CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE ITEM-NAME
                PERFORM AssertOk

            WHEN "conditions"
                MOVE JSONPOS TO CONDITION-JSONPOS
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

            WHEN "functions"
                CALL "JsonParse-ArrayStart" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

                PERFORM UNTIL EXIT
                    ADD 1 TO FUNCTION-COUNT
                    MOVE JSONPOS TO FUNCTION-POSITION(FUNCTION-COUNT)
                    CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                    PERFORM AssertOk
                    CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
                    IF FAILURE > 0 EXIT PERFORM END-IF
                END-PERFORM

                CALL "JsonParse-ArrayEnd" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

            WHEN "type"
                *> already handled
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

            WHEN OTHER
                COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-EntryItem: Unexpected key: " FUNCTION TRIM(STR)==.
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    .

GenerateFunctions.
    PERFORM VARYING FUNCTION-INDEX FROM 1 BY 1 UNTIL FUNCTION-INDEX > FUNCTION-COUNT
        MOVE FUNCTION-POSITION(FUNCTION-INDEX) TO JSONPOS
        MOVE 0 TO FN-CONDITIONS-POS

        CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
        PERFORM AssertOk

        PERFORM UNTIL EXIT
            CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
            PERFORM AssertOk

            EVALUATE STR
                WHEN "conditions"
                    MOVE JSONPOS TO FN-CONDITIONS-POS
                    CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                    PERFORM AssertOk

                WHEN "function"
                    CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE FN-NAME
                    PERFORM AssertOk

                WHEN "add"
                    CALL "JsonParse-Boolean" USING JSONBUF JSONPOS FAILURE FN-ADD
                    PERFORM AssertOk

                WHEN "count"
                    MOVE JSONPOS TO FN-COUNT-POS
                    CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                    PERFORM AssertOk

                WHEN "limit"
                    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
                    PERFORM AssertOk

                    PERFORM UNTIL EXIT
                        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
                        PERFORM AssertOk

                        EVALUATE STR
                            WHEN "min"
                                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE FN-LIMIT-MIN
                                PERFORM AssertOk
                            WHEN "max"
                                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE FN-LIMIT-MAX
                                PERFORM AssertOk
                            WHEN OTHER
                                COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-EntryItem: Unexpected limit key: " FUNCTION TRIM(STR)==.
                        END-EVALUATE

                        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
                        IF FAILURE > 0 EXIT PERFORM END-IF
                    END-PERFORM

                    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
                    PERFORM AssertOk

                WHEN "enchantment"  *> "minecraft:apply_bonus"
                WHEN "formula"      *> "minecraft:apply_bonus"
                WHEN "parameters"   *> "minecraft:apply_bonus"
                WHEN "source"       *> "minecraft:copy_components"
                WHEN "include"      *> "minecraft:copy_components"
                WHEN "block"        *> "minecraft:copy_state"
                WHEN "properties"   *> "minecraft:copy_state"
                    *> TODO implement
                    CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                    PERFORM AssertOk

                WHEN OTHER
                    COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-EntryItem: Unexpected function key: " FUNCTION TRIM(STR)==.
            END-EVALUATE

            CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
            IF FAILURE > 0 EXIT PERFORM END-IF
        END-PERFORM

        EVALUATE FN-NAME
            WHEN "minecraft:set_count"
                CALL "Codegen-TemplateEval" USING FUNCTION-SET-COUNT-TPL LK-BUFFER LK-LENGTH

            WHEN "minecraft:limit_count"
                CALL "Codegen-TemplateEval" USING FUNCTION-LIMIT-COUNT-TPL LK-BUFFER LK-LENGTH

            WHEN "minecraft:explosion_decay"
                CALL "Codegen-TemplateEval" USING FUNCTION-EXPLOSION-DECAY-TPL LK-BUFFER LK-LENGTH

            WHEN "minecraft:apply_bonus"
                *> TODO implement
                CONTINUE

            WHEN "minecraft:copy_components"
                *> TODO implement
                CONTINUE

            WHEN "minecraft:copy_state"
                *> TODO implement
                CONTINUE

            WHEN OTHER
                COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-EntryItem: Unexpected function type: " FUNCTION TRIM(FN-NAME)==.
        END-EVALUATE
    END-PERFORM
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-EntryItem: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-EntryItem.

*> --- CG-BlocksLootTable-FnSetCount ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-FnSetCount.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    COPY DD-CODEGEN-JSON.
    01 FN-CONDITIONS-POS            BINARY-LONG UNSIGNED        EXTERNAL.
    01 FN-ADD                       BINARY-CHAR UNSIGNED        EXTERNAL.
    01 FN-COUNT-POS                 BINARY-LONG UNSIGNED        EXTERNAL.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "conditions"
            IF FN-CONDITIONS-POS > 0
                MOVE FN-CONDITIONS-POS TO JSONPOS
                CALL "CG-BlocksLootTable-Conditions" USING LK-BUFFER LK-LENGTH
            END-IF

        WHEN "add"
            IF FN-ADD = 0
                STRING "false" INTO LK-BUFFER(LK-LENGTH + 1:)
                ADD 5 TO LK-LENGTH
            ELSE
                STRING "true" INTO LK-BUFFER(LK-LENGTH + 1:)
                ADD 4 TO LK-LENGTH
            END-IF

        WHEN "count-provider"
            MOVE FN-COUNT-POS TO JSONPOS
            CALL "CG-BlocksLootTable-Number" USING LK-BUFFER LK-LENGTH
    END-EVALUATE

    GOBACK.

END PROGRAM CG-BlocksLootTable-FnSetCount.

*> --- CG-BlocksLootTable-FnLimit ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-FnLimit.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    COPY DD-CODEGEN-JSON.
    01 FN-CONDITIONS-POS            BINARY-LONG UNSIGNED        EXTERNAL.
    01 FN-LIMIT-MIN                 FLOAT-LONG                  EXTERNAL.
    01 FN-LIMIT-MAX                 FLOAT-LONG                  EXTERNAL.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "conditions"
            IF FN-CONDITIONS-POS > 0
                MOVE FN-CONDITIONS-POS TO JSONPOS
                CALL "CG-BlocksLootTable-Conditions" USING LK-BUFFER LK-LENGTH
            END-IF

        WHEN "min"
            CALL "Codegen-WriteFloat" USING FN-LIMIT-MIN LK-BUFFER LK-LENGTH

        WHEN "max"
            CALL "Codegen-WriteFloat" USING FN-LIMIT-MAX LK-BUFFER LK-LENGTH
    END-EVALUATE.

END PROGRAM CG-BlocksLootTable-FnLimit.

*> --- CG-BlocksLootTable-FnExplDecay ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-FnExplDecay.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    COPY DD-CODEGEN-JSON.
    01 FN-CONDITIONS-POS            BINARY-LONG UNSIGNED        EXTERNAL.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "conditions"
            IF FN-CONDITIONS-POS > 0
                MOVE FN-CONDITIONS-POS TO JSONPOS
                CALL "CG-BlocksLootTable-Conditions" USING LK-BUFFER LK-LENGTH
            END-IF
    END-EVALUATE.

END PROGRAM CG-BlocksLootTable-FnExplDecay.

*> --- CG-BlocksLootTable-Number ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-Number.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==NUMBER-CONSTANT==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==NUMBER-UNIFORM==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==NUMBER-BINOMIAL==.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    COPY DD-CODEGEN-JSON.
    01 NUM-CONSTANT-VALUE           FLOAT-LONG                  EXTERNAL.
    01 NUM-BINOM-N                  FLOAT-LONG                  EXTERNAL.
    01 NUM-BINOM-P                  FLOAT-LONG                  EXTERNAL.
    01 NUM-UNIFORM-MIN              FLOAT-LONG                  EXTERNAL.
    01 NUM-UNIFORM-MAX              FLOAT-LONG                  EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 NUM-PROVIDER-TYPE            PIC X(64).
LINKAGE SECTION.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-NumConstant"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/number-constant.tpl.cob" REPLACE-PTR NUMBER-CONSTANT-TPL
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-NumUniform"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/number-uniform.tpl.cob" REPLACE-PTR NUMBER-UNIFORM-TPL
        SET REPLACE-PTR TO ENTRY "CG-BlocksLootTable-NumBinomial"
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/number-binomial.tpl.cob" REPLACE-PTR NUMBER-BINOMIAL-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    *> shorthand for constant number provider
    CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE NUM-CONSTANT-VALUE
    IF FAILURE = 0
        CALL "Codegen-TemplateEval" USING NUMBER-CONSTANT-TPL LK-BUFFER LK-LENGTH
        GOBACK
    END-IF

    *> Type defaults to uniform distribution
    MOVE "minecraft:uniform" TO NUM-PROVIDER-TYPE

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "type"
                CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE NUM-PROVIDER-TYPE
                PERFORM AssertOk

            WHEN "value"
                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE NUM-CONSTANT-VALUE
                PERFORM AssertOk

            WHEN "n"
                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE NUM-BINOM-N
                PERFORM AssertOk

            WHEN "p"
                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE NUM-BINOM-P
                PERFORM AssertOk

            WHEN "min"
                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE NUM-UNIFORM-MIN
                PERFORM AssertOk

            WHEN "max"
                CALL "JsonParse-Float" USING JSONBUF JSONPOS FAILURE NUM-UNIFORM-MAX
                PERFORM AssertOk

            WHEN OTHER
                COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-Number: Unexpected key: " FUNCTION TRIM(STR)==.
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    EVALUATE NUM-PROVIDER-TYPE
        WHEN "minecraft:constant"
            CALL "Codegen-TemplateEval" USING NUMBER-CONSTANT-TPL LK-BUFFER LK-LENGTH

        WHEN "minecraft:uniform"
            CALL "Codegen-TemplateEval" USING NUMBER-UNIFORM-TPL LK-BUFFER LK-LENGTH

        WHEN "minecraft:binomial"
            CALL "Codegen-TemplateEval" USING NUMBER-BINOMIAL-TPL LK-BUFFER LK-LENGTH

        WHEN OTHER
            COPY ASSERT-FAILED REPLACING MSG BY =="CG-BlocksLootTable-Number: Unexpected number provider type: " FUNCTION TRIM(NUM-PROVIDER-TYPE)==.
    END-EVALUATE

    GOBACK.

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-Number: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-Number.

*> --- CG-BlocksLootTable-NumConstant ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-NumConstant.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NUM-CONSTANT-VALUE           FLOAT-LONG                  EXTERNAL.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "value"
            CALL "Codegen-WriteFloat" USING NUM-CONSTANT-VALUE LK-BUFFER LK-LENGTH
    END-EVALUATE.

END PROGRAM CG-BlocksLootTable-NumConstant.

*> --- CG-BlocksLootTable-NumUniform ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-NumUniform.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NUM-UNIFORM-MIN              FLOAT-LONG                  EXTERNAL.
    01 NUM-UNIFORM-MAX              FLOAT-LONG                  EXTERNAL.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "min"
            CALL "Codegen-WriteFloat" USING NUM-UNIFORM-MIN LK-BUFFER LK-LENGTH
        WHEN "max"
            CALL "Codegen-WriteFloat" USING NUM-UNIFORM-MAX LK-BUFFER LK-LENGTH
    END-EVALUATE.

END PROGRAM CG-BlocksLootTable-NumUniform.

*> --- CG-BlocksLootTable-NumBinomial ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-NumBinomial.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NUM-BINOM-N                  FLOAT-LONG                  EXTERNAL.
    01 NUM-BINOM-P                  FLOAT-LONG                  EXTERNAL.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "n"
            CALL "Codegen-WriteFloat" USING NUM-BINOM-N LK-BUFFER LK-LENGTH
        WHEN "p"
            CALL "Codegen-WriteFloat" USING NUM-BINOM-P LK-BUFFER LK-LENGTH
    END-EVALUATE.

END PROGRAM CG-BlocksLootTable-NumBinomial.
