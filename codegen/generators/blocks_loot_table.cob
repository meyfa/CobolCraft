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
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
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
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
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

            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
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
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
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
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-RANDOM-CHANCE==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-TABLE-BONUS==.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
    01 CONDITION-CHANCE             FLOAT-LONG                  EXTERNAL.
    01 CONDITION-CHANCES-POS        BINARY-LONG UNSIGNED        EXTERNAL.
    01 CONDITION-ENCHANTMENT        PIC X(64)                   EXTERNAL.
    01 CONDITION-TERM-POS           BINARY-LONG UNSIGNED        EXTERNAL.
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
            *> TODO implement
            CONTINUE

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
    *> shared state
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
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
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
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
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
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
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
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
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
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
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
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
    *> last filename and read position, to avoid double parse
    01 LAST-FILENAME                PIC X(255).
    01 LAST-JSONPOS                 BINARY-LONG UNSIGNED.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 ITEM-NAME                    PIC X(64).
    01 JSONPOS-BACKUP               BINARY-LONG UNSIGNED.
LOCAL-STORAGE SECTION.
    01 CONDITION-JSONPOS            BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
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
    END-EVALUATE

    GOBACK.

ParseEntryItem.
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

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-EntryItem: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-EntryItem.
