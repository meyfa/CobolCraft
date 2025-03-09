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
        WHEN "REGISTRATIONS"
            PERFORM VARYING DIR-INDEX FROM 1 BY 1 UNTIL DIR-INDEX > LOOT-TABLE-DIR-SIZE
                MOVE LOOT-TABLE-DIR-NAME(DIR-INDEX) TO CURRENT-FILENAME
                CALL "Codegen-TemplateEval" USING REGISTRATION-TPL LK-BUFFER LK-LENGTH
            END-PERFORM

        WHEN "CALLBACKS"
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
        WHEN "PROGID"
            CALL "Codegen-ProgramId" USING "CB-" BLOCK-NAME PROGID
            STRING PROGID INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD FUNCTION STORED-CHAR-LENGTH(PROGID) TO LK-LENGTH
        WHEN "BLOCK-NAME"
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
        WHEN "PROGID"
            MOVE CURRENT-FILENAME TO BLOCK-NAME
            CALL "TrimFileExt" USING BLOCK-NAME
            CALL "Codegen-ProgramId" USING "CB-" BLOCK-NAME PROGID
            STRING PROGID INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD FUNCTION STORED-CHAR-LENGTH(PROGID) TO LK-LENGTH
        WHEN "BODY"
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
        WHEN "CONDITIONS"
            PERFORM ReplaceConditions
        WHEN "BODY"
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
PROGRAM-ID. CG-BlocksLootTable-Conditions.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-EXPLOSION==.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CONDITION-MATCH-TOOL==.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 CONDITION-TYPE               PIC X(64).
LINKAGE SECTION.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/condition-explosion.tpl.cob" OMITTED CONDITION-EXPLOSION-TPL
        CALL "Codegen-TemplateLoad" USING "blocks_loot_table/condition-match-tool.tpl.cob" OMITTED CONDITION-MATCH-TOOL-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    CALL "JsonParse-ArrayStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        PERFORM ParsePoolCondition
        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    GOBACK.

ParsePoolCondition.
    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "condition"
                CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE CONDITION-TYPE
                PERFORM AssertOk
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    EVALUATE CONDITION-TYPE
        WHEN "minecraft:survives_explosion"
            CALL "Codegen-TemplateEval" USING CONDITION-EXPLOSION-TPL LK-BUFFER LK-LENGTH
        WHEN "minecraft:match_tool"
            CALL "Codegen-TemplateEval" USING CONDITION-MATCH-TOOL-TPL LK-BUFFER LK-LENGTH
    END-EVALUATE
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-Conditions: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-Conditions.

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
        WHEN OTHER
            CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
            PERFORM AssertOk

            *> TODO Move this into a template
            STRING "        CONTINUE" X"0A" INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 17 TO LK-LENGTH
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
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 JSONPOS-BACKUP               BINARY-LONG UNSIGNED.
LOCAL-STORAGE SECTION.
    01 ALT-INDEX                    BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-LENGTH.
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
    *> TODO Move all of this into a template

    MOVE 0 TO ALT-INDEX

    CALL "JsonParse-ArrayStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        IF ALT-INDEX > 0
            STRING
                "        ELSE" X"0A"
                "        MOVE 1 TO COND" X"0A"
            INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 36 TO LK-LENGTH
        END-IF

        MOVE JSONPOS TO JSONPOS-BACKUP
        CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
        PERFORM AssertOk
        CALL "JsonParse-FindValue" USING JSONBUF JSONPOS "conditions" FAILURE
        IF FAILURE = 0
            CALL "CG-BlocksLootTable-Conditions" USING LK-BUFFER LK-LENGTH
        END-IF
        MOVE JSONPOS-BACKUP TO JSONPOS

        STRING "        IF COND NOT = 0" X"0A" INTO LK-BUFFER(LK-LENGTH + 1:)
        ADD 24 TO LK-LENGTH

        CALL "CG-BlocksLootTable-Entry" USING LK-BUFFER LK-LENGTH

        ADD 1 TO ALT-INDEX

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    PERFORM ALT-INDEX TIMES
        STRING "        END-IF" X"0A" INTO LK-BUFFER(LK-LENGTH + 1:)
        ADD 15 TO LK-LENGTH
    END-PERFORM

    CALL "JsonParse-ArrayEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-Alternative: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-Alternative.

*> --- CG-BlocksLootTable-EntryItem ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-BlocksLootTable-EntryItem.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    01 CURRENT-FILENAME             PIC X(255)                  EXTERNAL.
    01 JSONBUF                      PIC X(16000)                EXTERNAL.
    01 JSONPOS                      BINARY-LONG UNSIGNED        EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 ITEM-NAME                    PIC X(64).
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "name"
                CALL "JsonParse-String" USING JSONBUF JSONPOS FAILURE ITEM-NAME
                PERFORM AssertOk
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    EVALUATE LK-VARNAME
        WHEN "ITEM-NAME"
            STRING ITEM-NAME INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD FUNCTION STORED-CHAR-LENGTH(ITEM-NAME) TO LK-LENGTH
    END-EVALUATE

    GOBACK.

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="CG-BlocksLootTable-EntryItem: Failed to parse JSON: " FUNCTION TRIM(CURRENT-FILENAME)==.
    .

END PROGRAM CG-BlocksLootTable-EntryItem.
