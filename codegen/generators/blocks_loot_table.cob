IDENTIFICATION DIVISION.
PROGRAM-ID. Codegen-BlocksLootTable.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> output buffer
    01 BUFFER                       PIC X(1000000).
    01 BUFFERLEN                    BINARY-LONG UNSIGNED.
    *> data directory
    01 DIR-PATH                     PIC X(255)                  GLOBAL.
    01 DIR-PATH-LENGTH              BINARY-LONG UNSIGNED        GLOBAL.
    01 DIR-HANDLE                   PIC X(8)                    GLOBAL.
    01 DIR-ENTRY                    PIC X(255)                  GLOBAL.
    01 EOF                          BINARY-CHAR UNSIGNED        GLOBAL.
    *> templates
    01 MAIN-TPL GLOBAL.
        COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==MAIN==.
    01 REGISTRATION-TPL GLOBAL.
        COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==REGISTRATION==.
    01 CALLBACK-TPL GLOBAL.
        COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==CALLBACK==.
    *> template replacement pointers
    01 MAIN-REPLACE-PTR             PROGRAM-POINTER             GLOBAL.
    01 REGISTRATION-REPLACE-PTR     PROGRAM-POINTER             GLOBAL.
    01 CALLBACK-REPLACE-PTR         PROGRAM-POINTER             GLOBAL.
    *> replacement strings
    01 PROGID                       PIC X(30)                   GLOBAL.
    01 BLOCK-NAME                   PIC X(255)                  GLOBAL.
    01 ITEM-NAME                    PIC X(255)                  GLOBAL.
LINKAGE SECTION.
    01 LK-DATADIR                   PIC X ANY LENGTH.
    01 LK-OUTDIR                    PIC X ANY LENGTH.
    01 LK-TPLDIR                    PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-DATADIR LK-OUTDIR LK-TPLDIR.
    INITIALIZE DIR-PATH
    STRING FUNCTION TRIM(LK-DATADIR) "/generated/data/minecraft/loot_table/blocks" INTO DIR-PATH
    MOVE FUNCTION STORED-CHAR-LENGTH(DIR-PATH) TO DIR-PATH-LENGTH

    CALL "Codegen-TemplateRead" USING LK-TPLDIR "blocks_loot_table/main.tpl.cob" MAIN-TPL
    CALL "Codegen-TemplateRead" USING LK-TPLDIR "blocks_loot_table/registration.tpl.cob" REGISTRATION-TPL
    CALL "Codegen-TemplateRead" USING LK-TPLDIR "blocks_loot_table/callback.tpl.cob" CALLBACK-TPL

    SET MAIN-REPLACE-PTR TO ENTRY "MainTemplateReplace"
    SET REGISTRATION-REPLACE-PTR TO ENTRY "RegistrationTemplateReplace"
    SET CALLBACK-REPLACE-PTR TO ENTRY "CallbackTemplateReplace"

    CALL "Codegen-Start" USING LK-OUTDIR "blocks_loot_table.cob"

    MOVE 0 TO BUFFERLEN
    CALL "Codegen-TemplateEval" USING MAIN-TPL MAIN-REPLACE-PTR BUFFER BUFFERLEN
    CALL "Codegen-Append" USING BUFFER(1:BUFFERLEN)

    CALL "Codegen-End"

    GOBACK.

    *> --- MainTemplateReplace ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. MainTemplateReplace.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-VARNAME               PIC X ANY LENGTH.
        01 LK-BUFFER                PIC X ANY LENGTH.
        01 LK-LENGTH                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
        CALL "OpenDirectory" USING DIR-PATH DIR-PATH-LENGTH DIR-HANDLE GIVING EOF
        IF EOF > 0
            DISPLAY "Codegen: Failed to open directory: " FUNCTION TRIM(DIR-PATH) UPON STDERR
            STOP RUN RETURNING 1
        END-IF

        EVALUATE LK-VARNAME
            WHEN "REGISTRATIONS"
                PERFORM ReplaceRegistrations
            WHEN "CALLBACKS"
                PERFORM ReplaceCallbacks
        END-EVALUATE

        CALL "CloseDirectory" USING DIR-HANDLE

        GOBACK.

    ReplaceRegistrations.
        PERFORM UNTIL EXIT
            CALL "ReadDirectory" USING DIR-HANDLE DIR-ENTRY GIVING EOF
            IF EOF > 0
                EXIT PERFORM
            END-IF

            MOVE DIR-ENTRY TO BLOCK-NAME
            CALL "TrimFileExt" USING BLOCK-NAME
            CALL "Codegen-ProgramId" USING "CB-" BLOCK-NAME PROGID

            CALL "Codegen-TemplateEval" USING REGISTRATION-TPL REGISTRATION-REPLACE-PTR LK-BUFFER LK-LENGTH
        END-PERFORM
        .

    ReplaceCallbacks.
        PERFORM UNTIL EXIT
            CALL "ReadDirectory" USING DIR-HANDLE DIR-ENTRY GIVING EOF
            IF EOF > 0
                EXIT PERFORM
            END-IF

            MOVE DIR-ENTRY TO BLOCK-NAME
            CALL "TrimFileExt" USING BLOCK-NAME
            CALL "Codegen-ProgramId" USING "CB-" BLOCK-NAME PROGID

            MOVE BLOCK-NAME TO ITEM-NAME

            *> just an example :)
            IF BLOCK-NAME = "stone"
                MOVE "cobblestone" TO ITEM-NAME
            END-IF

            CALL "Codegen-TemplateEval" USING CALLBACK-TPL CALLBACK-REPLACE-PTR LK-BUFFER LK-LENGTH
        END-PERFORM
        .

    END PROGRAM MainTemplateReplace.

    *> --- RegistrationTemplateReplace ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. RegistrationTemplateReplace.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-VARNAME               PIC X ANY LENGTH.
        01 LK-BUFFER                PIC X ANY LENGTH.
        01 LK-LENGTH                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
        EVALUATE LK-VARNAME
            WHEN "PROGID"
                MOVE PROGID TO LK-BUFFER(LK-LENGTH + 1:)
                ADD FUNCTION STORED-CHAR-LENGTH(PROGID) TO LK-LENGTH
            WHEN "BLOCK-NAME"
                STRING "minecraft:" FUNCTION TRIM(BLOCK-NAME) INTO LK-BUFFER(LK-LENGTH + 1:)
                COMPUTE LK-LENGTH = LK-LENGTH + 10 + FUNCTION STORED-CHAR-LENGTH(BLOCK-NAME)
        END-EVALUATE.

    END PROGRAM RegistrationTemplateReplace.

    *> --- CallbackTemplateReplace ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. CallbackTemplateReplace.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-VARNAME               PIC X ANY LENGTH.
        01 LK-BUFFER                PIC X ANY LENGTH.
        01 LK-LENGTH                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
        EVALUATE LK-VARNAME
            WHEN "PROGID"
                MOVE PROGID TO LK-BUFFER(LK-LENGTH + 1:)
                ADD FUNCTION STORED-CHAR-LENGTH(PROGID) TO LK-LENGTH
            WHEN "ITEM-NAME"
                STRING "minecraft:" FUNCTION TRIM(ITEM-NAME) INTO LK-BUFFER(LK-LENGTH + 1:)
                COMPUTE LK-LENGTH = LK-LENGTH + 10 + FUNCTION STORED-CHAR-LENGTH(ITEM-NAME)
        END-EVALUATE.

    END PROGRAM CallbackTemplateReplace.

END PROGRAM Codegen-BlocksLootTable.
