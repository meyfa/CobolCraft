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
    *> main template
    01 MAIN-TPL                     PIC X(1000)                 GLOBAL.
    01 MAIN-LENGTH                  BINARY-LONG UNSIGNED        GLOBAL.
    *> registration template
    01 REGISTRATION-TPL             PIC X(1000)                 GLOBAL.
    01 REGISTRATION-LENGTH          BINARY-LONG UNSIGNED        GLOBAL.
    *> callback template
    01 CALLBACK-TPL                 PIC X(1000)                 GLOBAL.
    01 CALLBACK-LENGTH              BINARY-LONG UNSIGNED        GLOBAL.
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

    CALL "Codegen-TemplateRead" USING LK-TPLDIR "blocks_loot_table/main.tpl.cob" MAIN-TPL MAIN-LENGTH
    CALL "Codegen-TemplateRead" USING LK-TPLDIR "blocks_loot_table/registration.tpl.cob" REGISTRATION-TPL REGISTRATION-LENGTH
    CALL "Codegen-TemplateRead" USING LK-TPLDIR "blocks_loot_table/callback.tpl.cob" CALLBACK-TPL CALLBACK-LENGTH

    SET MAIN-REPLACE-PTR TO ENTRY "MainTemplateReplace"
    SET REGISTRATION-REPLACE-PTR TO ENTRY "RegistrationTemplateReplace"
    SET CALLBACK-REPLACE-PTR TO ENTRY "CallbackTemplateReplace"

    CALL "Codegen-Start" USING LK-OUTDIR "blocks_loot_table.cob"

    CALL "Codegen-TemplateEval" USING MAIN-TPL MAIN-LENGTH MAIN-REPLACE-PTR BUFFER BUFFERLEN
    CALL "Codegen-Append" USING BUFFER(1:BUFFERLEN)

    CALL "Codegen-End"

    GOBACK.

    *> --- MainTemplateReplace ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. MainTemplateReplace.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 REPLACE-LENGTH           BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        01 LK-VARNAME               PIC X ANY LENGTH.
        01 LK-VALUE                 PIC X ANY LENGTH.
        01 LK-LENGTH                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-VARNAME LK-VALUE LK-LENGTH.
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

            CALL "Codegen-TemplateEval" USING REGISTRATION-TPL REGISTRATION-LENGTH REGISTRATION-REPLACE-PTR LK-VALUE(LK-LENGTH + 1:) REPLACE-LENGTH
            ADD REPLACE-LENGTH TO LK-LENGTH
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

            CALL "Codegen-TemplateEval" USING CALLBACK-TPL CALLBACK-LENGTH CALLBACK-REPLACE-PTR LK-VALUE(LK-LENGTH + 1:) REPLACE-LENGTH
            ADD REPLACE-LENGTH TO LK-LENGTH
        END-PERFORM
        .

    END PROGRAM MainTemplateReplace.

    *> --- RegistrationTemplateReplace ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. RegistrationTemplateReplace.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-VARNAME               PIC X ANY LENGTH.
        01 LK-VALUE                 PIC X ANY LENGTH.
        01 LK-LENGTH                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-VARNAME LK-VALUE LK-LENGTH.
        EVALUATE LK-VARNAME
            WHEN "PROGID"
                MOVE PROGID TO LK-VALUE
                MOVE FUNCTION STORED-CHAR-LENGTH(LK-VALUE) TO LK-LENGTH
            WHEN "BLOCK-NAME"
                INITIALIZE LK-VALUE
                STRING "minecraft:" FUNCTION TRIM(BLOCK-NAME) INTO LK-VALUE
                MOVE FUNCTION STORED-CHAR-LENGTH(LK-VALUE) TO LK-LENGTH
        END-EVALUATE.

    END PROGRAM RegistrationTemplateReplace.

    *> --- CallbackTemplateReplace ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. CallbackTemplateReplace.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-VARNAME               PIC X ANY LENGTH.
        01 LK-VALUE                 PIC X ANY LENGTH.
        01 LK-LENGTH                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-VARNAME LK-VALUE LK-LENGTH.
        EVALUATE LK-VARNAME
            WHEN "PROGID"
                MOVE PROGID TO LK-VALUE
                MOVE FUNCTION STORED-CHAR-LENGTH(LK-VALUE) TO LK-LENGTH
            WHEN "ITEM-NAME"
                INITIALIZE LK-VALUE
                STRING "minecraft:" FUNCTION TRIM(ITEM-NAME) INTO LK-VALUE
                MOVE FUNCTION STORED-CHAR-LENGTH(LK-VALUE) TO LK-LENGTH
        END-EVALUATE.

    END PROGRAM CallbackTemplateReplace.

END PROGRAM Codegen-BlocksLootTable.
