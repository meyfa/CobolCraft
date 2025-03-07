IDENTIFICATION DIVISION.
PROGRAM-ID. Codegen-BlocksLootTable.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 GEN                          PIC X(4096).
    01 GEN-LENGTH                   BINARY-LONG UNSIGNED.
    01 DIR-PATH                     PIC X(255).
    01 DIR-PATH-LENGTH              BINARY-LONG UNSIGNED.
    01 DIR-HANDLE                   PIC X(8).
    01 DIR-ENTRY                    PIC X(255).
    01 EOF                          BINARY-CHAR UNSIGNED.
    01 BLOCK-NAME                   PIC X(255).
    01 PROGID                       PIC X(30).
LINKAGE SECTION.
    01 LK-DATADIR                   PIC X ANY LENGTH.
    01 LK-OUTDIR                    PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-DATADIR LK-OUTDIR.
    INITIALIZE DIR-PATH
    STRING FUNCTION TRIM(LK-DATADIR) "/generated/data/minecraft/loot_table/blocks" INTO DIR-PATH
    MOVE FUNCTION STORED-CHAR-LENGTH(DIR-PATH) TO DIR-PATH-LENGTH

    CALL "Codegen-Start" USING LK-OUTDIR "blocks_loot_table.cob"

    PERFORM WriteHeader
    PERFORM WriteHelpers
    PERFORM WriteCallbacks
    PERFORM WriteEnd

    CALL "Codegen-End"

    GOBACK.

WriteHeader.
    *> Program header
    INITIALIZE GEN
    STRING
        'IDENTIFICATION DIVISION.                                                ' X'0A'
        'PROGRAM-ID. GC-BlocksLootTable.                                         ' X'0A'
        X'0A'
        'DATA DIVISION.                                                          ' X'0A'
        'WORKING-STORAGE SECTION.                                                ' X'0A'
        '    01 CB-PTR                   PROGRAM-POINTER.                        ' X'0A'
        X'0A'
        'PROCEDURE DIVISION.                                                     ' X'0A'
    INTO GEN
    CALL "Codegen-Append" USING GEN

    *> Registration of callbacks
    CALL "OpenDirectory" USING DIR-PATH DIR-PATH-LENGTH DIR-HANDLE GIVING RETURN-CODE
    IF RETURN-CODE NOT = 0
        DISPLAY "Codegen: Failed to open directory: " FUNCTION TRIM(DIR-PATH) UPON STDERR
        STOP RUN
    END-IF

    PERFORM UNTIL EXIT
        CALL "ReadDirectory" USING DIR-HANDLE DIR-ENTRY GIVING EOF
        IF EOF > 0
            EXIT PERFORM
        END-IF

        MOVE DIR-ENTRY TO BLOCK-NAME
        CALL "TrimFileExt" USING BLOCK-NAME
        CALL "Codegen-ProgramId" USING "CB-" BLOCK-NAME PROGID

        INITIALIZE GEN
        STRING
            '    SET CB-PTR TO ENTRY "' FUNCTION TRIM(PROGID) '"' X'0A'
            '    CALL "Register" USING "minecraft:' FUNCTION TRIM(BLOCK-NAME) '" CB-PTR' X'0A'
        INTO GEN
        CALL "Codegen-Append" USING GEN
    END-PERFORM

    CALL "CloseDirectory" USING DIR-HANDLE

    INITIALIZE GEN
    STRING
        X'0A'
        '    GOBACK.                                                             ' X'0A'
        X'0A'
    INTO GEN
    CALL "Codegen-Append" USING GEN
    .

WriteHelpers.
    *> Helper programs
    INITIALIZE GEN
    STRING
        '    IDENTIFICATION DIVISION.                                            ' X'0A'
        '    PROGRAM-ID. Register.                                               ' X'0A'
        X'0A'
        '    DATA DIVISION.                                                      ' X'0A'
        '    WORKING-STORAGE SECTION.                                            ' X'0A'
        '        01 BLOCK-ID             BINARY-LONG.                            ' X'0A'
        '    LINKAGE SECTION.                                                    ' X'0A'
        '        01 LK-BLOCK-NAME        PIC X ANY LENGTH.                       ' X'0A'
        '        01 LK-CALLBACK          PROGRAM-POINTER.                        ' X'0A'
        X'0A'
        '    PROCEDURE DIVISION USING LK-BLOCK-NAME LK-CALLBACK.                 ' X'0A'
        '        CALL "Registries-Get-EntryId"                                   ' X'0A'
        '            USING "minecraft:block" LK-BLOCK-NAME BLOCK-ID              ' X'0A'
        '        CALL "SetCallback-BlockLoot" USING BLOCK-ID LK-CALLBACK         ' X'0A'
        '        GOBACK.                                                         ' X'0A'
        X'0A'
        '    END PROGRAM Register.                                               ' X'0A'
        X'0A'
    INTO GEN
    CALL "Codegen-Append" USING GEN
    .

WriteCallbacks.
    *> Callback programs
    CALL "OpenDirectory" USING DIR-PATH DIR-PATH-LENGTH DIR-HANDLE GIVING RETURN-CODE
    IF RETURN-CODE NOT = 0
        DISPLAY "Codegen: Failed to open directory: " FUNCTION TRIM(DIR-PATH) UPON STDERR
        STOP RUN
    END-IF

    PERFORM UNTIL EXIT
        CALL "ReadDirectory" USING DIR-HANDLE DIR-ENTRY GIVING EOF
        IF EOF > 0
            EXIT PERFORM
        END-IF

        MOVE DIR-ENTRY TO BLOCK-NAME
        CALL "TrimFileExt" USING BLOCK-NAME
        CALL "Codegen-ProgramId" USING "CB-" BLOCK-NAME PROGID

        *> Just something to test :)
        IF BLOCK-NAME = "stone"
            MOVE "cobblestone" TO BLOCK-NAME
        END-IF

        INITIALIZE GEN
        STRING
            '    IDENTIFICATION DIVISION.                                            ' X'0A'
            '    PROGRAM-ID. ' FUNCTION TRIM(PROGID) '.                              ' X'0A'
            X'0A'
            '    DATA DIVISION.                                                      ' X'0A'
            '    LINKAGE SECTION.                                                    ' X'0A'
            '        01 LK-ITEM-NAME         PIC X ANY LENGTH.                       ' X'0A'
            X'0A'
            '    PROCEDURE DIVISION USING LK-ITEM-NAME.                              ' X'0A'
            '        MOVE "minecraft:' FUNCTION TRIM(BLOCK-NAME) '" TO LK-ITEM-NAME  ' X'0A'
            '        GOBACK.                                                         ' X'0A'
            X'0A'
            '    END PROGRAM ' FUNCTION TRIM(PROGID) '.                              ' X'0A'
            X'0A'
        INTO GEN
        CALL "Codegen-Append" USING GEN
    END-PERFORM

    CALL "CloseDirectory" USING DIR-HANDLE
    .

WriteEnd.
    *> Program end
    INITIALIZE GEN
    STRING
        'END PROGRAM GC-BlocksLootTable.                                         ' X'0A'
    INTO GEN
    CALL "Codegen-Append" USING GEN
    .

END PROGRAM Codegen-BlocksLootTable.
