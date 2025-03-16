IDENTIFICATION DIVISION.
PROGRAM-ID. Generated-BlocksLootTable.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CB                       PROGRAM-POINTER.

PROCEDURE DIVISION.
$registrations:indent=4$
    GOBACK.

    IDENTIFICATION DIVISION.
    PROGRAM-ID. Register.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BLOCK-ID             BINARY-LONG.
    LINKAGE SECTION.
        01 LK-BLOCK-NAME        PIC X ANY LENGTH.
        01 LK-CALLBACK          PROGRAM-POINTER.

    PROCEDURE DIVISION USING LK-BLOCK-NAME LK-CALLBACK.
        CALL "Registries-Get-EntryId" USING "minecraft:block" LK-BLOCK-NAME BLOCK-ID
        CALL "SetCallback-BlockLoot" USING BLOCK-ID LK-CALLBACK
        GOBACK.

    END PROGRAM Register.
$callbacks:indent=4$

END PROGRAM Generated-BlocksLootTable.
