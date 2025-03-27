*> --- RegisterBlock-CraftingTable ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-CraftingTable.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 WINDOW-TYPE-CRAFTING     BINARY-LONG GLOBAL.
    01 INTERACT-PTR             PROGRAM-POINTER.
    01 BLOCK-ID                 BINARY-LONG.
    01 BLOCK-STATE              BINARY-LONG.

PROCEDURE DIVISION.
    SET INTERACT-PTR TO ENTRY "Callback-Interact"

    CALL "Registries-Lookup" USING "minecraft:menu" "minecraft:crafting" WINDOW-TYPE-CRAFTING
    IF WINDOW-TYPE-CRAFTING < 0
        DISPLAY "RegisterBlock-CraftingTable: Failed to get window type ID"
        GOBACK
    END-IF

    CALL "Registries-Lookup" USING "minecraft:block" "minecraft:crafting_table" BLOCK-ID
    CALL "Blocks-GetDefaultStateId" USING BLOCK-ID BLOCK-STATE
    CALL "SetCallback-BlockInteract" USING BLOCK-STATE INTERACT-PTR

    GOBACK.

    *> --- Callback-Interact ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Interact.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 WINDOW-SYNC-PTR          PROGRAM-POINTER.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-INTERACT.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> TODO handle already open window

        ADD 1 TO PLAYER-WINDOW-ID(LK-PLAYER)
        MOVE WINDOW-TYPE-CRAFTING TO PLAYER-WINDOW-TYPE(LK-PLAYER)
        CALL "SendPacket-OpenScreen" USING PLAYER-CLIENT(LK-PLAYER) PLAYER-WINDOW-ID(LK-PLAYER) PLAYER-WINDOW-TYPE(LK-PLAYER)

        INITIALIZE PLAYER-WINDOW-SLOTS(LK-PLAYER)

        *> TODO handle callback not found
        CALL "GetCallback-WindowSync" USING PLAYER-WINDOW-TYPE(LK-PLAYER) WINDOW-SYNC-PTR
        CALL WINDOW-SYNC-PTR USING LK-PLAYER

        GOBACK.

    END PROGRAM Callback-Interact.

END PROGRAM RegisterBlock-CraftingTable.
