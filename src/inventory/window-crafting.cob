*> --- RegisterWindow-Crafting ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterWindow-Crafting.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 WINDOW-TYPE                  BINARY-LONG.
    01 SYNC-PTR                     PROGRAM-POINTER.
    01 CLOSE-PTR                    PROGRAM-POINTER.
    01 SET-SLOT-PTR                 PROGRAM-POINTER.

PROCEDURE DIVISION.
    CALL "Registries-Get-EntryId" USING "minecraft:menu" "minecraft:crafting" WINDOW-TYPE
    IF WINDOW-TYPE < 0
        DISPLAY "RegisterBlock-CraftingTable: Failed to get window type ID"
        GOBACK
    END-IF

    SET SYNC-PTR TO ENTRY "Callback-Sync"
    SET CLOSE-PTR TO ENTRY "Callback-Close"
    SET SET-SLOT-PTR TO ENTRY "Callback-SetSlot"
    CALL "SetCallback-WindowSync" USING WINDOW-TYPE SYNC-PTR
    CALL "SetCallback-WindowClose" USING WINDOW-TYPE CLOSE-PTR
    CALL "SetCallback-WindowSetSlot" USING WINDOW-TYPE SET-SLOT-PTR

    GOBACK.

    *> --- Callback-Sync ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Sync.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        *> https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Inventory#Crafting_table
        *> 0 = crafting output, 1..9 = crafting grid, 10..36 = player inventory, 37..45 = hotbar
        01 WINDOW-INVENTORY-LENGTH      BINARY-LONG UNSIGNED    VALUE 46.
        01 WINDOW-SLOTS.
            02 WINDOW-SLOT OCCURS 46 TIMES.
                COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==WINDOW==.
        01 SLOT                         BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-WINDOW-SYNC.

    PROCEDURE DIVISION USING LK-PLAYER.
        *> TODO can we avoid the copies?
        PERFORM VARYING SLOT FROM 0 BY 1 UNTIL SLOT > 9
            MOVE PLAYER-WINDOW-SLOT(LK-PLAYER, SLOT + 1) TO WINDOW-SLOT(SLOT + 1)
        END-PERFORM
        PERFORM VARYING SLOT FROM 10 BY 1 UNTIL SLOT > 45
            MOVE PLAYER-INVENTORY-SLOT(LK-PLAYER, SLOT) TO WINDOW-SLOT(SLOT + 1)
        END-PERFORM

        ADD 1 TO PLAYER-WINDOW-STATE(LK-PLAYER)
        CALL "SendPacket-SetContainerContent" USING PLAYER-CLIENT(LK-PLAYER) PLAYER-WINDOW-ID(LK-PLAYER) PLAYER-WINDOW-STATE(LK-PLAYER)
            WINDOW-INVENTORY-LENGTH WINDOW-SLOTS PLAYER-MOUSE-ITEM(LK-PLAYER)

        GOBACK.

    END PROGRAM Callback-Sync.

    *> --- Callback-Close ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Close.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 SLOT                     BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-WINDOW-CLOSE.

    PROCEDURE DIVISION USING LK-PLAYER.
        *> stash the mouse item
        CALL "Inventory-StoreItem" USING PLAYER-INVENTORY(LK-PLAYER) PLAYER-MOUSE-ITEM(LK-PLAYER)
        IF PLAYER-MOUSE-SLOT-COUNT(LK-PLAYER) > 0
            *> TODO drop item
            MOVE 0 TO PLAYER-MOUSE-SLOT-COUNT(LK-PLAYER)
        END-IF

        *> stash crafting input
        PERFORM VARYING SLOT FROM 1 BY 1 UNTIL SLOT > 9
            CALL "Inventory-StoreItem" USING PLAYER-INVENTORY(LK-PLAYER) PLAYER-WINDOW-SLOT(LK-PLAYER, SLOT + 1)
            IF PLAYER-WINDOW-SLOT(LK-PLAYER, SLOT + 1) > 0
                *> TODO drop item
                MOVE 0 TO PLAYER-WINDOW-SLOT(LK-PLAYER, SLOT + 1)
            END-IF
        END-PERFORM

        CALL "Inventory-SyncPlayerInventory" USING LK-PLAYER

        GOBACK.

    END PROGRAM Callback-Close.

    *> --- Callback-SetSlot ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-SetSlot.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
    LINKAGE SECTION.
        COPY DD-CALLBACK-WINDOW-SET-SLOT.

    PROCEDURE DIVISION USING LK-PLAYER LK-INDEX LK-SLOT.
        EVALUATE LK-INDEX
            WHEN 0 *> crafting output
                *> TODO
                CONTINUE

            WHEN 1 THRU 9 *> crafting input
                MOVE LK-SLOT TO PLAYER-WINDOW-SLOT(LK-PLAYER, LK-INDEX + 1)

            WHEN 10 THRU 45 *> player inventory
                MOVE LK-SLOT TO PLAYER-INVENTORY-SLOT(LK-PLAYER, LK-INDEX)

            WHEN OTHER
                DISPLAY "Invalid slot number: " LK-INDEX
        END-EVALUATE
        GOBACK.

    END PROGRAM Callback-SetSlot.

END PROGRAM RegisterWindow-Crafting.
