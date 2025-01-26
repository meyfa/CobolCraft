*> --- Inventory-SyncPlayerInventory ---
*> Utility to synchronize the player's inventory with the client.
*> This can be called even with other windows open, and will always update the player's inventory (window ID 0).
IDENTIFICATION DIVISION.
PROGRAM-ID. Inventory-SyncPlayerInventory.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    01 PLAYER-INVENTORY-WINDOW-TYPE BINARY-LONG                 VALUE -1.
    01 SYNC-PTR                     PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-PLAYER                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER.
    IF SYNC-PTR = NULL
        CALL "GetCallback-WindowSync" USING PLAYER-INVENTORY-WINDOW-TYPE SYNC-PTR
    END-IF
    CALL SYNC-PTR USING LK-PLAYER
    GOBACK.

END PROGRAM Inventory-SyncPlayerInventory.

*> --- Inventory-PickItem ---
*> Pick an item into the player's inventory (creative mode), switching to it if possible.
IDENTIFICATION DIVISION.
PROGRAM-ID. Inventory-PickItem.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Inventory
    *> slot 0: crafting output
    *> slots 1-4: crafting input
    *> slots 5-8: armor
    *> slots 9-35: main inventory
    *> slots 36-44: hotbar
    *> slot 45: offhand
    78 HOTBAR-START                 VALUE 37.
    78 HOTBAR-END                   VALUE 45.
    78 MAIN-START                   VALUE 10.
    78 MAIN-END                     VALUE 36.
    *> temporary data
    01 SLOT                         BINARY-CHAR UNSIGNED.
    01 EMPTY-SLOT                   BINARY-CHAR UNSIGNED.
    01 TEMP-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==TEMP==.
LINKAGE SECTION.
    01 LK-HELD-SLOT                 BINARY-CHAR UNSIGNED.
    01 LK-INVENTORY.
        02 LK-INVENTORY-SLOT OCCURS 46 TIMES.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK-INVENTORY==.
    01 LK-ITEM-ID                   BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-HELD-SLOT LK-INVENTORY LK-ITEM-ID.
    *> TODO implement this for survival mode

    *> If the item is in the hotbar, switch to it.
    PERFORM VARYING SLOT FROM HOTBAR-START BY 1 UNTIL SLOT > HOTBAR-END
        IF LK-INVENTORY-SLOT-ID(SLOT) = LK-ITEM-ID
            COMPUTE LK-HELD-SLOT = SLOT - HOTBAR-START
            GOBACK
        END-IF
    END-PERFORM

    PERFORM FindEmptySlot

    *> Select an empty hotbar slot if possible
    IF EMPTY-SLOT >= HOTBAR-START AND EMPTY-SLOT <= HOTBAR-END
        COMPUTE LK-HELD-SLOT = EMPTY-SLOT - HOTBAR-START
    END-IF

    *> Try to swap the item from the main inventory into the held slot
    PERFORM VARYING SLOT FROM MAIN-START BY 1 UNTIL SLOT > MAIN-END
        IF LK-INVENTORY-SLOT-ID(SLOT) = LK-ITEM-ID
            MOVE LK-INVENTORY-SLOT(SLOT) TO TEMP-SLOT
            MOVE LK-INVENTORY-SLOT(LK-HELD-SLOT + HOTBAR-START) TO LK-INVENTORY-SLOT(SLOT)
            MOVE TEMP-SLOT TO LK-INVENTORY-SLOT(LK-HELD-SLOT + HOTBAR-START)
            GOBACK
        END-IF
    END-PERFORM

    *> Try to move the item from the held slot into an empty slot
    IF EMPTY-SLOT >= MAIN-START AND EMPTY-SLOT <= MAIN-END
        MOVE LK-INVENTORY-SLOT(LK-HELD-SLOT + HOTBAR-START) TO LK-INVENTORY-SLOT(EMPTY-SLOT)
    END-IF

    *> Place the item in the held slot
    MOVE LK-ITEM-ID TO LK-INVENTORY-SLOT-ID(LK-HELD-SLOT + HOTBAR-START)
    MOVE 1 TO LK-INVENTORY-SLOT-COUNT(LK-HELD-SLOT + HOTBAR-START)
    *> TODO: Properly declare structured components
    *> 0x00 0x00 = no components to add, no components to remove
    MOVE 2 TO LK-INVENTORY-SLOT-NBT-LENGTH(LK-HELD-SLOT + HOTBAR-START)
    MOVE X"0000" TO LK-INVENTORY-SLOT-NBT-DATA(LK-HELD-SLOT + HOTBAR-START)(1:2)

    GOBACK.

FindEmptySlot.
    *> In order of preference: selected, hotbar, inventory
    IF LK-INVENTORY-SLOT-ID(LK-HELD-SLOT + HOTBAR-START) = 0
        COMPUTE EMPTY-SLOT = LK-HELD-SLOT + HOTBAR-START
        EXIT PARAGRAPH
    END-IF
    PERFORM VARYING EMPTY-SLOT FROM HOTBAR-START BY 1 UNTIL EMPTY-SLOT > HOTBAR-END
        IF LK-INVENTORY-SLOT-ID(EMPTY-SLOT) = 0
            EXIT PARAGRAPH
        END-IF
    END-PERFORM
    PERFORM VARYING EMPTY-SLOT FROM MAIN-START BY 1 UNTIL EMPTY-SLOT > MAIN-END
        IF LK-INVENTORY-SLOT-ID(EMPTY-SLOT) = 0
            EXIT PARAGRAPH
        END-IF
    END-PERFORM
    MOVE 0 TO EMPTY-SLOT
    EXIT PARAGRAPH.

END PROGRAM Inventory-PickItem.

*> --- Inventory-StoreItem ---
*> Store as much of the item stack as possible in the player's inventory. The remainder is returned in-place.
IDENTIFICATION DIVISION.
PROGRAM-ID. Inventory-StoreItem.

DATA DIVISION.
WORKING-STORAGE SECTION.
    78 HOTBAR-START                 VALUE 37.
    78 HOTBAR-END                   VALUE 45.
    78 MAIN-START                   VALUE 10.
    78 MAIN-END                     VALUE 36.
    01 SLOT                         BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-INVENTORY.
        02 LK-INVENTORY-SLOT OCCURS 46 TIMES.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK-INVENTORY==.
    01 LK-ITEM.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK-ITEM==.

PROCEDURE DIVISION USING LK-INVENTORY LK-ITEM.
    PERFORM VARYING SLOT FROM HOTBAR-START BY 1 UNTIL SLOT > HOTBAR-END OR LK-ITEM-SLOT-COUNT <= 0
        PERFORM StoreInSlot
    END-PERFORM
    PERFORM VARYING SLOT FROM MAIN-START BY 1 UNTIL SLOT > MAIN-END OR LK-ITEM-SLOT-COUNT <= 0
        PERFORM StoreInSlot
    END-PERFORM
    GOBACK.

StoreInSlot.
    *> TODO handle compatible items (requires parsing data components, and knowledge of max stack sizes)
    IF LK-INVENTORY-SLOT-COUNT(SLOT) = 0
        MOVE LK-ITEM TO LK-INVENTORY-SLOT(SLOT)
        MOVE 0 TO LK-ITEM-SLOT-COUNT
    END-IF
    EXIT PARAGRAPH.

END PROGRAM Inventory-StoreItem.

*> --- Inventory-UpdateCraftingOutput ---
*> Update the crafting output slot based on the current crafting grid.
IDENTIFICATION DIVISION.
PROGRAM-ID. Inventory-UpdateCraftingOutput.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-RECIPES.
    01 SLOT                     BINARY-LONG UNSIGNED.
    01 CRAFTING-INGREDIENTS.
        02 CRAFTING-INGREDIENT OCCURS 9 TIMES BINARY-LONG UNSIGNED.
    01 RECIPE-INDEX             BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INVENTORY-LENGTH      BINARY-LONG UNSIGNED.
    01 LK-INVENTORY.
        02 LK-SLOT OCCURS 1 TO 54 TIMES DEPENDING ON LK-INVENTORY-LENGTH.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
    01 LK-INPUT-COUNT           BINARY-LONG UNSIGNED.
    01 LK-INPUT-START           BINARY-LONG UNSIGNED.
    01 LK-OUTPUT-SLOT           BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-INVENTORY-LENGTH LK-INVENTORY LK-INPUT-COUNT LK-INPUT-START LK-OUTPUT-SLOT.
    INITIALIZE CRAFTING-INGREDIENTS
    PERFORM VARYING SLOT FROM 1 BY 1 UNTIL SLOT > LK-INPUT-COUNT
        IF LK-SLOT-COUNT(LK-INPUT-START + SLOT) > 0
            MOVE LK-SLOT-ID(LK-INPUT-START + SLOT) TO CRAFTING-INGREDIENT(SLOT)
        END-IF
    END-PERFORM

    SORT CRAFTING-INGREDIENT ON DESCENDING KEY CRAFTING-INGREDIENT

    PERFORM VARYING RECIPE-INDEX FROM 1 BY 1 UNTIL RECIPE-INDEX > RECIPES-SHAPELESS-COUNT
        IF RECIPE-SHAPELESS-INPUTS(RECIPE-INDEX) = CRAFTING-INGREDIENTS
            MOVE RECIPE-SHAPELESS-OUTPUT-ID(RECIPE-INDEX) TO LK-SLOT-ID(LK-OUTPUT-SLOT + 1)
            MOVE RECIPE-SHAPELESS-OUTPUT-COUNT(RECIPE-INDEX) TO LK-SLOT-COUNT(LK-OUTPUT-SLOT + 1)
            *> TODO data components?!
            MOVE 2 TO LK-SLOT-NBT-LENGTH(LK-OUTPUT-SLOT + 1)
            MOVE X"0000" TO LK-SLOT-NBT-DATA(LK-OUTPUT-SLOT + 1)(1:2)
            GOBACK
        END-IF
    END-PERFORM

    *> No matching recipe found - clear the output slot
    MOVE 0 TO LK-SLOT-COUNT(LK-OUTPUT-SLOT + 1)

    GOBACK.

END PROGRAM Inventory-UpdateCraftingOutput.
