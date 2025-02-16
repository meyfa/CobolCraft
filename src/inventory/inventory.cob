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
*> Pick an item into the player's hotbar. If the item already exists in the inventory, the hotbar will be switched to
*> that item. In creative mode, the item is added to the inventory if needed.
IDENTIFICATION DIVISION.
PROGRAM-ID. Inventory-PickItem.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
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
    01 LK-PLAYER                    BINARY-LONG UNSIGNED.
    01 LK-ITEM-ID                   BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-ID.
    *> If the item is in the hotbar, switch to it.
    PERFORM VARYING SLOT FROM HOTBAR-START BY 1 UNTIL SLOT > HOTBAR-END
        IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, SLOT) > 0 AND PLAYER-INVENTORY-SLOT-ID(LK-PLAYER, SLOT) = LK-ITEM-ID
            COMPUTE PLAYER-HOTBAR(LK-PLAYER) = SLOT - HOTBAR-START
            PERFORM SendChanges
            GOBACK
        END-IF
    END-PERFORM

    PERFORM FindEmptySlot

    *> Select an empty hotbar slot if possible
    IF EMPTY-SLOT >= HOTBAR-START AND EMPTY-SLOT <= HOTBAR-END
        COMPUTE PLAYER-HOTBAR(LK-PLAYER) = EMPTY-SLOT - HOTBAR-START
    END-IF

    *> Try to swap the item from the main inventory into the held slot
    PERFORM VARYING SLOT FROM MAIN-START BY 1 UNTIL SLOT > MAIN-END
        IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, SLOT) > 0 AND PLAYER-INVENTORY-SLOT-ID(LK-PLAYER, SLOT) = LK-ITEM-ID
            MOVE PLAYER-INVENTORY-SLOT(LK-PLAYER, SLOT) TO TEMP-SLOT
            MOVE PLAYER-INVENTORY-SLOT(LK-PLAYER, PLAYER-HOTBAR(LK-PLAYER) + HOTBAR-START) TO PLAYER-INVENTORY-SLOT(LK-PLAYER, SLOT)
            MOVE TEMP-SLOT TO PLAYER-INVENTORY-SLOT(LK-PLAYER, PLAYER-HOTBAR(LK-PLAYER) + HOTBAR-START)
            PERFORM SendChanges
            GOBACK
        END-IF
    END-PERFORM

    IF PLAYER-GAMEMODE(LK-PLAYER) = 1
        *> Try to move the item from the held slot into an empty slot
        IF EMPTY-SLOT >= MAIN-START AND EMPTY-SLOT <= MAIN-END
            MOVE PLAYER-INVENTORY-SLOT(LK-PLAYER, PLAYER-HOTBAR(LK-PLAYER) + HOTBAR-START) TO PLAYER-INVENTORY-SLOT(LK-PLAYER, EMPTY-SLOT)
        END-IF

        *> Place the item in the held slot
        MOVE LK-ITEM-ID TO PLAYER-INVENTORY-SLOT-ID(LK-PLAYER, PLAYER-HOTBAR(LK-PLAYER) + HOTBAR-START)
        MOVE 1 TO PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, PLAYER-HOTBAR(LK-PLAYER) + HOTBAR-START)
        *> TODO: Properly declare structured components
        *> 0x00 0x00 = no components to add, no components to remove
        MOVE 2 TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(LK-PLAYER, PLAYER-HOTBAR(LK-PLAYER) + HOTBAR-START)
        MOVE X"0000" TO PLAYER-INVENTORY-SLOT-NBT-DATA(LK-PLAYER, PLAYER-HOTBAR(LK-PLAYER) + HOTBAR-START)(1:2)

        PERFORM SendChanges
    END-IF

    GOBACK.

FindEmptySlot.
    *> In order of preference: selected, hotbar, inventory
    IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, PLAYER-HOTBAR(LK-PLAYER) + HOTBAR-START) = 0
        COMPUTE EMPTY-SLOT = PLAYER-HOTBAR(LK-PLAYER) + HOTBAR-START
        EXIT PARAGRAPH
    END-IF
    PERFORM VARYING EMPTY-SLOT FROM HOTBAR-START BY 1 UNTIL EMPTY-SLOT > HOTBAR-END
        IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, EMPTY-SLOT) = 0
            EXIT PARAGRAPH
        END-IF
    END-PERFORM
    PERFORM VARYING EMPTY-SLOT FROM MAIN-START BY 1 UNTIL EMPTY-SLOT > MAIN-END
        IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, EMPTY-SLOT) = 0
            EXIT PARAGRAPH
        END-IF
    END-PERFORM
    MOVE 0 TO EMPTY-SLOT
    .

SendChanges.
    CALL "SendPacket-SetHeldItem" USING PLAYER-CLIENT(LK-PLAYER) PLAYER-HOTBAR(LK-PLAYER)
    *> TODO send only changed slots
    CALL "Inventory-SyncPlayerInventory" USING LK-PLAYER
    .

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
    .

END PROGRAM Inventory-StoreItem.

*> --- Inventory-UpdateCraftingOutput ---
*> Update the crafting output slot based on the current crafting grid.
IDENTIFICATION DIVISION.
PROGRAM-ID. Inventory-UpdateCraftingOutput.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-RECIPES.
    01 SLOT                     BINARY-LONG UNSIGNED.
    01 INITIAL-GRID.
        02 INITIAL-GRID-ID      OCCURS 9 TIMES BINARY-LONG UNSIGNED.
    01 CRAFTING-GRID.
        02 CRAFTING-GRID-ID     OCCURS 9 TIMES BINARY-LONG UNSIGNED.
    01 RECIPE-INDEX             BINARY-LONG UNSIGNED.
    01 CHOICE-INDEX             BINARY-LONG UNSIGNED.
    01 SLOT-MATCHED             BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-INVENTORY-LENGTH      BINARY-LONG UNSIGNED.
    01 LK-INVENTORY.
        02 LK-SLOT OCCURS 1 TO 54 TIMES DEPENDING ON LK-INVENTORY-LENGTH.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
    01 LK-INPUT-COUNT           BINARY-LONG UNSIGNED.
    01 LK-INPUT-START           BINARY-LONG UNSIGNED.
    01 LK-OUTPUT-SLOT           BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-INVENTORY-LENGTH LK-INVENTORY LK-INPUT-COUNT LK-INPUT-START LK-OUTPUT-SLOT.
    INITIALIZE INITIAL-GRID
    IF LK-INPUT-COUNT = 4
        IF LK-SLOT-COUNT(LK-INPUT-START + 1) > 0
            MOVE LK-SLOT-ID(LK-INPUT-START + 1) TO INITIAL-GRID-ID(1)
        END-IF
        IF LK-SLOT-COUNT(LK-INPUT-START + 2) > 0
            MOVE LK-SLOT-ID(LK-INPUT-START + 2) TO INITIAL-GRID-ID(2)
        END-IF
        IF LK-SLOT-COUNT(LK-INPUT-START + 3) > 0
            MOVE LK-SLOT-ID(LK-INPUT-START + 3) TO INITIAL-GRID-ID(4)
        END-IF
        IF LK-SLOT-COUNT(LK-INPUT-START + 4) > 0
            MOVE LK-SLOT-ID(LK-INPUT-START + 4) TO INITIAL-GRID-ID(5)
        END-IF
    ELSE
        PERFORM VARYING SLOT FROM 1 BY 1 UNTIL SLOT > LK-INPUT-COUNT
            IF LK-SLOT-COUNT(LK-INPUT-START + SLOT) > 0
                MOVE LK-SLOT-ID(LK-INPUT-START + SLOT) TO INITIAL-GRID-ID(SLOT)
            END-IF
        END-PERFORM
    END-IF

    MOVE INITIAL-GRID TO CRAFTING-GRID
    PERFORM MatchShapelessRecipes

    MOVE INITIAL-GRID TO CRAFTING-GRID
    PERFORM MatchShapedRecipes

    *> No matching recipe found - clear the output slot
    MOVE 0 TO LK-SLOT-COUNT(LK-OUTPUT-SLOT + 1)

    GOBACK.

MatchShapedRecipes.
    *> Move slots to the left edge
    PERFORM 2 TIMES
        IF CRAFTING-GRID-ID(1) = 0 AND CRAFTING-GRID-ID(4) = 0 AND CRAFTING-GRID-ID(7) = 0
            MOVE CRAFTING-GRID-ID(2) TO CRAFTING-GRID-ID(1)
            MOVE CRAFTING-GRID-ID(3) TO CRAFTING-GRID-ID(2)
            MOVE CRAFTING-GRID-ID(5) TO CRAFTING-GRID-ID(4)
            MOVE CRAFTING-GRID-ID(6) TO CRAFTING-GRID-ID(5)
            MOVE CRAFTING-GRID-ID(8) TO CRAFTING-GRID-ID(7)
            MOVE CRAFTING-GRID-ID(9) TO CRAFTING-GRID-ID(8)
            MOVE 0 TO CRAFTING-GRID-ID(3) CRAFTING-GRID-ID(6) CRAFTING-GRID-ID(9)
        END-IF
    END-PERFORM

    *> Move slots to the top edge
    PERFORM 2 TIMES
        IF CRAFTING-GRID-ID(1) = 0 AND CRAFTING-GRID-ID(2) = 0 AND CRAFTING-GRID-ID(3) = 0
            MOVE CRAFTING-GRID-ID(4) TO CRAFTING-GRID-ID(1)
            MOVE CRAFTING-GRID-ID(5) TO CRAFTING-GRID-ID(2)
            MOVE CRAFTING-GRID-ID(6) TO CRAFTING-GRID-ID(3)
            MOVE CRAFTING-GRID-ID(7) TO CRAFTING-GRID-ID(4)
            MOVE CRAFTING-GRID-ID(8) TO CRAFTING-GRID-ID(5)
            MOVE CRAFTING-GRID-ID(9) TO CRAFTING-GRID-ID(6)
            MOVE 0 TO CRAFTING-GRID-ID(7) CRAFTING-GRID-ID(8) CRAFTING-GRID-ID(9)
        END-IF
    END-PERFORM

    *> match by comparing the normalized ingredient list
    PERFORM VARYING RECIPE-INDEX FROM 1 BY 1 UNTIL RECIPE-INDEX > RECIPES-SHAPED-COUNT
        IF RECIPE-SHAPED-INPUTS(RECIPE-INDEX) = CRAFTING-GRID
            CALL "SetRecipeOutput" USING LK-SLOT(LK-OUTPUT-SLOT + 1) RECIPE-SHAPED-OUTPUT(RECIPE-INDEX)
            GOBACK
        END-IF
    END-PERFORM

    *> check "complex" recipes (shaped recipes with multiple options per slot)
    PERFORM VARYING RECIPE-INDEX FROM 1 BY 1 UNTIL RECIPE-INDEX > RECIPES-COMPLEX-COUNT
        PERFORM VARYING SLOT FROM 1 BY 1 UNTIL SLOT > 9
            MOVE 0 TO SLOT-MATCHED
            PERFORM VARYING CHOICE-INDEX FROM 1 BY 1 UNTIL CHOICE-INDEX > RECIPE-COMPLEX-INPUT-OPTIONS(RECIPE-INDEX, SLOT)
                IF RECIPE-COMPLEX-INPUT-ID(RECIPE-INDEX, SLOT, CHOICE-INDEX) = CRAFTING-GRID-ID(SLOT)
                    MOVE 1 TO SLOT-MATCHED
                    EXIT PERFORM
                END-IF
            END-PERFORM
            IF SLOT-MATCHED = 0
                EXIT PERFORM
            END-IF
        END-PERFORM
        *> If we made it through all slots, the recipe matches
        IF SLOT > 9
            CALL "SetRecipeOutput" USING LK-SLOT(LK-OUTPUT-SLOT + 1) RECIPE-COMPLEX-OUTPUT(RECIPE-INDEX)
            GOBACK
        END-IF
    END-PERFORM
    .

MatchShapelessRecipes.
    SORT CRAFTING-GRID-ID ON DESCENDING KEY CRAFTING-GRID-ID

    *> match by comparing the normalized ingredient list
    PERFORM VARYING RECIPE-INDEX FROM 1 BY 1 UNTIL RECIPE-INDEX > RECIPES-SHAPELESS-COUNT
        IF RECIPE-SHAPELESS-INPUTS(RECIPE-INDEX) = CRAFTING-GRID
            CALL "SetRecipeOutput" USING LK-SLOT(LK-OUTPUT-SLOT + 1) RECIPE-SHAPELESS-OUTPUT(RECIPE-INDEX)
            GOBACK
        END-IF
    END-PERFORM
    .

    *> --- SetRecipeOutput ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. SetRecipeOutput.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-SLOT.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
        01 LK-OUTPUT.
            02 LK-OUTPUT-ID BINARY-LONG UNSIGNED.
            02 LK-OUTPUT-COUNT BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-SLOT LK-OUTPUT.
        MOVE LK-OUTPUT-ID TO LK-SLOT-ID
        MOVE LK-OUTPUT-COUNT TO LK-SLOT-COUNT
        *> TODO data components?!
        MOVE 2 TO LK-SLOT-NBT-LENGTH
        MOVE X"0000" TO LK-SLOT-NBT-DATA(1:2)
        GOBACK.

    END PROGRAM SetRecipeOutput.

END PROGRAM Inventory-UpdateCraftingOutput.
