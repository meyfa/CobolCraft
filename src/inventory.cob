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

FindEmptySlot SECTION.
    *> In order of preference: selected, hotbar, inventory
    IF LK-INVENTORY-SLOT-ID(LK-HELD-SLOT + HOTBAR-START) = 0
        COMPUTE EMPTY-SLOT = LK-HELD-SLOT + HOTBAR-START
        EXIT SECTION
    END-IF
    PERFORM VARYING EMPTY-SLOT FROM HOTBAR-START BY 1 UNTIL EMPTY-SLOT > HOTBAR-END
        IF LK-INVENTORY-SLOT-ID(EMPTY-SLOT) = 0
            EXIT SECTION
        END-IF
    END-PERFORM
    PERFORM VARYING EMPTY-SLOT FROM MAIN-START BY 1 UNTIL EMPTY-SLOT > MAIN-END
        IF LK-INVENTORY-SLOT-ID(EMPTY-SLOT) = 0
            EXIT SECTION
        END-IF
    END-PERFORM
    MOVE 0 TO EMPTY-SLOT
    EXIT SECTION.

END PROGRAM Inventory-PickItem.
