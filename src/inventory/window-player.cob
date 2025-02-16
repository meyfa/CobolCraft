*> --- RegisterWindow-Player ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterWindow-Player.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> the player inventory window is not one of the registered window types
    01 WINDOW-TYPE                  BINARY-LONG                 VALUE -1.
    01 SYNC-PTR                     PROGRAM-POINTER.
    01 CLOSE-PTR                    PROGRAM-POINTER.
    01 SET-SLOT-PTR                 PROGRAM-POINTER.
    01 DROP-PTR                     PROGRAM-POINTER.

PROCEDURE DIVISION.

    SET SYNC-PTR TO ENTRY "Callback-Sync"
    SET CLOSE-PTR TO ENTRY "Callback-Close"
    SET SET-SLOT-PTR TO ENTRY "Callback-SetSlot"
    SET DROP-PTR TO ENTRY "Callback-Drop"

    CALL "SetCallback-WindowSync" USING WINDOW-TYPE SYNC-PTR
    CALL "SetCallback-WindowClose" USING WINDOW-TYPE CLOSE-PTR
    CALL "SetCallback-WindowSetSlot" USING WINDOW-TYPE SET-SLOT-PTR
    CALL "SetCallback-WindowDrop" USING WINDOW-TYPE DROP-PTR

    GOBACK.

    *> --- Callback-Sync ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Sync.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 PLAYER-INVENTORY-LENGTH      BINARY-LONG UNSIGNED        VALUE 46.
        01 PLAYER-INVENTORY-WINDOW-ID   BINARY-LONG UNSIGNED        VALUE 0.
        01 EMPTY-SLOT.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==EMPTY==.
    LINKAGE SECTION.
        COPY DD-CALLBACK-WINDOW-SYNC.

    PROCEDURE DIVISION USING LK-PLAYER.
        *> In contrast to other window types, the player inventory could be synced even with another window open.

        ADD 1 TO PLAYER-INVENTORY-STATE(LK-PLAYER)
        IF PLAYER-WINDOW-ID(LK-PLAYER) = PLAYER-INVENTORY-WINDOW-ID
            CALL "SendPacket-SetContainerContent" USING PLAYER-CLIENT(LK-PLAYER) PLAYER-INVENTORY-WINDOW-ID
                PLAYER-INVENTORY-STATE(LK-PLAYER) PLAYER-INVENTORY-LENGTH PLAYER-INVENTORY(LK-PLAYER)
                PLAYER-MOUSE-ITEM(LK-PLAYER)
        ELSE
            *> if the player has a different window open, do not send the mouse item here
            CALL "SendPacket-SetContainerContent" USING PLAYER-CLIENT(LK-PLAYER) PLAYER-INVENTORY-WINDOW-ID
                PLAYER-INVENTORY-STATE(LK-PLAYER) PLAYER-INVENTORY-LENGTH PLAYER-INVENTORY(LK-PLAYER)
                EMPTY-SLOT
        END-IF

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
            CALL "World-DropItem-FromPlayer" USING PLAYER-MOUSE-ITEM(LK-PLAYER) LK-PLAYER
            MOVE 0 TO PLAYER-MOUSE-SLOT-COUNT(LK-PLAYER)
        END-IF

        *> stash crafting input
        PERFORM VARYING SLOT FROM 1 BY 1 UNTIL SLOT > 4
            CALL "Inventory-StoreItem" USING PLAYER-INVENTORY(LK-PLAYER) PLAYER-INVENTORY-SLOT(LK-PLAYER, SLOT + 1)
            IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, SLOT + 1) > 0
                CALL "World-DropItem-FromPlayer" USING PLAYER-INVENTORY-SLOT(LK-PLAYER, SLOT + 1) LK-PLAYER
                MOVE 0 TO PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, SLOT + 1)
            END-IF
        END-PERFORM

        *> clear crafting output
        MOVE 0 TO PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, 1)

        CALL "Inventory-SyncPlayerInventory" USING LK-PLAYER

        GOBACK.

    END PROGRAM Callback-Close.

    *> --- Callback-SetSlot ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-SetSlot.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 PLAYER-INVENTORY-LENGTH  BINARY-LONG UNSIGNED        VALUE 46.
        01 CRAFTING-GRID-SIZE       BINARY-LONG UNSIGNED        VALUE 4.
        01 CRAFTING-GRID-START      BINARY-LONG UNSIGNED        VALUE 1.
        01 CRAFTING-OUTPUT-SLOT     BINARY-LONG UNSIGNED        VALUE 0.
    LINKAGE SECTION.
        COPY DD-CALLBACK-WINDOW-SET-SLOT.

    PROCEDURE DIVISION USING LK-PLAYER LK-INDEX LK-SLOT LK-SYNC-REQUIRED.
        MOVE 0 TO LK-SYNC-REQUIRED

        EVALUATE LK-INDEX
            WHEN 0 *> crafting output
                 MOVE LK-SLOT TO PLAYER-INVENTORY-SLOT(LK-PLAYER, LK-INDEX + 1)

            WHEN 1 THRU 4 *> crafting input
                 MOVE LK-SLOT TO PLAYER-INVENTORY-SLOT(LK-PLAYER, LK-INDEX + 1)
                CALL "Inventory-UpdateCraftingOutput" USING PLAYER-INVENTORY-LENGTH PLAYER-INVENTORY(LK-PLAYER)
                    CRAFTING-GRID-SIZE CRAFTING-GRID-START CRAFTING-OUTPUT-SLOT
                MOVE 1 TO LK-SYNC-REQUIRED

            WHEN 5 THRU 45 *> armor, inventory, offhand
                MOVE LK-SLOT TO PLAYER-INVENTORY-SLOT(LK-PLAYER, LK-INDEX + 1)

            WHEN OTHER
                DISPLAY "Invalid slot number: " LK-INDEX
        END-EVALUATE

        GOBACK.

    END PROGRAM Callback-SetSlot.

    *> --- Callback-Drop ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Drop.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 DROP-SLOT.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==DROP==.
        01 SLOT-INDEX               BINARY-LONG UNSIGNED.
        *> TODO deduplicate with Callback-SetSlot
        01 WINDOW-INVENTORY-LENGTH  BINARY-LONG UNSIGNED        VALUE 46.
        01 CRAFTING-GRID-SIZE       BINARY-LONG UNSIGNED        VALUE 4.
        01 CRAFTING-GRID-START      BINARY-LONG UNSIGNED        VALUE 1.
        01 CRAFTING-OUTPUT-SLOT     BINARY-LONG UNSIGNED        VALUE 0.
    LINKAGE SECTION.
        COPY DD-CALLBACK-WINDOW-DROP.

    PROCEDURE DIVISION USING LK-PLAYER LK-INDEX LK-STACK LK-SYNC-REQUIRED.
        EVALUATE LK-INDEX
            WHEN 0 *> crafting output
                *> TODO support dropping entire crafting output (LK-STACK = 1)
                MOVE PLAYER-INVENTORY-SLOT(LK-PLAYER, LK-INDEX + 1) TO DROP-SLOT
                IF DROP-SLOT-COUNT > 0
                    *> crafting output becomes empty, input is reduced by 1 each
                    MOVE 0 TO PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, LK-INDEX + 1)
                    PERFORM VARYING SLOT-INDEX FROM 1 BY 1 UNTIL SLOT-INDEX > 4
                        IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, SLOT-INDEX + 1) > 0
                            SUBTRACT 1 FROM PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, SLOT-INDEX + 1)
                        END-IF
                    END-PERFORM
                    CALL "World-DropItem-FromPlayer" USING DROP-SLOT LK-PLAYER
                    PERFORM UpdateCrafting
                END-IF

            WHEN 1 THRU 4 *> crafting input
                MOVE PLAYER-INVENTORY-SLOT(LK-PLAYER, LK-INDEX + 1) TO DROP-SLOT
                IF DROP-SLOT-COUNT > 0
                    IF LK-STACK = 0
                        MOVE 1 TO DROP-SLOT-COUNT
                    END-IF
                    SUBTRACT DROP-SLOT-COUNT FROM PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, LK-INDEX + 1)
                    CALL "World-DropItem-FromPlayer" USING DROP-SLOT LK-PLAYER
                    PERFORM UpdateCrafting
                END-IF

            WHEN 5 THRU 45 *> armor, inventory, offhand
                MOVE PLAYER-INVENTORY-SLOT(LK-PLAYER, LK-INDEX + 1) TO DROP-SLOT
                IF DROP-SLOT-COUNT > 0
                    IF LK-STACK = 0
                        MOVE 1 TO DROP-SLOT-COUNT
                    END-IF
                    SUBTRACT DROP-SLOT-COUNT FROM PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, LK-INDEX + 1)
                    CALL "World-DropItem-FromPlayer" USING DROP-SLOT LK-PLAYER
                END-IF

            WHEN OTHER
                DISPLAY "Invalid slot number: " LK-INDEX
        END-EVALUATE

        GOBACK.

    UpdateCrafting.
        CALL "Inventory-UpdateCraftingOutput" USING WINDOW-INVENTORY-LENGTH PLAYER-WINDOW-SLOTS(LK-PLAYER)
            CRAFTING-GRID-SIZE CRAFTING-GRID-START CRAFTING-OUTPUT-SLOT
        *> TODO only set this when the output slot is actually changed
        MOVE 1 TO LK-SYNC-REQUIRED
        .

    END PROGRAM Callback-Drop.

END PROGRAM RegisterWindow-Player.
