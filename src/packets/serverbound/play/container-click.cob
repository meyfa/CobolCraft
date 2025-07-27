IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-ContainerClick.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PLAYER-OFFHAND-SLOT-ID   BINARY-LONG             VALUE 45.
    01 PLAYER-HOTBAR-SLOT-ID    BINARY-LONG             VALUE 36.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    01 PLAYER-ID                BINARY-LONG.
    01 WINDOW-ID                BINARY-LONG.
    01 STATE-ID                 BINARY-LONG.
    01 SYNC-PTR                 PROGRAM-POINTER.
    01 GET-SLOT-PTR             PROGRAM-POINTER.
    01 SET-SLOT-PTR             PROGRAM-POINTER.
    01 DROP-PTR                 PROGRAM-POINTER.
    01 SHIFT-PTR                PROGRAM-POINTER.
    01 SERVER-CHANGED-SLOTS     BINARY-LONG UNSIGNED.
    01 TEMP-CHANGED-SLOTS       BINARY-LONG UNSIGNED.
    01 SYNC-REQUIRED            BINARY-CHAR UNSIGNED.
    01 SLOT                     BINARY-SHORT.
    01 BUTTON                   BINARY-CHAR.
    01 MODE-ENUM                BINARY-LONG.
    01 CLIENT-CHANGED-SLOTS     BINARY-LONG.
    01 SLOT-NUMBER              BINARY-SHORT.
    01 DECODE-SLOT-COUNT        BINARY-CHAR.
    01 CLIENT-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==CLIENT==.
    01 SERVER-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==SERVER==.
    01 ITEMS-EQUAL              BINARY-CHAR UNSIGNED.
    01 ITEMS-COMPATIBLE         BINARY-CHAR UNSIGNED.
    01 MAX-STACK-SIZE           BINARY-LONG.
    01 TEMP-COUNT               BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET WINDOW-ID
    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET STATE-ID
    CALL "Decode-Short" USING LK-BUFFER LK-OFFSET SLOT
    CALL "Decode-Byte" USING LK-BUFFER LK-OFFSET BUTTON
    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET MODE-ENUM

    *> ignore clicks in windows other than the current one
    IF WINDOW-ID NOT = PLAYER-WINDOW-ID(PLAYER-ID)
        PERFORM AbortEarly
    END-IF

    CALL "GetCallback-WindowSync" USING PLAYER-WINDOW-TYPE(PLAYER-ID) SYNC-PTR
    CALL "GetCallback-WindowGetSlot" USING PLAYER-WINDOW-TYPE(PLAYER-ID) GET-SLOT-PTR
    CALL "GetCallback-WindowSetSlot" USING PLAYER-WINDOW-TYPE(PLAYER-ID) SET-SLOT-PTR
    CALL "GetCallback-WindowDrop" USING PLAYER-WINDOW-TYPE(PLAYER-ID) DROP-PTR
    CALL "GetCallback-WindowShift" USING PLAYER-WINDOW-TYPE(PLAYER-ID) SHIFT-PTR
    IF SYNC-PTR = NULL OR GET-SLOT-PTR = NULL OR SET-SLOT-PTR = NULL OR DROP-PTR = NULL OR SHIFT-PTR = NULL
        DISPLAY "RecvPacket-ContainerClick: Unable to handle window type " PLAYER-WINDOW-TYPE(PLAYER-ID)
        PERFORM AbortEarly
    END-IF

    *> sync client if state ID differs from last sent
    IF (WINDOW-ID = 0 AND STATE-ID NOT = PLAYER-INVENTORY-STATE(PLAYER-ID)) OR (WINDOW-ID > 0 AND STATE-ID NOT = PLAYER-WINDOW-STATE(PLAYER-ID))
        CALL SYNC-PTR USING PLAYER-ID
        PERFORM AbortEarly
    END-IF

    MOVE 0 TO SERVER-CHANGED-SLOTS

    EVALUATE MODE-ENUM
        WHEN 0
            PERFORM Mode0
        WHEN 1
            PERFORM Mode1
        WHEN 2
            PERFORM Mode2
        WHEN 3
            PERFORM Mode3
        WHEN 4
            PERFORM Mode4
        WHEN 5
            PERFORM Mode5
        WHEN 6
            PERFORM Mode6
        WHEN OTHER
            DISPLAY "RecvPacket-ContainerClick: invalid mode " MODE-ENUM
            PERFORM AbortEarly
    END-EVALUATE

    PERFORM CompareChanges

    IF SYNC-REQUIRED NOT = 0
        CALL SYNC-PTR USING PLAYER-ID
    END-IF

    GOBACK.

Mode0.
    EVALUATE TRUE
        *> left click
        WHEN BUTTON = 0 AND SLOT >= 0
            CALL GET-SLOT-PTR USING PLAYER-ID SLOT CLIENT-SLOT
            CALL "Inventory-CompareItems" USING CLIENT-SLOT PLAYER-MOUSE-ITEM(PLAYER-ID) ITEMS-COMPATIBLE

            EVALUATE TRUE
                *> When (at least) one slot is empty, or the items are incompatible, swap the contents.
                WHEN PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID) = 0 OR CLIENT-SLOT-COUNT = 0 OR ITEMS-COMPATIBLE = 0
                    CALL SET-SLOT-PTR USING PLAYER-ID SLOT PLAYER-MOUSE-ITEM(PLAYER-ID) TEMP-CHANGED-SLOTS
                    ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS
                    MOVE CLIENT-SLOT TO PLAYER-MOUSE-ITEM(PLAYER-ID)

                *> When both slots are non-empty but compatible, place as many mouse items as possible into the slot.
                WHEN ITEMS-COMPATIBLE NOT = 0
                    CALL "Items-Get-MaxStackSize" USING CLIENT-SLOT-ID MAX-STACK-SIZE
                    COMPUTE TEMP-COUNT = FUNCTION MIN(MAX-STACK-SIZE - CLIENT-SLOT-COUNT, PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID))
                    IF TEMP-COUNT > 0
                        SUBTRACT TEMP-COUNT FROM PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID)
                        ADD TEMP-COUNT TO CLIENT-SLOT-COUNT
                        CALL SET-SLOT-PTR USING PLAYER-ID SLOT CLIENT-SLOT TEMP-CHANGED-SLOTS
                        ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS
                    END-IF
            END-EVALUATE

        *> right click
        WHEN BUTTON = 1 AND SLOT >= 0
            CALL GET-SLOT-PTR USING PLAYER-ID SLOT CLIENT-SLOT
            CALL "Inventory-CompareItems" USING CLIENT-SLOT PLAYER-MOUSE-ITEM(PLAYER-ID) ITEMS-COMPATIBLE

            EVALUATE TRUE
                *> When the mouse slot is empty, pick up half of the clicked slot (rounded up).
                WHEN PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID) = 0
                    DIVIDE CLIENT-SLOT-COUNT BY 2 GIVING TEMP-COUNT ROUNDED MODE IS TOWARD-GREATER
                    IF TEMP-COUNT > 0
                        MOVE CLIENT-SLOT TO PLAYER-MOUSE-ITEM(PLAYER-ID)
                        MOVE TEMP-COUNT TO PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID)
                        SUBTRACT TEMP-COUNT FROM CLIENT-SLOT-COUNT
                        CALL SET-SLOT-PTR USING PLAYER-ID SLOT CLIENT-SLOT TEMP-CHANGED-SLOTS
                        ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS
                    END-IF

                *> Otherwise, if the items are compatible, or the clicked slot is empty, add a single mouse item into the slot.
                WHEN ITEMS-COMPATIBLE NOT = 0 OR CLIENT-SLOT-COUNT = 0
                    CALL "Items-Get-MaxStackSize" USING PLAYER-MOUSE-SLOT-ID(PLAYER-ID) MAX-STACK-SIZE
                    IF CLIENT-SLOT-COUNT < MAX-STACK-SIZE
                        COMPUTE TEMP-COUNT = CLIENT-SLOT-COUNT + 1
                        SUBTRACT 1 FROM PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID)
                        MOVE PLAYER-MOUSE-ITEM(PLAYER-ID) TO CLIENT-SLOT
                        MOVE TEMP-COUNT TO CLIENT-SLOT-COUNT
                        CALL SET-SLOT-PTR USING PLAYER-ID SLOT CLIENT-SLOT TEMP-CHANGED-SLOTS
                        ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS
                    END-IF

                *> If the items are not compatible, swap the contents.
                WHEN ITEMS-COMPATIBLE = 0
                    CALL SET-SLOT-PTR USING PLAYER-ID SLOT PLAYER-MOUSE-ITEM(PLAYER-ID) TEMP-CHANGED-SLOTS
                    ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS
                    MOVE CLIENT-SLOT TO PLAYER-MOUSE-ITEM(PLAYER-ID)
            END-EVALUATE

        *> drop stack outside inventory
        WHEN BUTTON = 0 AND SLOT = -999
            IF PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID) > 0
                MOVE PLAYER-MOUSE-ITEM(PLAYER-ID) TO CLIENT-SLOT
                SUBTRACT CLIENT-SLOT-COUNT FROM PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID)
                CALL "World-DropItem-FromPlayer" USING CLIENT-SLOT PLAYER-ID
                ADD 1 TO SERVER-CHANGED-SLOTS
            END-IF

        *> drop single item outside inventory
        WHEN BUTTON = 1 AND SLOT = -999
            IF PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID) > 0
                MOVE PLAYER-MOUSE-ITEM(PLAYER-ID) TO CLIENT-SLOT
                MOVE 1 TO CLIENT-SLOT-COUNT
                SUBTRACT CLIENT-SLOT-COUNT FROM PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID)
                CALL "World-DropItem-FromPlayer" USING CLIENT-SLOT PLAYER-ID
                ADD 1 TO SERVER-CHANGED-SLOTS
            END-IF

        *> click on GUI, but not on a slot
        WHEN (BUTTON = 0 OR 1) AND SLOT = -1
            *> nothing to do
            CONTINUE

        WHEN OTHER
            PERFORM InvalidButtonSlot
    END-EVALUATE
    .

Mode1.
    EVALUATE TRUE
        WHEN (BUTTON = 0 OR 1) AND SLOT >= 0
            CALL SHIFT-PTR USING PLAYER-ID SLOT TEMP-CHANGED-SLOTS
            ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS

        WHEN OTHER
            PERFORM InvalidButtonSlot
    END-EVALUATE
    .

Mode2.
    EVALUATE TRUE
        WHEN BUTTON >= 0 AND BUTTON <= 8 AND SLOT >= 0
            *> TODO handle number keys in containers
            IF WINDOW-ID > 0
                CALL "Inventory-SyncPlayerInventory" USING PLAYER-ID
                PERFORM AbortEarly
            END-IF

            COMPUTE SLOT-NUMBER = BUTTON + PLAYER-HOTBAR-SLOT-ID
            CALL GET-SLOT-PTR USING PLAYER-ID SLOT CLIENT-SLOT
            CALL GET-SLOT-PTR USING PLAYER-ID SLOT-NUMBER SERVER-SLOT

            CALL SET-SLOT-PTR USING PLAYER-ID SLOT SERVER-SLOT TEMP-CHANGED-SLOTS
            ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS

            CALL SET-SLOT-PTR USING PLAYER-ID SLOT-NUMBER CLIENT-SLOT TEMP-CHANGED-SLOTS
            ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS

        WHEN BUTTON = 40 AND SLOT >= 0
            *> TODO handle offhand swap key (F) in containers
            If WINDOW-ID > 0
                CALL "Inventory-SyncPlayerInventory" USING PLAYER-ID
                PERFORM AbortEarly
            END-IF

            *> TODO Prevent swapping with illegal slots (armor slots, crafting)

            CALL GET-SLOT-PTR USING PLAYER-ID SLOT CLIENT-SLOT
            CALL GET-SLOT-PTR USING PLAYER-ID PLAYER-OFFHAND-SLOT-ID SERVER-SLOT

            CALL SET-SLOT-PTR USING PLAYER-ID SLOT SERVER-SLOT TEMP-CHANGED-SLOTS
            ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS

            CALL SET-SLOT-PTR USING PLAYER-ID PLAYER-OFFHAND-SLOT-ID CLIENT-SLOT TEMP-CHANGED-SLOTS
            ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS

        WHEN OTHER
            PERFORM InvalidButtonSlot
    END-EVALUATE
    .

Mode3.
    EVALUATE TRUE
        *> middle click
        WHEN BUTTON = 2 AND SLOT >= 0
            *> only valid in creative mode in non-player inventories
            IF PLAYER-GAMEMODE(PLAYER-ID) NOT = 1 OR PLAYER-WINDOW-TYPE(PLAYER-ID) = 0
                PERFORM AbortEarly
            END-IF

            CALL GET-SLOT-PTR USING PLAYER-ID SLOT CLIENT-SLOT
            IF PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID) = 0 AND CLIENT-SLOT-COUNT > 0
                CALL "Items-Get-MaxStackSize" USING CLIENT-SLOT-ID MAX-STACK-SIZE
                MOVE CLIENT-SLOT TO PLAYER-MOUSE-ITEM(PLAYER-ID)
                MOVE MAX-STACK-SIZE TO PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID)
                ADD 1 TO SERVER-CHANGED-SLOTS
            END-IF

        WHEN OTHER
            PERFORM InvalidButtonSlot
    END-EVALUATE
    .

Mode4.
    EVALUATE TRUE
        *> drop key (Q), CTRL + drop key (Q)
        WHEN (BUTTON = 0 OR 1) AND SLOT >= 0
            CALL DROP-PTR USING PLAYER-ID SLOT BUTTON TEMP-CHANGED-SLOTS
            ADD TEMP-CHANGED-SLOTS TO SERVER-CHANGED-SLOTS

        *> click outside inventory, but with empty mouse item
        WHEN (BUTTON = 0 OR 1) AND SLOT = -999
            *> nothing to do
            CONTINUE

        WHEN OTHER
            PERFORM InvalidButtonSlot
    END-EVALUATE
    .

Mode5.
    *> TODO implement painting
    EVALUATE TRUE
        WHEN (BUTTON = 0 OR 4 OR 8) AND SLOT = -999
            *> TODO start left, right, middle
            CONTINUE

        WHEN (BUTTON = 1 OR 5 OR 9) AND SLOT >= 0
            *> TODO add slot to left, right, middle
            CONTINUE

        WHEN (BUTTON = 2 OR 6 OR 10) AND SLOT = -999
            *> TODO end left, right, middle
            CONTINUE

        WHEN OTHER
            PERFORM InvalidButtonSlot
    END-EVALUATE
    .

Mode6.
    EVALUATE TRUE
        WHEN BUTTON = 0 AND SLOT >= 0
            *> TODO double click
            CONTINUE

        WHEN OTHER
            PERFORM InvalidButtonSlot
    END-EVALUATE
    .

CompareChanges.
    *> Read the client's predicted slot changes and compare them to what the server has computed.

    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET CLIENT-CHANGED-SLOTS
    IF CLIENT-CHANGED-SLOTS < 0 OR CLIENT-CHANGED-SLOTS > 128
        GOBACK
    END-IF

    IF CLIENT-CHANGED-SLOTS NOT = SERVER-CHANGED-SLOTS
        MOVE 1 TO SYNC-REQUIRED
    END-IF

    PERFORM CLIENT-CHANGED-SLOTS TIMES
        CALL "Decode-Short" USING LK-BUFFER LK-OFFSET SLOT-NUMBER
        CALL "Decode-InventorySlot" USING LK-BUFFER LK-OFFSET CLIENT-SLOT

        CALL GET-SLOT-PTR USING PLAYER-ID SLOT-NUMBER SERVER-SLOT
        CALL "Inventory-ItemsEqual" USING CLIENT-SLOT SERVER-SLOT ITEMS-EQUAL
        IF ITEMS-EQUAL = 0
            MOVE 1 TO SYNC-REQUIRED
        END-IF
    END-PERFORM

    *> carried item
    CALL "Decode-InventorySlot" USING LK-BUFFER LK-OFFSET CLIENT-SLOT
    CALL "Inventory-ItemsEqual" USING CLIENT-SLOT PLAYER-MOUSE-ITEM(PLAYER-ID) ITEMS-EQUAL
    IF ITEMS-EQUAL = 0
        MOVE 1 TO SYNC-REQUIRED
    END-IF
    .

InvalidButtonSlot.
    DISPLAY "RecvPacket-ContainerClick: mode " MODE-ENUM ": invalid button/slot: " BUTTON "/" SLOT
    PERFORM AbortEarly
    .

AbortEarly.
    *> Call when reading the packet is aborted early to still consume all slot data,
    *> which avoids a warning in the log about unread data.

    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET CLIENT-CHANGED-SLOTS
    IF CLIENT-CHANGED-SLOTS < 0 OR CLIENT-CHANGED-SLOTS > 128
        EXIT PARAGRAPH
    END-IF

    PERFORM CLIENT-CHANGED-SLOTS TIMES
        CALL "Decode-Short" USING LK-BUFFER LK-OFFSET SLOT-NUMBER
        CALL "Decode-InventorySlot" USING LK-BUFFER LK-OFFSET CLIENT-SLOT
    END-PERFORM

    CALL "Decode-InventorySlot" USING LK-BUFFER LK-OFFSET CLIENT-SLOT

    GOBACK.

END PROGRAM RecvPacket-ContainerClick.
