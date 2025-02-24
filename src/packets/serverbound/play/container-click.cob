IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-ContainerClick.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    01 PLAYER-ID                BINARY-LONG.
    01 WINDOW-ID                BINARY-LONG.
    01 STATE-ID                 BINARY-LONG.
    01 SYNC-PTR                 PROGRAM-POINTER.
    01 SET-SLOT-PTR             PROGRAM-POINTER.
    01 DROP-PTR                 PROGRAM-POINTER.
    01 SLOT-SYNC-REQUIRED       BINARY-CHAR UNSIGNED.
    01 SYNC-REQUIRED            BINARY-CHAR UNSIGNED.
    01 SLOT                     BINARY-SHORT.
    01 BUTTON                   BINARY-CHAR.
    01 MODE-ENUM                BINARY-LONG.
    01 CHANGED-SLOT-COUNT       BINARY-LONG.
    01 SLOT-NUMBER              BINARY-SHORT.
    01 DECODE-SLOT-COUNT        BINARY-CHAR.
    01 CLIENT-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==CLIENT==.
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

    *> TODO We currently accept the client's changed slot data as correct, but we should really compute it ourselves
    *>      based on slot/button/mode and then check if it matches the client's data.

    *> ignore clicks in windows other than the current one
    IF WINDOW-ID NOT = PLAYER-WINDOW-ID(PLAYER-ID)
        PERFORM AbortEarly
    END-IF

    CALL "GetCallback-WindowSync" USING PLAYER-WINDOW-TYPE(PLAYER-ID) SYNC-PTR
    CALL "GetCallback-WindowSetSlot" USING PLAYER-WINDOW-TYPE(PLAYER-ID) SET-SLOT-PTR
    CALL "GetCallback-WindowDrop" USING PLAYER-WINDOW-TYPE(PLAYER-ID) DROP-PTR
    IF SYNC-PTR = NULL OR SET-SLOT-PTR = NULL OR DROP-PTR = NULL
        DISPLAY "RecvPacket-ContainerClick: Unable to handle window type " PLAYER-WINDOW-TYPE(PLAYER-ID)
        PERFORM AbortEarly
    END-IF

    *> sync client if state ID differs from last sent
    IF (WINDOW-ID = 0 AND STATE-ID NOT = PLAYER-INVENTORY-STATE(PLAYER-ID)) OR (WINDOW-ID > 0 AND STATE-ID NOT = PLAYER-WINDOW-STATE(PLAYER-ID))
        CALL SYNC-PTR USING PLAYER-ID
        PERFORM AbortEarly
    END-IF

    *> TODO handle offhand swap key (F) in containers
    If WINDOW-ID > 0 AND MODE-ENUM = 2 AND BUTTON = 40
        CALL "Inventory-SyncPlayerInventory" USING PLAYER-ID
        PERFORM AbortEarly
    END-IF

    IF MODE-ENUM = 0 AND SLOT = -999 AND PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID) > 0
        *> click outside inventory; button 0: drop stack, button 1: drop single item
        MOVE PLAYER-MOUSE-ITEM(PLAYER-ID) TO CLIENT-SLOT
        IF BUTTON = 0
            MOVE 0 TO PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID)
        ELSE
            MOVE 1 TO CLIENT-SLOT-COUNT
            SUBTRACT 1 FROM PLAYER-MOUSE-SLOT-COUNT(PLAYER-ID)
        END-IF
        CALL "World-DropItem-FromPlayer" USING CLIENT-SLOT PLAYER-ID
    END-IF

    IF MODE-ENUM = 4
        *> drop specific slot
        IF SLOT = -999
            PERFORM AbortEarly
        END-IF
        CALL DROP-PTR USING PLAYER-ID SLOT BUTTON SYNC-REQUIRED
    END-IF

    *> TODO implement painting properly
    *> For now we ignore all but the end event, assuming the client will send the changed slots with the end event.
    IF MODE-ENUM = 5 AND NOT (BUTTON = 2 OR 6 OR 10)
        PERFORM AbortEarly
    END-IF

    *> iterate changed slots
    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET CHANGED-SLOT-COUNT
    IF CHANGED-SLOT-COUNT < 0 OR CHANGED-SLOT-COUNT > 128
        GOBACK
    END-IF

    PERFORM CHANGED-SLOT-COUNT TIMES
        CALL "Decode-Short" USING LK-BUFFER LK-OFFSET SLOT-NUMBER
        CALL "Decode-InventorySlot" USING LK-BUFFER LK-OFFSET CLIENT-SLOT

        CALL SET-SLOT-PTR USING PLAYER-ID SLOT-NUMBER CLIENT-SLOT SLOT-SYNC-REQUIRED
        IF SLOT-SYNC-REQUIRED NOT = 0
            MOVE 1 TO SYNC-REQUIRED
        END-IF
    END-PERFORM

    *> carried item
    CALL "Decode-InventorySlot" USING LK-BUFFER LK-OFFSET PLAYER-MOUSE-ITEM(PLAYER-ID)

    IF SYNC-REQUIRED = 1
        CALL SYNC-PTR USING PLAYER-ID
    END-IF

    GOBACK.

AbortEarly.
    *> Call when reading the packet is aborted early to still consume all slot data,
    *> which avoids a warning in the log about unread data.

    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET CHANGED-SLOT-COUNT
    IF CHANGED-SLOT-COUNT < 0 OR CHANGED-SLOT-COUNT > 128
        EXIT PARAGRAPH
    END-IF

    PERFORM CHANGED-SLOT-COUNT TIMES
        CALL "Decode-Short" USING LK-BUFFER LK-OFFSET SLOT-NUMBER
        CALL "Decode-InventorySlot" USING LK-BUFFER LK-OFFSET CLIENT-SLOT
    END-PERFORM

    CALL "Decode-InventorySlot" USING LK-BUFFER LK-OFFSET CLIENT-SLOT

    GOBACK.

END PROGRAM RecvPacket-ContainerClick.
