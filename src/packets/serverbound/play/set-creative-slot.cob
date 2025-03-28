IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-SetCreativeSlot.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    01 PLAYER-ID                BINARY-LONG.
    01 SLOT-NUMBER              BINARY-SHORT.
    01 CLIENT-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==CLIENT==.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    CALL "Decode-Short" USING LK-BUFFER LK-OFFSET SLOT-NUMBER
    CALL "Decode-InventorySlot" USING LK-BUFFER LK-OFFSET CLIENT-SLOT

    IF PLAYER-GAMEMODE(PLAYER-ID) NOT = 1
        GOBACK
    END-IF

    EVALUATE SLOT-NUMBER
        WHEN 0 THRU 45
            MOVE CLIENT-SLOT TO PLAYER-INVENTORY-SLOT(PLAYER-ID, SLOT-NUMBER + 1)
        WHEN -1
            CALL "World-DropItem-FromPlayer" USING CLIENT-SLOT PLAYER-ID
        WHEN OTHER
            DISPLAY "Invalid slot number: " SLOT-NUMBER
    END-EVALUATE

    GOBACK.

END PROGRAM RecvPacket-SetCreativeSlot.
