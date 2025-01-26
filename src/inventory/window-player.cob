*> --- RegisterWindow-Player ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterWindow-Player.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> the player inventory window is not one of the registered window types
    01 WINDOW-TYPE                  BINARY-LONG                 VALUE -1.
    01 CLOSE-PTR                    PROGRAM-POINTER.

PROCEDURE DIVISION.
    SET CLOSE-PTR TO ENTRY "Callback-Close"
    CALL "SetCallback-WindowClose" USING WINDOW-TYPE CLOSE-PTR
    GOBACK.

    *> --- Callback-Close ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Close.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
    LINKAGE SECTION.
        COPY DD-CALLBACK-WINDOW-CLOSE.

    PROCEDURE DIVISION USING LK-PLAYER.
        CALL "Inventory-StoreItem" USING PLAYER-INVENTORY(LK-PLAYER) PLAYER-MOUSE-ITEM(LK-PLAYER)
        IF PLAYER-MOUSE-SLOT-COUNT(LK-PLAYER) > 0
            *> TODO drop item
            CONTINUE
        END-IF
        ADD 1 TO PLAYER-WINDOW-STATE(LK-PLAYER)
        CALL "SendPacket-SetContainerContent" USING LK-PLAYER PLAYER-WINDOW-STATE(LK-PLAYER)
            PLAYER-INVENTORY(LK-PLAYER) PLAYER-MOUSE-ITEM(LK-PLAYER)
        GOBACK.

    END PROGRAM Callback-Close.

END PROGRAM RegisterWindow-Player.
