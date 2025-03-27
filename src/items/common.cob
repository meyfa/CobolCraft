*> --- ItemUtil-GetReplaceablePosition ---
*> Blocks should be placed at the clicked position if the block is replaceable. For non-replaceable blocks, they should
*> be placed next to the clicked position (depending on the face clicked). This helper method calculates the correct
*> position for the block to be placed at.
*> In case there is no valid position (no replaceable block, or outside of world bounds), the valid flag will be 0.
IDENTIFICATION DIVISION.
PROGRAM-ID. ItemUtil-GetReplaceablePosition.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BLOCK-STATE              BINARY-LONG.
    01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
    01 CB-PTR-REPLACEABLE       PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                     BINARY-LONG.
        02 LK-Y                     BINARY-LONG.
        02 LK-Z                     BINARY-LONG.
    01 LK-FACE                  BINARY-LONG.
    01 LK-VALID                 BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-POSITION LK-FACE LK-VALID.
    CALL "World-GetBlock" USING LK-POSITION BLOCK-STATE
    CALL "GetCallback-BlockReplaceable" USING BLOCK-STATE CB-PTR-REPLACEABLE
    CALL CB-PTR-REPLACEABLE USING BLOCK-STATE CHECK-RESULT
    IF CHECK-RESULT = 1
        MOVE 1 TO LK-VALID
        GOBACK
    END-IF

    CALL "Facing-GetRelative" USING LK-FACE LK-POSITION
    CALL "World-CheckBounds" USING LK-POSITION CHECK-RESULT
    IF CHECK-RESULT NOT = 0
        MOVE 0 TO LK-VALID
        GOBACK
    END-IF

    CALL "World-GetBlock" USING LK-POSITION BLOCK-STATE
    CALL "GetCallback-BlockReplaceable" USING BLOCK-STATE CB-PTR-REPLACEABLE
    CALL CB-PTR-REPLACEABLE USING BLOCK-STATE CHECK-RESULT
    IF CHECK-RESULT = 0
        MOVE 0 TO LK-VALID
        GOBACK
    END-IF

    MOVE 1 TO LK-VALID
    GOBACK.

END PROGRAM ItemUtil-GetReplaceablePosition.

*> --- ItemUtil-ConsumeItem ---
*> Consumes the item in the player's hand, reducing its count by 1. Creative mode players are unaffected.
*> Optionally, a replacement item ID can be specified, which will be added to the player's inventory or dropped if the
*> inventory is full.
IDENTIFICATION DIVISION.
PROGRAM-ID. ItemUtil-ConsumeItem.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    01 SLOT                     BINARY-LONG UNSIGNED.
    01 REPLACEMENT-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==REPLACEMENT==.
LINKAGE SECTION.
    01 LK-PLAYER                BINARY-LONG UNSIGNED.
    01 LK-SLOT                  BINARY-LONG UNSIGNED.
    01 LK-REPLACEMENT-ID        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER LK-SLOT OPTIONAL LK-REPLACEMENT-ID.
    IF PLAYER-GAMEMODE(LK-PLAYER) = 1
        GOBACK
    END-IF

    IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, LK-SLOT + 1) > 0
        SUBTRACT 1 FROM PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, LK-SLOT + 1)
    END-IF

    IF LK-REPLACEMENT-ID IS NOT OMITTED
        MOVE LK-REPLACEMENT-ID TO REPLACEMENT-SLOT-ID
        MOVE 1 TO REPLACEMENT-SLOT-COUNT
        *> TODO data components
        MOVE 2 TO REPLACEMENT-SLOT-NBT-LENGTH
        MOVE X"0000" TO REPLACEMENT-SLOT-NBT-DATA(1:2)

        *> Prefer the same slot if possible
        IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER, LK-SLOT + 1) = 0
            MOVE REPLACEMENT-SLOT TO PLAYER-INVENTORY-SLOT(LK-PLAYER, LK-SLOT + 1)
        ELSE
            CALL "Inventory-StoreItem" USING PLAYER-INVENTORY(LK-PLAYER) REPLACEMENT-SLOT
            IF REPLACEMENT-SLOT-COUNT > 0
                CALL "World-DropItem-FromPlayer" USING REPLACEMENT-SLOT LK-PLAYER
            END-IF
        END-IF
    END-IF

    *> TODO send only changed slots
    CALL "Inventory-SyncPlayerInventory" USING LK-PLAYER

    GOBACK.

END PROGRAM ItemUtil-ConsumeItem.
