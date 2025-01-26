IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-UseItemOn.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-ITEM         PIC X(14) VALUE "minecraft:item".
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    01 PLAYER-ID                BINARY-LONG.
    *> payload
    01 HAND-ENUM                BINARY-LONG.
    01 LOCATION.
        02 LOCATION-X           BINARY-LONG.
        02 LOCATION-Y           BINARY-LONG.
        02 LOCATION-Z           BINARY-LONG.
    01 FACE-ENUM                BINARY-LONG.
    01 CURSOR-POS.
        02 CURSOR-POS-X         FLOAT-SHORT.
        02 CURSOR-POS-Y         FLOAT-SHORT.
        02 CURSOR-POS-Z         FLOAT-SHORT.
    01 SEQUENCE-ID              BINARY-LONG.
    *> variables
    01 SLOT-INDEX               BINARY-LONG UNSIGNED.
    01 ITEM-ID                  BINARY-LONG.
    01 BLOCK-STATE-ID           BINARY-LONG.
    01 ITEM-IDENTIFIER          PIC X(255).
    01 CALLBACK-PTR-ITEM        PROGRAM-POINTER.
    01 CALLBACK-PTR-BLOCK       PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET HAND-ENUM
    CALL "Decode-Position" USING LK-BUFFER LK-OFFSET LOCATION
    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET FACE-ENUM
    CALL "Decode-Float" USING LK-BUFFER LK-OFFSET CURSOR-POS-X
    CALL "Decode-Float" USING LK-BUFFER LK-OFFSET CURSOR-POS-Y
    CALL "Decode-Float" USING LK-BUFFER LK-OFFSET CURSOR-POS-Z
    *> TODO: "inside block" flag, "world border hit" flag
    ADD 2 TO LK-OFFSET
    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET SEQUENCE-ID

    *> hand enum: 0=main hand, 1=off hand
    IF HAND-ENUM = 0
        *> compute the inventory slot
        COMPUTE SLOT-INDEX = 36 + PLAYER-HOTBAR(PLAYER-ID)
    ELSE
        MOVE 45 TO SLOT-INDEX
    END-IF

    *> Determine the item's "use" callback
    MOVE PLAYER-INVENTORY-SLOT-ID(PLAYER-ID, SLOT-INDEX + 1) TO ITEM-ID
    CALL "Registries-Get-EntryName" USING C-MINECRAFT-ITEM ITEM-ID ITEM-IDENTIFIER
    CALL "GetCallback-ItemUse" USING ITEM-IDENTIFIER CALLBACK-PTR-ITEM

    *> Determine the current block's "interact" callback
    CALL "World-GetBlock" USING LOCATION BLOCK-STATE-ID
    CALL "GetCallback-BlockInteract" USING BLOCK-STATE-ID CALLBACK-PTR-BLOCK

    *> If the player is sneaking, we should execute the item's "use" callback instead of the block's
    *> "interact" callback - unless the item has no "use" callback.
    EVALUATE TRUE
        WHEN CALLBACK-PTR-ITEM NOT = NULL AND (CALLBACK-PTR-BLOCK = NULL OR PLAYER-SNEAKING(PLAYER-ID) NOT = 0)
            CALL CALLBACK-PTR-ITEM USING PLAYER-ID ITEM-IDENTIFIER LOCATION FACE-ENUM CURSOR-POS
        WHEN CALLBACK-PTR-BLOCK NOT = NULL
            CALL CALLBACK-PTR-BLOCK USING PLAYER-ID ITEM-IDENTIFIER LOCATION FACE-ENUM CURSOR-POS
    END-EVALUATE

    *> Acknowledge the action
    CALL "SendPacket-AckBlockChange" USING LK-CLIENT SEQUENCE-ID

    *> For survival mode, send the inventory slot contents, as they likely should have changed but we don't do that.
    *> TODO get smarter about this
    IF PLAYER-GAMEMODE(PLAYER-ID) NOT = 1
        CALL "Inventory-SyncPlayerInventory" USING PLAYER-ID
    END-IF

    GOBACK.

END PROGRAM RecvPacket-UseItemOn.
