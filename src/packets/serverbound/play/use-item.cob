IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-UseItem.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    01 PLAYER-ID                BINARY-LONG.
    *> payload
    01 HAND-ENUM                BINARY-LONG.
    01 SEQUENCE-ID              BINARY-LONG.
    01 HEAD-YAW                 FLOAT-SHORT.
    01 HEAD-PITCH               FLOAT-SHORT.
    *> variables
    01 SLOT-INDEX               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET HAND-ENUM
    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET SEQUENCE-ID
    CALL "Decode-Float" USING LK-BUFFER LK-OFFSET HEAD-YAW
    CALL "Decode-Float" USING LK-BUFFER LK-OFFSET HEAD-PITCH

    *> hand enum: 0=main hand, 1=offhand
    IF HAND-ENUM = 0
        COMPUTE SLOT-INDEX = 36 + PLAYER-HOTBAR(PLAYER-ID)
    ELSE
        MOVE 45 TO SLOT-INDEX
    END-IF

    *> TODO: Buckets send this packet when clicking a liquid directly - handle this case
    *> TODO food items
    *> TODO potions
    *> TODO splash potions
    *> TODO lingering potions
    *> TODO bows
    *> TODO shields
    *> TODO bottle o' enchanting
    *> TODO eggs, snowballs, ender pearls, etc.

    *> Acknowledge the action
    CALL "SendPacket-AckBlockChange" USING LK-CLIENT SEQUENCE-ID

    *> For survival mode, send the inventory slot contents, as they likely should have changed but we don't do that.
    *> TODO get smarter about this
    IF PLAYER-GAMEMODE(PLAYER-ID) NOT = 1
        CALL "Inventory-SyncPlayerInventory" USING PLAYER-ID
    END-IF

    GOBACK.

END PROGRAM RecvPacket-UseItem.
