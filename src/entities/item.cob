*> --- RegisterEntity-Item ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterEntity-Item.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ITEM-REGISTRY            BINARY-LONG                     GLOBAL.
    01 SERIALIZE-PTR            PROGRAM-POINTER.
    01 DESERIALIZE-PTR          PROGRAM-POINTER.
    01 TICK-PTR                 PROGRAM-POINTER.
    01 ENTITY-TYPE              BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    CALL "Registries-LookupRegistry" USING "minecraft:item" ITEM-REGISTRY

    SET SERIALIZE-PTR TO ENTRY "Callback-Serialize"
    SET DESERIALIZE-PTR TO ENTRY "Callback-Deserialize"
    SET TICK-PTR TO ENTRY "Callback-Tick"

    CALL "Registries-Lookup" USING "minecraft:entity_type" "minecraft:item" ENTITY-TYPE

    CALL "SetCallback-EntitySerialize" USING ENTITY-TYPE SERIALIZE-PTR
    CALL "SetCallback-EntityDeserialize" USING ENTITY-TYPE DESERIALIZE-PTR
    CALL "SetCallback-EntityTick" USING ENTITY-TYPE TICK-PTR

    GOBACK.

    *> --- Callback-Serialize ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Serialize.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR                  PIC X(256).
        01 LEN                  BINARY-LONG UNSIGNED.
        01 INT16                BINARY-SHORT.
        01 INT32                BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ENTITY-SERIALIZE.

    PROCEDURE DIVISION USING LK-ENTITY LK-NBTENC LK-BUFFER.
        CALL "EntityBase-Serialize" USING LK-ENTITY LK-NBTENC LK-BUFFER

        *> Age
        MOVE LK-ENTITY-AGE TO INT16
        CALL "NbtEncode-Short" USING LK-NBTENC LK-BUFFER "Age" INT16

        *> Item slot
        CALL "NbtEncode-Compound" USING LK-NBTENC LK-BUFFER "Item"

        *> Item: id
        MOVE ENTITY-ITEM-SLOT-ID TO INT32
        CALL "Registries-EntryName" USING ITEM-REGISTRY INT32 STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO LEN
        CALL "NbtEncode-String" USING LK-NBTENC LK-BUFFER "id" STR LEN

        *> Item: count
        MOVE ENTITY-ITEM-SLOT-COUNT TO INT32
        CALL "NbtEncode-Int" USING LK-NBTENC LK-BUFFER "Count" INT32

        *> Item: components
        *> TODO encode the structured components
        IF ENTITY-ITEM-SLOT-NBT-LENGTH > 0
            CALL "NbtEncode-ByteBuffer" USING LK-NBTENC LK-BUFFER "tag" ENTITY-ITEM-SLOT-NBT-DATA ENTITY-ITEM-SLOT-NBT-LENGTH
        END-IF

        CALL "NbtEncode-EndCompound" USING LK-NBTENC LK-BUFFER

        *> Pickup delay
        MOVE LK-ENTITY-ITEM-PICKUP-DELAY TO INT16
        CALL "NbtEncode-Short" USING LK-NBTENC LK-BUFFER "PickupDelay" INT16

        *> TODO health, owner, thrower

        GOBACK.

    END PROGRAM Callback-Serialize.

    *> --- Callback-Deserialize ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Deserialize.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 AT-END               BINARY-CHAR UNSIGNED.
        01 LEN                  BINARY-LONG UNSIGNED.
        01 TAG                  PIC X(16).
        01 STR                  PIC X(256).
        01 INT16                BINARY-SHORT.
        01 INT32                BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ENTITY-DESERIALIZE.

    PROCEDURE DIVISION USING LK-ENTITY LK-NBTDEC LK-BUFFER LK-TAG.
        EVALUATE LK-TAG
            WHEN "Age"
                CALL "NbtDecode-Short" USING LK-NBTDEC LK-BUFFER INT16
                MOVE INT16 TO LK-ENTITY-AGE
            WHEN "Item"
                PERFORM DeserializeItem
            WHEN "PickupDelay"
                CALL "NbtDecode-Short" USING LK-NBTDEC LK-BUFFER INT16
                MOVE INT16 TO LK-ENTITY-ITEM-PICKUP-DELAY
            WHEN OTHER
                CALL "EntityBase-Deserialize" USING LK-ENTITY LK-NBTDEC LK-BUFFER LK-TAG
        END-EVALUATE
        GOBACK.

    DeserializeItem.
        CALL "NbtDecode-Compound" USING LK-NBTDEC LK-BUFFER
        PERFORM UNTIL EXIT
            CALL "NbtDecode-Peek" USING LK-NBTDEC LK-BUFFER AT-END TAG
            IF AT-END > 0
                EXIT PERFORM
            END-IF
            EVALUATE TAG
                WHEN "id"
                    CALL "NbtDecode-String" USING LK-NBTDEC LK-BUFFER STR LEN
                    CALL "Registries-Lookup" USING "minecraft:item" STR(1:LEN) ENTITY-ITEM-SLOT-ID
                WHEN "Count"
                    CALL "NbtDecode-Int" USING LK-NBTDEC LK-BUFFER INT32
                    MOVE INT32 TO ENTITY-ITEM-SLOT-COUNT
                *> TODO: decode the structured components
                WHEN "tag"
                    CALL "NbtDecode-ByteBuffer" USING LK-NBTDEC LK-BUFFER ENTITY-ITEM-SLOT-NBT-DATA ENTITY-ITEM-SLOT-NBT-LENGTH
                WHEN OTHER
                    CALL "NbtDecode-Skip" USING LK-NBTDEC LK-BUFFER
            END-EVALUATE
        END-PERFORM
        CALL "NbtDecode-EndCompound" USING LK-NBTDEC LK-BUFFER
        .

    END PROGRAM Callback-Deserialize.

    *> --- Callback-Tick ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Tick.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        COPY DD-CLIENTS.
        COPY DD-CLIENT-STATES.
        COPY DD-SERVER-PROPERTIES.
        01 ENTITY-AABB.
            COPY DD-AABB REPLACING LEADING ==PREFIX== BY ==ENTITY==.
        01 PLAYER-INDEX             BINARY-LONG UNSIGNED.
        01 COLLISION                BINARY-CHAR UNSIGNED.
        01 PREVIOUS-ITEM-COUNT      BINARY-LONG UNSIGNED.
        01 COLLECTED-ITEM-COUNT     BINARY-LONG UNSIGNED.
        01 CLIENT-ID                BINARY-LONG UNSIGNED.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 BLOCK-ID                 BINARY-LONG.
        01 REPLACEABLE-PTR          PROGRAM-POINTER.
        01 REPLACEABLE              BINARY-CHAR UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ENTITY-TICK.

    PROCEDURE DIVISION USING LK-ENTITY LK-PLAYER-AABBS LK-REMOVE.
        MOVE 0 TO LK-REMOVE
        ADD 1 TO LK-ENTITY-AGE
        IF LK-ENTITY-AGE >  6000
            MOVE 1 TO LK-REMOVE
            GOBACK
        END-IF
        PERFORM UpdatePosition
        COMPUTE LK-ENTITY-ITEM-PICKUP-DELAY = FUNCTION MAX(LK-ENTITY-ITEM-PICKUP-DELAY - 1, 0)
        IF LK-ENTITY-ITEM-PICKUP-DELAY <= 0
            PERFORM TryPickupItem
        END-IF
        GOBACK.

    UpdatePosition.
        *> TODO: Handle block collisions properly... This is just a hack for now.
        COMPUTE BLOCK-X ROUNDED MODE IS TOWARD-LESSER = LK-ENTITY-X + LK-ENTITY-VELOCITY-X
        COMPUTE BLOCK-Y ROUNDED MODE IS TOWARD-LESSER = LK-ENTITY-Y + LK-ENTITY-VELOCITY-Y
        COMPUTE BLOCK-Z ROUNDED MODE IS TOWARD-LESSER = LK-ENTITY-Z + LK-ENTITY-VELOCITY-Z
        PERFORM CheckBlockCollision
        IF COLLISION = 0
            COMPUTE LK-ENTITY-X = LK-ENTITY-X + LK-ENTITY-VELOCITY-X
            COMPUTE LK-ENTITY-Y = LK-ENTITY-Y + LK-ENTITY-VELOCITY-Y
            COMPUTE LK-ENTITY-Z = LK-ENTITY-Z + LK-ENTITY-VELOCITY-Z

            COMPUTE LK-ENTITY-VELOCITY-X = LK-ENTITY-VELOCITY-X * 0.98
            COMPUTE LK-ENTITY-VELOCITY-Y = LK-ENTITY-VELOCITY-Y * 0.98 - 0.04
            COMPUTE LK-ENTITY-VELOCITY-Z = LK-ENTITY-VELOCITY-Z * 0.98

            PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                IF CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
                    CALL "SendPacket-EntityPositionSync" USING CLIENT-ID LK-ENTITY-ID LK-ENTITY-POSITION LK-ENTITY-ROTATION LK-ENTITY-VELOCITY LK-ENTITY-ON-GROUND
                END-IF
            END-PERFORM
        ELSE
            MOVE 0 TO LK-ENTITY-VELOCITY-X LK-ENTITY-VELOCITY-Y LK-ENTITY-VELOCITY-Z
        END-IF
        .

    TryPickupItem.
        *> Compute the entity AABB, expanded by 1 block horizontally and 0.5 blocks vertically, since the player's pickup
        *> bounding box is larger than their regular hitbox, but we don't want to recompute each player's AABB for every
        *> item entity.
        COMPUTE ENTITY-AABB-MIN-X = LK-ENTITY-X - 0.125 - 1
        COMPUTE ENTITY-AABB-MAX-X = LK-ENTITY-X + 0.125 + 1
        COMPUTE ENTITY-AABB-MIN-Y = LK-ENTITY-Y         - 0.5
        COMPUTE ENTITY-AABB-MAX-Y = LK-ENTITY-Y + 0.25  + 0.5
        COMPUTE ENTITY-AABB-MIN-Z = LK-ENTITY-Z - 0.125 - 1
        COMPUTE ENTITY-AABB-MAX-Z = LK-ENTITY-Z + 0.125 + 1

        *> Check for player collisions
        PERFORM VARYING PLAYER-INDEX FROM 1 BY 1 UNTIL PLAYER-INDEX > MAX-PLAYERS
            IF PLAYER-CLIENT(PLAYER-INDEX) > 0
                CALL "CheckCollisionAABB" USING ENTITY-AABB LK-PLAYER-AABB(PLAYER-INDEX) COLLISION
                IF COLLISION NOT = 0
                    MOVE ENTITY-ITEM-SLOT-COUNT TO PREVIOUS-ITEM-COUNT
                    CALL "Inventory-StoreItem" USING PLAYER-INVENTORY(PLAYER-INDEX) LK-ENTITY-ITEM-SLOT
                    IF ENTITY-ITEM-SLOT-COUNT < PREVIOUS-ITEM-COUNT
                        COMPUTE COLLECTED-ITEM-COUNT = ENTITY-ITEM-SLOT-COUNT - PREVIOUS-ITEM-COUNT
                        PERFORM SendPickupItem
                        *> TODO sync just the slot that changed
                        CALL "Inventory-SyncPlayerInventory" USING PLAYER-INDEX
                    END-IF
                    IF ENTITY-ITEM-SLOT-COUNT < 1
                        PERFORM SendPickupItem
                        MOVE 1 TO LK-REMOVE
                        GOBACK
                    END-IF
                END-IF
            END-IF
        END-PERFORM
        .

    SendPickupItem.
        PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
            IF CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
                CALL "SendPacket-TakeItemEntity" USING CLIENT-ID LK-ENTITY-ID PLAYER-INDEX COLLECTED-ITEM-COUNT
            END-IF
        END-PERFORM
        .

    CheckBlockCollision.
        CALL "World-CheckBounds" USING BLOCK-POSITION COLLISION
        IF COLLISION NOT = 0
            MOVE 0 TO COLLISION
            EXIT PARAGRAPH
        END-IF

        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        IF BLOCK-ID <= 0
            MOVE 0 TO COLLISION
            EXIT PARAGRAPH
        END-IF

        CALL "GetCallback-BlockReplaceable" USING BLOCK-ID REPLACEABLE-PTR
        IF REPLACEABLE-PTR = NULL
            MOVE 0 TO COLLISION
            EXIT PARAGRAPH
        END-IF

        CALL REPLACEABLE-PTR USING BLOCK-POSITION REPLACEABLE
        IF REPLACEABLE = 0
            MOVE 1 TO COLLISION
        ELSE
            MOVE 0 TO COLLISION
        END-IF
        .

    END PROGRAM Callback-Tick.

END PROGRAM RegisterEntity-Item.
