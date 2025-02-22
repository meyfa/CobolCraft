*> --- World-Save ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-Save.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-SERVER-PROPERTIES.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 WORLD-PATH               PIC X(1024).
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    *> Create directories. Ignore errors, as they are likely to be caused by the directories already existing.
    CALL "CBL_CREATE_DIR" USING SP-LEVEL-NAME

    INITIALIZE WORLD-PATH
    STRING FUNCTION TRIM(SP-LEVEL-NAME) "/region" INTO WORLD-PATH
    CALL "CBL_CREATE_DIR" USING WORLD-PATH

    INITIALIZE WORLD-PATH
    STRING FUNCTION TRIM(SP-LEVEL-NAME) "/entities" INTO WORLD-PATH
    CALL "CBL_CREATE_DIR" USING WORLD-PATH

    *> Save world metadata
    CALL "World-SaveLevel" USING LK-FAILURE
    IF LK-FAILURE > 0
        GOBACK
    END-IF

    *> Save dirty chunks
    PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > WORLD-CHUNK-COUNT
        SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)
        IF CHUNK-DIRTY-BLOCKS > 0 OR CHUNK-DIRTY-ENTITIES > 0
            CALL "World-SaveChunk" USING CHUNK-INDEX LK-FAILURE
            IF LK-FAILURE > 0
                GOBACK
            END-IF
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM World-Save.

*> --- World-Load ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-Load.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 LEVEL-SAVE-REQUIRED      BINARY-CHAR UNSIGNED.
    01 CHUNK-SAVE-REQUIRED      BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> Load the world metadata
    CALL "World-LoadLevel" USING LEVEL-SAVE-REQUIRED

    *> Load just the spawn area
    PERFORM UNTIL WORLD-CHUNK-COUNT = 0
        CALL "World-FreeChunk" USING WORLD-CHUNK-COUNT
    END-PERFORM
    CALL "World-EnsureSpawnChunks" USING CHUNK-SAVE-REQUIRED

    *> Save the world if necessary
    IF LEVEL-SAVE-REQUIRED > 0 OR CHUNK-SAVE-REQUIRED > 0
        CALL "World-Save" USING LK-FAILURE
    END-IF

    GOBACK.

END PROGRAM World-Load.

*> --- World-Tick ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-Tick.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    COPY DD-CHUNK-ENTITY.
    COPY DD-PLAYERS.
    COPY DD-CLIENTS.
    COPY DD-CLIENT-STATES.
    COPY DD-SERVER-PROPERTIES.
    01 ENTITY-TYPE-ITEM         BINARY-LONG                 VALUE -1.
    01 PLAYER-INDEX             BINARY-LONG UNSIGNED.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 ENTITY-PTR               POINTER.
    *> Pre-computed player bounding boxes
    01 PLAYER-AABBS.
        02 PLAYER-AABB OCCURS PLAYER-CAPACITY TIMES.
            COPY DD-AABB REPLACING LEADING ==PREFIX== BY ==PLAYER==.
    01 ENTITY-AABB.
        COPY DD-AABB REPLACING LEADING ==PREFIX== BY ==ENTITY==.
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

PROCEDURE DIVISION.
    IF ENTITY-TYPE-ITEM < 0
        CALL "Registries-Get-EntryId" USING "minecraft:entity_type" "minecraft:item" ENTITY-TYPE-ITEM
    END-IF

    ADD 1 TO WORLD-AGE
    ADD 1 TO WORLD-TIME

    *> Compute player bounding boxes
    PERFORM VARYING PLAYER-INDEX FROM 1 BY 1 UNTIL PLAYER-INDEX > MAX-PLAYERS
        IF PLAYER-CLIENT(PLAYER-INDEX) > 0
            CALL "PlayerAABB" USING PLAYER-POSITION(PLAYER-INDEX) PLAYER-SNEAKING(PLAYER-INDEX) PLAYER-AABB(PLAYER-INDEX)
        END-IF
    END-PERFORM

    *> Tick chunks
    PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > WORLD-CHUNK-COUNT
        SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)

        *> We cheat a bit and say that any chunk where an entity is ticked must save its entities.
        *> TODO Can we only set this when an entity has actually been modified?
        IF CHUNK-ENTITY-COUNT > 0
            MOVE 1 TO CHUNK-DIRTY-ENTITIES
        END-IF

        *> Tick entities
        SET ENTITY-PTR TO CHUNK-ENTITY-LIST
        PERFORM UNTIL ENTITY-PTR = NULL
            *> The order here ensures that we have the next-pointer available in case the entity is deallocated
            SET ADDRESS OF ENTITY-LIST TO ENTITY-PTR
            SET ENTITY-PTR TO ENTITY-LIST-NEXT
            PERFORM TickEntity
        END-PERFORM
    END-PERFORM

    GOBACK.

TickEntity.
    ADD 1 TO ENTITY-AGE

    IF ENTITY-ID = ENTITY-TYPE-ITEM
        PERFORM TickItemEntity
    END-IF
    .

TickItemEntity.
    *> TODO Move this into somewhere abstract, so that we can tick other entities as well
    IF ENTITY-AGE >  6000
        CALL "World-RemoveEntity" USING ENTITY-LIST-ENTITY
        EXIT PARAGRAPH
    END-IF

    *> TODO: Handle block collisions properly... This is just a hack for now.
    COMPUTE BLOCK-X ROUNDED MODE IS TOWARD-LESSER = ENTITY-X + ENTITY-VELOCITY-X
    COMPUTE BLOCK-Y ROUNDED MODE IS TOWARD-LESSER = ENTITY-Y + ENTITY-VELOCITY-Y
    COMPUTE BLOCK-Z ROUNDED MODE IS TOWARD-LESSER = ENTITY-Z + ENTITY-VELOCITY-Z
    PERFORM CheckBlockCollision
    IF COLLISION = 0
        COMPUTE ENTITY-X = ENTITY-X + ENTITY-VELOCITY-X
        COMPUTE ENTITY-Y = ENTITY-Y + ENTITY-VELOCITY-Y
        COMPUTE ENTITY-Z = ENTITY-Z + ENTITY-VELOCITY-Z

        COMPUTE ENTITY-VELOCITY-X = ENTITY-VELOCITY-X * 0.98
        COMPUTE ENTITY-VELOCITY-Y = ENTITY-VELOCITY-Y * 0.98 - 0.04
        COMPUTE ENTITY-VELOCITY-Z = ENTITY-VELOCITY-Z * 0.98

        PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
            IF CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
                CALL "SendPacket-EntityPositionSync" USING CLIENT-ID ENTITY-ID ENTITY-POSITION ENTITY-ROTATION ENTITY-VELOCITY ENTITY-ON-GROUND
            END-IF
        END-PERFORM
    ELSE
        MOVE 0 TO ENTITY-VELOCITY-X ENTITY-VELOCITY-Y ENTITY-VELOCITY-Z
    END-IF

    COMPUTE ENTITY-ITEM-PICKUP-DELAY = FUNCTION MAX(ENTITY-ITEM-PICKUP-DELAY - 1, 0)

    IF ENTITY-ITEM-PICKUP-DELAY <= 0
        *> Compute the entity AABB, expanded by 1 block horizontally and 0.5 blocks vertically, since the player's pickup
        *> bounding box is larger than their regular hitbox, but we don't want to recompute each player's AABB for every
        *> item entity.
        COMPUTE ENTITY-AABB-MIN-X = ENTITY-X - 0.125 - 1
        COMPUTE ENTITY-AABB-MAX-X = ENTITY-X + 0.125 + 1
        COMPUTE ENTITY-AABB-MIN-Y = ENTITY-Y         - 0.5
        COMPUTE ENTITY-AABB-MAX-Y = ENTITY-Y + 0.25  + 0.5
        COMPUTE ENTITY-AABB-MIN-Z = ENTITY-Z - 0.125 - 1
        COMPUTE ENTITY-AABB-MAX-Z = ENTITY-Z + 0.125 + 1

        *> Check for player collisions
        PERFORM VARYING PLAYER-INDEX FROM 1 BY 1 UNTIL PLAYER-INDEX > MAX-PLAYERS
            IF PLAYER-CLIENT(PLAYER-INDEX) > 0
                CALL "CheckCollisionAABB" USING ENTITY-AABB PLAYER-AABB(PLAYER-INDEX) COLLISION
                IF COLLISION NOT = 0
                    MOVE ENTITY-ITEM-SLOT-COUNT TO PREVIOUS-ITEM-COUNT
                    CALL "Inventory-StoreItem" USING PLAYER-INVENTORY(PLAYER-INDEX) ENTITY-ITEM-SLOT
                    IF ENTITY-ITEM-SLOT-COUNT < PREVIOUS-ITEM-COUNT
                        COMPUTE COLLECTED-ITEM-COUNT = ENTITY-ITEM-SLOT-COUNT - PREVIOUS-ITEM-COUNT
                        PERFORM SendPickupItem
                        *> TODO sync just the slot that changed
                        CALL "Inventory-SyncPlayerInventory" USING PLAYER-INDEX
                    END-IF
                    IF ENTITY-ITEM-SLOT-COUNT < 1
                        PERFORM SendPickupItem
                        CALL "World-RemoveEntity" USING ENTITY-LIST-ENTITY
                        EXIT PARAGRAPH
                    END-IF
                END-IF
            END-IF
        END-PERFORM
    END-IF

    *> Ensure the entity is in the correct chunk
    CALL "World-UpdateEntityChunk" USING ENTITY-LIST-ENTITY ENTITY-CHUNK-X ENTITY-CHUNK-Z
    .

SendPickupItem.
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
            CALL "SendPacket-TakeItemEntity" USING CLIENT-ID ENTITY-ID PLAYER-INDEX COLLECTED-ITEM-COUNT
        END-IF
    END-PERFORM
    .

CheckBlockCollision.
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

END PROGRAM World-Tick.

*> --- World-FindSpawnLocation ---
*> This program is used to find a suitable spawn location for a new player.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-FindSpawnLocation.

DATA DIVISION.
WORKING-STORAGE SECTION.
    78 SPAWN-RADIUS             VALUE 10.
    78 SPAWN-ATTEMPTS           VALUE 100.
    COPY DD-WORLD.
    01 BLOCK-POSITION.
        02 POS-X                BINARY-LONG.
        02 POS-Y                BINARY-LONG.
        02 POS-Z                BINARY-LONG.
    01 RANDOM-VALUE             FLOAT-LONG                  VALUE 0.
    01 BLOCK-STATE-ID           BINARY-LONG.
    01 BLOCK-IDENTIFIER         PIC X(256).
    01 AIR-COUNT                BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                 FLOAT-LONG.
        02 LK-Y                 FLOAT-LONG.
        02 LK-Z                 FLOAT-LONG.

PROCEDURE DIVISION USING LK-POSITION.
    IF RANDOM-VALUE = 0
        MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE(1:16)) TO RANDOM-VALUE
    END-IF

    MOVE WORLD-SPAWN-X TO POS-X LK-X
    MOVE WORLD-SPAWN-Y TO POS-Y LK-Y
    MOVE WORLD-SPAWN-Z TO POS-Z LK-Z

    PERFORM SPAWN-ATTEMPTS TIMES
        MOVE FUNCTION RANDOM TO RANDOM-VALUE
        COMPUTE POS-X = WORLD-SPAWN-X + (RANDOM-VALUE - 0.5) * SPAWN-RADIUS
        MOVE FUNCTION RANDOM TO RANDOM-VALUE
        COMPUTE POS-Z = WORLD-SPAWN-Z + (RANDOM-VALUE - 0.5) * SPAWN-RADIUS

        MOVE 0 TO AIR-COUNT
        PERFORM VARYING POS-Y FROM 319 BY -1 UNTIL POS-Y < -63
            CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-STATE-ID
            CALL "Blocks-Get-Name" USING BLOCK-STATE-ID BLOCK-IDENTIFIER
            EVALUATE BLOCK-IDENTIFIER
                WHEN = "minecraft:air" OR = "minecraft:cave_air" OR = "minecraft:void_air"
                    ADD 1 TO AIR-COUNT
                WHEN OTHER
                    IF AIR-COUNT >= 2
                        COMPUTE LK-X = POS-X + 0.5
                        COMPUTE LK-Y = POS-Y + 1
                        COMPUTE LK-Z = POS-Z + 0.5
                        GOBACK
                    END-IF
                    MOVE 0 TO AIR-COUNT
            END-EVALUATE
        END-PERFORM
    END-PERFORM

    GOBACK.

END PROGRAM World-FindSpawnLocation.
