*> --- World-SpawnEntity ---
*> Spawn an entity in the world. The entity will be assigned a new ID and UUID.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SpawnEntity.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> There may be 100 players with IDs starting at 1, so 101 is a safe starting point.
    *> TODO rework this to use a more robust method
    01 LAST-ENTITY-ID           BINARY-LONG UNSIGNED             VALUE 101.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    COPY DD-CHUNK-ENTITY.
    01 POS-CHUNK-X              BINARY-LONG.
    01 POS-CHUNK-Z              BINARY-LONG.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    COPY DD-CLIENTS.
    COPY DD-CLIENT-STATES.
    COPY DD-SERVER-PROPERTIES.
    01 CLIENT-ID                BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    *> The entity data. ID and UUID will be set by this program.
    01 LK-ENTITY.
        COPY DD-ENTITY REPLACING LEADING ==ENTITY== BY ==LK-ENTITY==.

PROCEDURE DIVISION USING LK-ENTITY.
    *> Find the chunk
    DIVIDE LK-ENTITY-X BY 16 GIVING POS-CHUNK-X ROUNDED MODE IS TOWARD-LESSER
    DIVIDE LK-ENTITY-Z BY 16 GIVING POS-CHUNK-Z ROUNDED MODE IS TOWARD-LESSER
    CALL "World-EnsureChunk" USING POS-CHUNK-X POS-CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        DISPLAY "Failed to spawn entity: chunk not found"
        GOBACK
    END-IF
    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)

    *> Generate a unique entity ID
    ADD 1 TO LAST-ENTITY-ID
    MOVE LAST-ENTITY-ID TO LK-ENTITY-ID
    CALL "RandomUUID" USING LK-ENTITY-UUID

    *> Instantiate a new entity linked list node with the entity data
    ALLOCATE ENTITY-LIST
    MOVE LK-ENTITY TO ENTITY-LIST-ENTITY

    *> Update the list head
    SET ENTITY-LIST-NEXT TO CHUNK-ENTITY-LIST
    SET CHUNK-ENTITY-LIST TO ADDRESS OF ENTITY-LIST
    ADD 1 TO CHUNK-ENTITY-COUNT

    *> Notify clients
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
            CALL "World-SendEntity" USING CLIENT-ID ENTITY-LIST-ENTITY
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM World-SpawnEntity.

*> --- World-SendEntity ---
*> Send a single entity to a client.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SendEntity.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ENTITY-TYPE-ITEM         BINARY-LONG                 VALUE -1.
    01 BUFFER                   PIC X(256).
    01 BUFFERPOS                BINARY-LONG UNSIGNED.
    01 TEMP-INT32               BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-ENTITY.
        COPY DD-ENTITY REPLACING LEADING ==ENTITY== BY ==LK-ENTITY==.

PROCEDURE DIVISION USING LK-CLIENT LK-ENTITY.
    *> To ensure that the spawn packet and metadata packet are processed in the same tick,
    *> send one packet bundle delimiter before and one after.
    CALL "SendPacket-BundleDelimiter" USING LK-CLIENT
    CALL "SendPacket-SpawnEntity" USING LK-CLIENT LK-ENTITY-ID LK-ENTITY-UUID LK-ENTITY-TYPE LK-ENTITY-POSITION LK-ENTITY-ROTATION LK-ENTITY-VELOCITY
    PERFORM SendMetadata
    CALL "SendPacket-BundleDelimiter" USING LK-CLIENT
    GOBACK.

SendMetadata.
    IF ENTITY-TYPE-ITEM < 0
        CALL "Registries-Get-EntryId" USING "minecraft:entity_type" "minecraft:item" ENTITY-TYPE-ITEM
    END-IF

    *> TODO make this more generic
    IF LK-ENTITY-TYPE = ENTITY-TYPE-ITEM
        MOVE 1 TO BUFFERPOS
        *> index(byte), type(VarInt), value(VarInt)

        IF LK-ENTITY-NO-GRAVITY NOT = 0
            *> index 5: has no gravity; type: boolean; value: true
            MOVE X"05" TO BUFFER(BUFFERPOS:1)
            ADD 1 TO BUFFERPOS
            MOVE 8 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 BUFFER BUFFERPOS
            MOVE X"01" TO BUFFER(BUFFERPOS:1)
            ADD 1 TO BUFFERPOS
        END-IF

        *> index 8: item type; type: slot; value: (item stack)
        MOVE X"08" TO BUFFER(BUFFERPOS:1)
        ADD 1 TO BUFFERPOS
        MOVE 7 TO TEMP-INT32
        CALL "Encode-VarInt" USING TEMP-INT32 BUFFER BUFFERPOS
        *> count
        MOVE ENTITY-ITEM-SLOT-COUNT TO TEMP-INT32
        CALL "Encode-VarInt" USING TEMP-INT32 BUFFER BUFFERPOS
        IF ENTITY-ITEM-SLOT-COUNT > 0
            *> item ID
            CALL "Encode-VarInt" USING ENTITY-ITEM-SLOT-ID BUFFER BUFFERPOS
            *> data components
            MOVE ENTITY-ITEM-SLOT-NBT-DATA(1:ENTITY-ITEM-SLOT-NBT-LENGTH) TO BUFFER(BUFFERPOS:ENTITY-ITEM-SLOT-NBT-LENGTH)
            ADD ENTITY-ITEM-SLOT-NBT-LENGTH TO BUFFERPOS
        END-IF

        *> terminator is 0xFF
        MOVE X"FF" TO BUFFER(BUFFERPOS:1)
        ADD 1 TO BUFFERPOS

        SUBTRACT 1 FROM BUFFERPOS
        CALL "SendPacket-SetEntityMetadata" USING LK-CLIENT LK-ENTITY-ID BUFFERPOS BUFFER
    END-IF
    .

END PROGRAM World-SendEntity.

*> --- World-RemoveEntity ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-RemoveEntity.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-CLIENT-STATES.
    COPY DD-SERVER-PROPERTIES.
    01 CLIENT-ID                BINARY-LONG UNSIGNED.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    COPY DD-CHUNK-ENTITY.
    01 POS-CHUNK-X              BINARY-LONG.
    01 POS-CHUNK-Z              BINARY-LONG.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 PREV-ENTITY-PTR          POINTER.
    01 ENTITY-PTR               POINTER.
    01 NEXT-ENTITY-PTR          POINTER.
LINKAGE SECTION.
    01 LK-ENTITY.
        COPY DD-ENTITY REPLACING LEADING ==ENTITY== BY ==LK-ENTITY==.

PROCEDURE DIVISION USING LK-ENTITY.
    *> Notify clients
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
            CALL "SendPacket-RemoveEntity" USING CLIENT-ID LK-ENTITY-ID
        END-IF
    END-PERFORM

    *> Find the chunk
    DIVIDE LK-ENTITY-X BY 16 GIVING POS-CHUNK-X ROUNDED MODE IS TOWARD-LESSER
    DIVIDE LK-ENTITY-Z BY 16 GIVING POS-CHUNK-Z ROUNDED MODE IS TOWARD-LESSER
    CALL "World-EnsureChunk" USING POS-CHUNK-X POS-CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        GOBACK
    END-IF
    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)

    *> Find the entity in the linked list
    SET PREV-ENTITY-PTR TO NULL
    SET ENTITY-PTR TO CHUNK-ENTITY-LIST
    PERFORM UNTIL ENTITY-PTR = NULL
        SET ADDRESS OF ENTITY-LIST TO ENTITY-PTR
        IF ENTITY-ID = LK-ENTITY-ID
            SET NEXT-ENTITY-PTR TO ENTITY-LIST-NEXT
            EXIT PERFORM
        END-IF
        SET PREV-ENTITY-PTR TO ENTITY-PTR
        SET ENTITY-PTR TO ENTITY-LIST-NEXT
    END-PERFORM

    IF ENTITY-PTR = NULL
        GOBACK
    END-IF

    *> Deallocate the entity
    FREE ENTITY-LIST

    *> Remove the entity from the linked list
    IF PREV-ENTITY-PTR = NULL
        SET CHUNK-ENTITY-LIST TO NEXT-ENTITY-PTR
    ELSE
        SET ADDRESS OF ENTITY-LIST TO PREV-ENTITY-PTR
        SET ENTITY-LIST-NEXT TO NEXT-ENTITY-PTR
    END-IF

    GOBACK.

END PROGRAM World-RemoveEntity.

*> --- World-DropItem ---
*> Utility program to drop an item entity at a given position.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-DropItem.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ENTITY-TYPE-ITEM         BINARY-LONG                 VALUE -1.
    01 ENTITY.
        COPY DD-ENTITY.
LINKAGE SECTION.
    01 LK-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
    01 LK-POSITION.
        02 LK-X                 FLOAT-LONG.
        02 LK-Y                 FLOAT-LONG.
        02 LK-Z                 FLOAT-LONG.
    01 LK-VELOCITY.
        02 LK-VELOCITY-X        FLOAT-LONG.
        02 LK-VELOCITY-Y        FLOAT-LONG.
        02 LK-VELOCITY-Z        FLOAT-LONG.
    *> Index of the player that dropped the item, if applicable
    01 LK-THROWER               BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-SLOT LK-POSITION LK-VELOCITY OPTIONAL LK-THROWER.
    IF LK-SLOT-COUNT < 1
        GOBACK
    END-IF

    IF ENTITY-TYPE-ITEM < 0
        CALL "Registries-Get-EntryId" USING "minecraft:entity_type" "minecraft:item" ENTITY-TYPE-ITEM
    END-IF

    INITIALIZE ENTITY
    MOVE ENTITY-TYPE-ITEM TO ENTITY-TYPE
    MOVE LK-POSITION TO ENTITY-POSITION
    MOVE LK-VELOCITY TO ENTITY-VELOCITY
    MOVE LK-SLOT TO ENTITY-ITEM-SLOT

    *> TODO Unset "no gravity" once the server computes the same motion as the client
    MOVE 1 TO ENTITY-NO-GRAVITY

    *> By default, items can be picked up after 0.5 seconds. Items dropped by players have a 2-second delay instead.
    IF LK-THROWER IS OMITTED
        MOVE 10 TO ENTITY-ITEM-PICKUP-DELAY
    ELSE
        MOVE 40 TO ENTITY-ITEM-PICKUP-DELAY
    END-IF

    CALL "World-SpawnEntity" USING ENTITY

    GOBACK.

END PROGRAM World-DropItem.

*> --- World-DropItem-FromPlayer ---
*> Utility program to drop an item entity originating from a player (at their head position).
IDENTIFICATION DIVISION.
PROGRAM-ID. World-DropItem-FromPlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    78 DEG2RAD                  VALUE 0.017453292.
    78 TAU                      VALUE 6.283185307.
    01 YAW-RAD                  FLOAT-LONG.
    01 PITCH-RAD                FLOAT-LONG.
    01 RANDOM-ANGLE             FLOAT-LONG.
    01 RANDOM-MAGNITUDE         FLOAT-LONG.
    01 ITEM-POS.
        02 ITEM-X               FLOAT-LONG.
        02 ITEM-Y               FLOAT-LONG.
        02 ITEM-Z               FLOAT-LONG.
    01 ITEM-VELOCITY.
        02 ITEM-VELOCITY-X      FLOAT-LONG.
        02 ITEM-VELOCITY-Y      FLOAT-LONG.
        02 ITEM-VELOCITY-Z      FLOAT-LONG.
LINKAGE SECTION.
    01 LK-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
    01 LK-THROWER               BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-SLOT LK-THROWER.
    MOVE PLAYER-POSITION(LK-THROWER) TO ITEM-POS
    IF PLAYER-SNEAKING(LK-THROWER) = 0
        *> Regular eye height 1.62
        COMPUTE ITEM-Y = ITEM-Y + 1.62 - 0.3
    ELSE
        *> Sneaking eye height 1.27
        COMPUTE ITEM-Y = ITEM-Y + 1.27 - 0.3
    END-IF

    COMPUTE YAW-RAD = PLAYER-YAW(LK-THROWER) * DEG2RAD
    COMPUTE PITCH-RAD = PLAYER-PITCH(LK-THROWER) * DEG2RAD

    COMPUTE RANDOM-ANGLE = FUNCTION RANDOM * TAU
    COMPUTE RANDOM-MAGNITUDE = FUNCTION RANDOM * 0.02

    *> Note: This matches Minecraft'S own calculation.
    COMPUTE ITEM-VELOCITY-X = -(FUNCTION SIN(YAW-RAD) * FUNCTION COS(PITCH-RAD)) * 0.3 + (FUNCTION COS(RANDOM-ANGLE) * RANDOM-MAGNITUDE)
    COMPUTE ITEM-VELOCITY-Z = (FUNCTION COS(YAW-RAD) * FUNCTION COS(PITCH-RAD)) * 0.3 + (FUNCTION SIN(RANDOM-ANGLE) * RANDOM-MAGNITUDE)
    COMPUTE ITEM-VELOCITY-Y = -(FUNCTION SIN(PITCH-RAD)) * 0.3 + 0.1 + (FUNCTION RANDOM + FUNCTION RANDOM) * 0.1

    CALL "World-DropItem" USING LK-SLOT ITEM-POS ITEM-VELOCITY LK-THROWER

    GOBACK.

END PROGRAM World-DropItem-FromPlayer.

*> --- World-DropItem-FromBlock ---
*> Utility program to drop an item entity originating from a block position, with the appropriate random offset and
*> initial velocity.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-DropItem-FromBlock.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    01 ITEM-POS.
        02 ITEM-X               FLOAT-LONG.
        02 ITEM-Y               FLOAT-LONG.
        02 ITEM-Z               FLOAT-LONG.
    01 ITEM-VELOCITY.
        02 ITEM-VELOCITY-X      FLOAT-LONG.
        02 ITEM-VELOCITY-Y      FLOAT-LONG.
        02 ITEM-VELOCITY-Z      FLOAT-LONG.
LINKAGE SECTION.
    01 LK-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
    01 LK-BLOCK-POSITION.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.

PROCEDURE DIVISION USING LK-SLOT LK-BLOCK-POSITION.
    COMPUTE ITEM-X = LK-X + 0.125 + (0.75 * FUNCTION RANDOM)
    COMPUTE ITEM-Z = LK-Z + 0.125 + (0.75 * FUNCTION RANDOM)
    COMPUTE ITEM-Y = LK-Y + (0.75 * FUNCTION RANDOM)

    COMPUTE ITEM-VELOCITY-X = (-0.5 + FUNCTION RANDOM) * 0.05
    COMPUTE ITEM-VELOCITY-Z = (-0.5 + FUNCTION RANDOM) * 0.05
    COMPUTE ITEM-VELOCITY-Y = 0.2

    CALL "World-DropItem" USING LK-SLOT ITEM-POS ITEM-VELOCITY OMITTED

    GOBACK.

END PROGRAM World-DropItem-FromBlock.
