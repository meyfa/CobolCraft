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
    COPY DD-SERVER-PROPERTIES.
    01 PLAYER-INDEX             BINARY-LONG UNSIGNED.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 ENTITY-PTR               POINTER.
    *> Pre-computed player bounding boxes
    01 PLAYER-AABBS.
        02 PLAYER-AABB OCCURS PLAYER-CAPACITY TIMES.
            COPY DD-AABB REPLACING LEADING ==PREFIX== BY ==PLAYER==.
    01 ENTITY-TICK-PTR          PROGRAM-POINTER.
    01 REMOVE-ENTITY            BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION.
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
    CALL "GetCallback-EntityTick" USING ENTITY-TYPE ENTITY-TICK-PTR
    IF ENTITY-TICK-PTR = NULL
        EXIT PARAGRAPH
    END-IF

    CALL ENTITY-TICK-PTR USING ENTITY-LIST-ENTITY PLAYER-AABBS REMOVE-ENTITY
    IF REMOVE-ENTITY > 0
        CALL "World-RemoveEntity" USING ENTITY-LIST-ENTITY
        EXIT PARAGRAPH
    END-IF

    *> Ensure the entity is in the correct chunk
    CALL "World-UpdateEntityChunk" USING ENTITY-LIST-ENTITY ENTITY-CHUNK-X ENTITY-CHUNK-Z
    .

END PROGRAM World-Tick.

*> --- World-FindSpawnLocation ---
*> This program is used to find a suitable spawn location for a new player.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-FindSpawnLocation.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> constants
    78 SPAWN-RADIUS             VALUE 10.
    78 SPAWN-ATTEMPTS           VALUE 100.
    *> initialized once
    01 INIT-DONE                BINARY-CHAR UNSIGNED.
    01 RANDOM-VALUE             FLOAT-LONG.
    01 BLOCK-ID-AIR             BINARY-LONG.
    01 BLOCK-ID-CAVE-AIR        BINARY-LONG.
    01 BLOCK-ID-VOID-AIR        BINARY-LONG.
    *> variables
    COPY DD-WORLD.
    01 BLOCK-POSITION.
        02 POS-X                BINARY-LONG.
        02 POS-Y                BINARY-LONG.
        02 POS-Z                BINARY-LONG.
    01 BLOCK-STATE              BINARY-LONG.
    01 BLOCK-ID                 BINARY-LONG.
    01 AIR-COUNT                BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                 FLOAT-LONG.
        02 LK-Y                 FLOAT-LONG.
        02 LK-Z                 FLOAT-LONG.

PROCEDURE DIVISION USING LK-POSITION.
    IF INIT-DONE = 0
        MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE(1:16)) TO RANDOM-VALUE

        CALL "Registries-Lookup" USING "minecraft:block" "minecraft:air" BLOCK-ID-AIR
        CALL "Registries-Lookup" USING "minecraft:block" "minecraft:cave_air" BLOCK-ID-CAVE-AIR
        CALL "Registries-Lookup" USING "minecraft:block" "minecraft:void_air" BLOCK-ID-VOID-AIR
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
            CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-STATE
            CALL "Blocks-ForStateId" USING BLOCK-STATE BLOCK-ID
            EVALUATE BLOCK-ID
                WHEN = BLOCK-ID-AIR OR = BLOCK-ID-CAVE-AIR OR = BLOCK-ID-VOID-AIR
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
