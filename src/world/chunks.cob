*> --- World-AllocateChunk ---
*> Find a free chunk slot. If a chunk with the given coordinates is present, it is freed first.
*> All blocks in the chunk are set to air, and the coordinates are set. The chunk is, however, not marked as present.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-AllocateChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    01 BIOME-ID                 BINARY-LONG UNSIGNED.
    01 SECTION-INDEX            BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CHUNK-X               BINARY-LONG.
    01 LK-CHUNK-Z               BINARY-LONG.
    01 LK-CHUNK-INDEX           BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX.
    *> Check if the chunk is already present and free it if needed
    PERFORM VARYING LK-CHUNK-INDEX FROM 1 BY 1 UNTIL LK-CHUNK-INDEX > WORLD-CHUNK-COUNT
        SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(LK-CHUNK-INDEX)
        IF LK-CHUNK-X = CHUNK-X AND LK-CHUNK-Z = CHUNK-Z
            CALL "World-FreeChunk" USING LK-CHUNK-INDEX
            EXIT PERFORM
        END-IF
    END-PERFORM

    *> Allocate a new chunk
    COMPUTE LK-CHUNK-INDEX = WORLD-CHUNK-COUNT + 1
    IF LK-CHUNK-INDEX >= WORLD-CHUNK-CAPACITY
        DISPLAY "Warning: Failed to allocate chunk (no capacity)"
        MOVE 0 TO LK-CHUNK-INDEX
        GOBACK
    END-IF
    ALLOCATE CHUNK
    SET WORLD-CHUNK-POINTER(LK-CHUNK-INDEX) TO ADDRESS OF CHUNK

    INITIALIZE CHUNK
    MOVE LK-CHUNK-X TO CHUNK-X
    MOVE LK-CHUNK-Z TO CHUNK-Z
    INITIALIZE CHUNK-BLOCK-ENTITY-IDS REPLACING NUMERIC BY -1

    *> Set all sections to the plains biome
    CALL "Registries-Get-EntryId" USING "minecraft:worldgen/biome" "minecraft:plains" BIOME-ID
    PERFORM VARYING SECTION-INDEX FROM 1 BY 1 UNTIL SECTION-INDEX > CHUNK-SECTION-COUNT
        INITIALIZE CHUNK-SECTION-BIOMES(SECTION-INDEX) REPLACING NUMERIC BY BIOME-ID
    END-PERFORM

    ADD 1 TO WORLD-CHUNK-COUNT

    GOBACK.

END PROGRAM World-AllocateChunk.

*> --- World-FreeChunk ---
*> Free a chunk slot, properly deallocating any dynamically allocated memory.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-FreeChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    COPY DD-CHUNK-ENTITY.
    01 ENTITY-PTR               POINTER.
LINKAGE SECTION.
    01 LK-CHUNK-INDEX           BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-INDEX.
    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(LK-CHUNK-INDEX)

    *> free all entities
    SET ENTITY-PTR TO CHUNK-ENTITY-LIST
    PERFORM UNTIL ENTITY-PTR = NULL
        SET ADDRESS OF ENTITY-LIST TO ENTITY-PTR
        SET ENTITY-PTR TO ENTITY-LIST-NEXT
        FREE ENTITY-LIST
    END-PERFORM

    *> free the chunk itself
    FREE WORLD-CHUNK-POINTER(LK-CHUNK-INDEX)

    *> move the last slot into the freed slot
    SET WORLD-CHUNK-POINTER(LK-CHUNK-INDEX) TO WORLD-CHUNK-POINTER(WORLD-CHUNK-COUNT)
    SUBTRACT 1 FROM WORLD-CHUNK-COUNT

    GOBACK.

END PROGRAM World-FreeChunk.

*> --- World-FindChunkIndex ---
*> Find a chunk that is present and has the given coordinates.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-FindChunkIndex.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
LINKAGE SECTION.
    01 LK-CHUNK-X               BINARY-LONG.
    01 LK-CHUNK-Z               BINARY-LONG.
    01 LK-CHUNK-INDEX           BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX.
    PERFORM VARYING LK-CHUNK-INDEX FROM 1 BY 1 UNTIL LK-CHUNK-INDEX > WORLD-CHUNK-COUNT
        SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(LK-CHUNK-INDEX)
        IF LK-CHUNK-X = CHUNK-X AND LK-CHUNK-Z = CHUNK-Z
            GOBACK
        END-IF
    END-PERFORM
    MOVE 0 TO LK-CHUNK-INDEX
    GOBACK.

END PROGRAM World-FindChunkIndex.

*> --- World-GenerateChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-GenerateChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
LOCAL-STORAGE SECTION.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 SECTION-INDEX            BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX              BINARY-LONG UNSIGNED.
    01 TEMP-INT32               BINARY-LONG.
LINKAGE SECTION.
    01 LK-CHUNK-X               BINARY-LONG.
    01 LK-CHUNK-Z               BINARY-LONG.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z.
    CALL "World-AllocateChunk" USING LK-CHUNK-X LK-CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        GOBACK
    END-IF

    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)

    *> turn all blocks with Y <= 63 (= the bottom 128 blocks = the bottom 8 sections) into stone
    CALL "Blocks-Get-DefaultStateId" USING "minecraft:stone" TEMP-INT32
    PERFORM VARYING SECTION-INDEX FROM 1 BY 1 UNTIL SECTION-INDEX > 8
        PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > 4096
            MOVE TEMP-INT32 TO CHUNK-SECTION-BLOCK(SECTION-INDEX, BLOCK-INDEX)
        END-PERFORM
        MOVE 4096 TO CHUNK-SECTION-NON-AIR(SECTION-INDEX)
    END-PERFORM

    *> turn all blocks with Y = 63 (i.e., the top 16x16 blocks) into grass
    CALL "Blocks-Get-DefaultStateId" USING "minecraft:grass_block" TEMP-INT32
    MOVE 8 TO SECTION-INDEX
    COMPUTE BLOCK-INDEX = 4096 - 256 + 1
    PERFORM 256 TIMES
        MOVE TEMP-INT32 TO CHUNK-SECTION-BLOCK(SECTION-INDEX, BLOCK-INDEX)
        *> Note: No need to increment CHUNK-SECTION-NON-AIR, as the section is already full
        ADD 1 TO BLOCK-INDEX
    END-PERFORM

    *> chunk needs to be saved (blocks only)
    MOVE 1 TO CHUNK-DIRTY-BLOCKS
    MOVE 0 TO CHUNK-DIRTY-ENTITIES

    GOBACK.

END PROGRAM World-GenerateChunk.

*> --- World-EnsureChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-EnsureChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    01 IO-FAILURE               BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-CHUNK-X               BINARY-LONG.
    01 LK-CHUNK-Z               BINARY-LONG.
    01 LK-CHUNK-INDEX           BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX.
    *> attempt to find the chunk
    CALL "World-FindChunkIndex" USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX
    IF LK-CHUNK-INDEX > 0
        GOBACK
    END-IF
    *> not found, load or generate
    CALL "World-LoadChunk" USING LK-CHUNK-X LK-CHUNK-Z IO-FAILURE
    IF IO-FAILURE NOT = 0
        MOVE 0 TO IO-FAILURE
        CALL "World-GenerateChunk" USING LK-CHUNK-X LK-CHUNK-Z
    END-IF
    *> find the chunk again
    CALL "World-FindChunkIndex" USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX
    GOBACK.

END PROGRAM World-EnsureChunk.

*> --- World-EnsureSpawnChunks ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-EnsureSpawnChunks.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    01 X-START                  BINARY-LONG.
    01 X-END                    BINARY-LONG.
    01 Z-START                  BINARY-LONG.
    01 Z-END                    BINARY-LONG.
    01 X-POS                    BINARY-LONG.
    01 Z-POS                    BINARY-LONG.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-SAVE-REQUIRED         BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-SAVE-REQUIRED.
    MOVE 0 TO LK-SAVE-REQUIRED

    COMPUTE X-START ROUNDED MODE IS TOWARD-LESSER = (WORLD-SPAWN-X / 16) - 1
    COMPUTE Z-START ROUNDED MODE IS TOWARD-LESSER = (WORLD-SPAWN-Z / 16) - 1
    COMPUTE X-END = X-START + 2
    COMPUTE Z-END = Z-START + 2

    PERFORM VARYING X-POS FROM X-START BY 1 UNTIL X-POS > X-END
        PERFORM VARYING Z-POS FROM Z-START BY 1 UNTIL Z-POS > Z-END
            CALL "World-EnsureChunk" USING X-POS Z-POS CHUNK-INDEX
            IF CHUNK-INDEX = 0
                GOBACK
            END-IF
            SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)
            IF CHUNK-DIRTY-BLOCKS > 0 OR CHUNK-DIRTY-ENTITIES > 0
                MOVE 1 TO LK-SAVE-REQUIRED
            END-IF
        END-PERFORM
    END-PERFORM

    GOBACK.

END PROGRAM World-EnsureSpawnChunks.

*> --- World-UnloadChunks ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-UnloadChunks.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 CHUNK-BLOCK-X            BINARY-LONG.
    01 CHUNK-BLOCK-Z            BINARY-LONG.
    01 SPAWN-X-START            BINARY-LONG.
    01 SPAWN-X-END              BINARY-LONG.
    01 SPAWN-Z-START            BINARY-LONG.
    01 SPAWN-Z-END              BINARY-LONG.
    01 MIN-DISTANCE             BINARY-LONG.
    01 PLAYER-INDEX             BINARY-LONG UNSIGNED.
    *> World data
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    *> Player data
    COPY DD-PLAYERS.
    COPY DD-SERVER-PROPERTIES.
LINKAGE SECTION.
    01 LK-VIEW-DISTANCE         BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-VIEW-DISTANCE LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> Do not unload spawn chunks
    COMPUTE SPAWN-X-START ROUNDED MODE IS TOWARD-LESSER = (WORLD-SPAWN-X / 16) - 1
    COMPUTE SPAWN-X-END = SPAWN-X-START + 2
    COMPUTE SPAWN-Z-START ROUNDED MODE IS TOWARD-LESSER = (WORLD-SPAWN-Z / 16) - 1
    COMPUTE SPAWN-Z-END = SPAWN-Z-START + 2

    PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > WORLD-CHUNK-COUNT
        SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)
        IF (CHUNK-X < SPAWN-X-START OR > SPAWN-X-END) OR (CHUNK-Z < SPAWN-Z-START OR > SPAWN-Z-END)
            COMPUTE CHUNK-BLOCK-X = CHUNK-X * 16 + 8
            COMPUTE CHUNK-BLOCK-Z = CHUNK-Z * 16 + 8
            *> Compute the minimum distance to any player on any axis
            MOVE 1000000 TO MIN-DISTANCE
            PERFORM VARYING PLAYER-INDEX FROM 1 BY 1 UNTIL PLAYER-INDEX > MAX-PLAYERS
                IF PLAYER-CLIENT(PLAYER-INDEX) > 0
                    COMPUTE MIN-DISTANCE = FUNCTION MIN(MIN-DISTANCE, FUNCTION ABS(CHUNK-BLOCK-X - PLAYER-X(PLAYER-INDEX)))
                    COMPUTE MIN-DISTANCE = FUNCTION MIN(MIN-DISTANCE, FUNCTION ABS(CHUNK-BLOCK-Z - PLAYER-Z(PLAYER-INDEX)))
                END-IF
            END-PERFORM
            *> If the chunk is outside the view distance + 2 (for tolerance against thrashing), unload it
            COMPUTE MIN-DISTANCE = MIN-DISTANCE / 16 - LK-VIEW-DISTANCE
            IF MIN-DISTANCE > 2
                IF CHUNK-DIRTY-BLOCKS > 0 OR CHUNK-DIRTY-ENTITIES > 0
                    CALL "World-SaveChunk" USING CHUNK-INDEX LK-FAILURE
                    IF LK-FAILURE > 0
                        MOVE 1 TO LK-FAILURE
                        GOBACK
                    END-IF
                END-IF
                *> free the memory
                CALL "World-FreeChunk" USING CHUNK-INDEX
                *> since the chunk was removed, the next chunk will be at the same index
                SUBTRACT 1 FROM CHUNK-INDEX
            END-IF
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM World-UnloadChunks.
