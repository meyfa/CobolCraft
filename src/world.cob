*> --- World-FindChunkIndex ---
*> Find a chunk that is present and has the given coordinates.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-FindChunkIndex.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> World data
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-CHUNK-X           BINARY-LONG.
    01 LK-CHUNK-Z           BINARY-LONG.
    01 LK-CHUNK-INDEX       BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX.
    PERFORM VARYING LK-CHUNK-INDEX FROM 1 BY 1 UNTIL LK-CHUNK-INDEX > WORLD-CHUNK-COUNT
        IF WORLD-CHUNK-PRESENT(LK-CHUNK-INDEX) > 0 AND LK-CHUNK-X = WORLD-CHUNK-X(LK-CHUNK-INDEX) AND LK-CHUNK-Z = WORLD-CHUNK-Z(LK-CHUNK-INDEX)
            EXIT PERFORM
        END-IF
    END-PERFORM
    IF LK-CHUNK-INDEX > WORLD-CHUNK-COUNT
        MOVE 0 TO LK-CHUNK-INDEX
    END-IF
    GOBACK.

END PROGRAM World-FindChunkIndex.

*> --- World-AllocateChunk ---
*> Find a free chunk slot. If a chunk with the given coordinates is present, it is freed first.
*> All blocks in the chunk are set to air.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-AllocateChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 SECTION-INDEX        BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
    *> World data
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-CHUNK-X           BINARY-LONG.
    01 LK-CHUNK-Z           BINARY-LONG.
    01 LK-CHUNK-INDEX       BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX.
    PERFORM VARYING LK-CHUNK-INDEX FROM 1 BY 1 UNTIL LK-CHUNK-INDEX > WORLD-CHUNK-COUNT
        IF WORLD-CHUNK-PRESENT(LK-CHUNK-INDEX) = 0 OR (LK-CHUNK-X = WORLD-CHUNK-X(LK-CHUNK-INDEX) AND LK-CHUNK-Z = WORLD-CHUNK-Z(LK-CHUNK-INDEX))
            EXIT PERFORM
        END-IF
    END-PERFORM
    IF LK-CHUNK-INDEX > WORLD-CHUNK-COUNT
        MOVE 0 TO LK-CHUNK-INDEX
        GOBACK
    END-IF
    MOVE 0 TO WORLD-CHUNK-PRESENT(LK-CHUNK-INDEX)
    MOVE 0 TO WORLD-CHUNK-DIRTY(LK-CHUNK-INDEX)
    PERFORM VARYING SECTION-INDEX FROM 1 BY 1 UNTIL SECTION-INDEX > WORLD-SECTION-COUNT
        MOVE 0 TO WORLD-SECTION-NON-AIR(LK-CHUNK-INDEX, SECTION-INDEX)
        PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > 4096
            MOVE 0 TO WORLD-BLOCK-ID(LK-CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX)
        END-PERFORM
    END-PERFORM
    GOBACK.

END PROGRAM World-AllocateChunk.

*> --- World-GenerateChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-GenerateChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> Constants
    01 C-MINECRAFT-AIR              PIC X(50) VALUE "minecraft:air".
    01 C-MINECRAFT-STONE            PIC X(50) VALUE "minecraft:stone".
    01 C-MINECRAFT-GRASS_BLOCK      PIC X(50) VALUE "minecraft:grass_block".
    *> World data
    COPY DD-WORLD.
LOCAL-STORAGE SECTION.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 SECTION-INDEX        BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
    01 TEMP-INT32           BINARY-LONG.
LINKAGE SECTION.
    01 LK-CHUNK-X           BINARY-LONG.
    01 LK-CHUNK-Z           BINARY-LONG.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z.
    CALL "World-AllocateChunk" USING LK-CHUNK-X LK-CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        *> TODO handle this case
        GOBACK
    END-IF

    MOVE LK-CHUNK-X TO WORLD-CHUNK-X(CHUNK-INDEX)
    MOVE LK-CHUNK-Z TO WORLD-CHUNK-Z(CHUNK-INDEX)

    *> turn all blocks with Y <= 63 (= the bottom 128 blocks = the bottom 8 sections) into stone
    CALL "Blocks-Get-DefaultStateId" USING C-MINECRAFT-STONE TEMP-INT32
    PERFORM VARYING SECTION-INDEX FROM 1 BY 1 UNTIL SECTION-INDEX > 8
        PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > 4096
            MOVE TEMP-INT32 TO WORLD-BLOCK-ID(CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX)
        END-PERFORM
        MOVE 4096 TO WORLD-SECTION-NON-AIR(CHUNK-INDEX, SECTION-INDEX)
    END-PERFORM

    *> turn all blocks with Y = 63 (i.e., the top 16x16 blocks) into grass
    CALL "Blocks-Get-DefaultStateId" USING C-MINECRAFT-GRASS_BLOCK TEMP-INT32
    MOVE 8 TO SECTION-INDEX
    COMPUTE BLOCK-INDEX = 4096 - 256 + 1
    PERFORM 256 TIMES
        MOVE TEMP-INT32 TO WORLD-BLOCK-ID(CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX)
        *> Note: No need to increment WORLD-SECTION-NON-AIR, as the section is already full
        ADD 1 TO BLOCK-INDEX
    END-PERFORM

    *> mark the chunk as present and dirty (i.e., needing to be saved)
    MOVE 1 TO WORLD-CHUNK-PRESENT(CHUNK-INDEX)
    MOVE 1 TO WORLD-CHUNK-DIRTY(CHUNK-INDEX)

    GOBACK.

END PROGRAM World-GenerateChunk.

*> --- World-ChunkFileName ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-ChunkFileName.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 DISPLAY-UINT         PIC 9(10).
LOCAL-STORAGE SECTION.
    01 STR-POS              BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CHUNK-X           BINARY-LONG.
    01 LK-CHUNK-Z           BINARY-LONG.
    01 LK-CHUNK-FILE-NAME   PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-FILE-NAME.
    *> prefix
    MOVE "save/overworld/chunk_" TO LK-CHUNK-FILE-NAME
    COMPUTE STR-POS = FUNCTION STORED-CHAR-LENGTH(LK-CHUNK-FILE-NAME) + 1
    *> X
    IF LK-CHUNK-X < 0
        MOVE "-" TO LK-CHUNK-FILE-NAME(STR-POS:1)
        ADD 1 TO STR-POS
    END-IF
    MOVE FUNCTION ABS(LK-CHUNK-X) TO DISPLAY-UINT
    MOVE DISPLAY-UINT TO LK-CHUNK-FILE-NAME(STR-POS:)
    COMPUTE STR-POS = FUNCTION STORED-CHAR-LENGTH(LK-CHUNK-FILE-NAME) + 1
    *> delimiter
    MOVE "_" TO LK-CHUNK-FILE-NAME(STR-POS:1)
    ADD 1 TO STR-POS
    *> Z
    IF LK-CHUNK-Z < 0
        MOVE "-" TO LK-CHUNK-FILE-NAME(STR-POS:1)
        ADD 1 TO STR-POS
    END-IF
    MOVE FUNCTION ABS(LK-CHUNK-Z) TO DISPLAY-UINT
    MOVE DISPLAY-UINT TO LK-CHUNK-FILE-NAME(STR-POS:)
    COMPUTE STR-POS = FUNCTION STORED-CHAR-LENGTH(LK-CHUNK-FILE-NAME) + 1
    *> suffix
    MOVE ".dat" TO LK-CHUNK-FILE-NAME(STR-POS:)
    GOBACK.

END PROGRAM World-ChunkFileName.

*> --- World-SaveChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SaveChunk.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT OPTIONAL FD-CHUNK-FILE-OUT
        ASSIGN TO CHUNK-FILE-NAME
        ORGANIZATION IS SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD FD-CHUNK-FILE-OUT.
    COPY DD-CHUNK-FILE.
WORKING-STORAGE SECTION.
    *> File name
    01 CHUNK-FILE-NAME      PIC X(255).
    *> Temporary variables
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 SECTION-INDEX        BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
    01 FORCE-FIRST-SECTION  BINARY-CHAR UNSIGNED.
    01 CURRENT-BLOCK-ID     BINARY-LONG UNSIGNED.
    01 PALETTE-VALUE        BINARY-SHORT UNSIGNED.
    01 PALETTE-INDEX        BINARY-LONG UNSIGNED.
    *> World data
    COPY DD-WORLD.
LOCAL-STORAGE SECTION.
    *> A map of block state indices to palette indices
    78 BLOCK-PALETTE-LENGTH VALUE 100000.
    01 BLOCK-PALETTE-ITEM OCCURS BLOCK-PALETTE-LENGTH TIMES.
        02 BLOCK-PALETTE-INDEX  BINARY-SHORT UNSIGNED.
LINKAGE SECTION.
    01 LK-CHUNK-X           BINARY-LONG.
    01 LK-CHUNK-Z           BINARY-LONG.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    CALL "World-FindChunkIndex" USING LK-CHUNK-X LK-CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> open the file
    CALL "World-ChunkFileName" USING LK-CHUNK-X LK-CHUNK-Z CHUNK-FILE-NAME
    OPEN OUTPUT FD-CHUNK-FILE-OUT

    *> store data applicable to the entire chunk
    MOVE LK-CHUNK-X TO CHUNK-X
    MOVE LK-CHUNK-Z TO CHUNK-Z
    MOVE -4 TO CHUNK-Y

    *> count non-air sections
    MOVE 0 TO CHUNK-SECTION-COUNT
    PERFORM VARYING SECTION-INDEX FROM 1 BY 1 UNTIL SECTION-INDEX > WORLD-SECTION-COUNT
        IF WORLD-SECTION-NON-AIR(CHUNK-INDEX, SECTION-INDEX) > 0
            ADD 1 TO CHUNK-SECTION-COUNT
        END-IF
    END-PERFORM

    *> need to write at least one section
    MOVE 0 TO FORCE-FIRST-SECTION
    IF CHUNK-SECTION-COUNT = 0
        ADD 1 TO CHUNK-SECTION-COUNT
        MOVE 1 TO FORCE-FIRST-SECTION
    END-IF

    *> write the chunk sections
    PERFORM VARYING SECTION-INDEX FROM 1 BY 1 UNTIL SECTION-INDEX > WORLD-SECTION-COUNT
        IF WORLD-SECTION-NON-AIR(CHUNK-INDEX, SECTION-INDEX) > 0 OR (SECTION-INDEX = 1 AND FORCE-FIRST-SECTION > 0)
            MOVE 0 TO CHUNK-PALETTE-LENGTH

            COMPUTE CHUNK-SECTION-Y = SECTION-INDEX - 1 + CHUNK-Y

            PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > 4096
                *> If the block is not in the palette, add it
                MOVE WORLD-BLOCK-ID(CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX) TO CURRENT-BLOCK-ID
                MOVE BLOCK-PALETTE-INDEX(CURRENT-BLOCK-ID + 1) TO PALETTE-VALUE
                IF PALETTE-VALUE = 0
                    ADD 1 TO CHUNK-PALETTE-LENGTH
                    MOVE CHUNK-PALETTE-LENGTH TO PALETTE-VALUE
                    MOVE PALETTE-VALUE TO BLOCK-PALETTE-INDEX(CURRENT-BLOCK-ID + 1)
                    CALL "Blocks-Get-StateDescription" USING CURRENT-BLOCK-ID CHUNK-PALETTE-ENTRY(CHUNK-PALETTE-LENGTH)
                END-IF
                *> Store the palette index
                COMPUTE CHUNK-SECTION-DATA(BLOCK-INDEX) = PALETTE-VALUE - 1
            END-PERFORM

            WRITE CHUNK-FILE-RECORD

            *> Reset the palette for the next section. Doing this for the specific IDs used is much faster than
            *> resetting the entire palette.
            IF SECTION-INDEX < WORLD-SECTION-COUNT
                PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > 4096
                    MOVE 0 TO BLOCK-PALETTE-INDEX(WORLD-BLOCK-ID(CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX) + 1)
                END-PERFORM
            END-IF
        END-IF
    END-PERFORM

    *> finish
    CLOSE FD-CHUNK-FILE-OUT

    MOVE 0 TO WORLD-CHUNK-DIRTY(CHUNK-INDEX)

    GOBACK.

END PROGRAM World-SaveChunk.

*> --- World-LoadChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-LoadChunk.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT OPTIONAL FD-CHUNK-FILE-IN
        ASSIGN TO CHUNK-FILE-NAME
        ORGANIZATION IS SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD FD-CHUNK-FILE-IN.
    COPY DD-CHUNK-FILE.
WORKING-STORAGE SECTION.
    *> File name
    01 CHUNK-FILE-NAME      PIC X(255).
    *> A map of palette indices to block state IDs
    01 BLOCK-STATE-IDS      BINARY-SHORT UNSIGNED OCCURS 4096 TIMES.
    *> Temporary variables
    01 PALETTE-INDEX        BINARY-SHORT UNSIGNED.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 RECORD-INDEX         BINARY-LONG UNSIGNED.
    01 SECTION-INDEX        BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
    01 CURRENT-BLOCK-ID     BINARY-LONG UNSIGNED.
    *> World data
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-CHUNK-X           BINARY-LONG.
    01 LK-CHUNK-Z           BINARY-LONG.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> open the file
    CALL "World-ChunkFileName" USING LK-CHUNK-X LK-CHUNK-Z CHUNK-FILE-NAME
    OPEN INPUT FD-CHUNK-FILE-IN

    *> read the chunk sections
    MOVE 0 TO CHUNK-INDEX
    MOVE 1 TO RECORD-INDEX
    PERFORM UNTIL RECORD-INDEX = 0
        READ FD-CHUNK-FILE-IN
        AT END
            MOVE 0 TO RECORD-INDEX
        NOT AT END
            *> For the first section, read the data that applies to the entire chunk, and find a chunk slot in memory.
            IF RECORD-INDEX = 1
                CALL "World-AllocateChunk" USING LK-CHUNK-X LK-CHUNK-Z CHUNK-INDEX
                IF CHUNK-INDEX = 0
                    MOVE 1 TO LK-FAILURE
                    CLOSE FD-CHUNK-FILE-IN
                    GOBACK
                END-IF
                MOVE CHUNK-X TO WORLD-CHUNK-X(CHUNK-INDEX)
                MOVE CHUNK-Z TO WORLD-CHUNK-Z(CHUNK-INDEX)
            END-IF

            *> load the block palette
            PERFORM VARYING PALETTE-INDEX FROM 1 BY 1 UNTIL PALETTE-INDEX > CHUNK-PALETTE-LENGTH
                CALL "Blocks-Get-StateId" USING CHUNK-PALETTE-ENTRY(PALETTE-INDEX) BLOCK-STATE-IDS(PALETTE-INDEX)
            END-PERFORM

            *> load the section data
            COMPUTE SECTION-INDEX = CHUNK-SECTION-Y - CHUNK-Y + 1
            PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > 4096
                MOVE BLOCK-STATE-IDS(CHUNK-SECTION-DATA(BLOCK-INDEX) + 1) TO CURRENT-BLOCK-ID
                IF CURRENT-BLOCK-ID > 0
                    MOVE CURRENT-BLOCK-ID TO WORLD-BLOCK-ID(CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX)
                    ADD 1 TO WORLD-SECTION-NON-AIR(CHUNK-INDEX, SECTION-INDEX)
                END-IF
            END-PERFORM

            ADD 1 TO RECORD-INDEX
        END-READ
    END-PERFORM

    *> finish
    CLOSE FD-CHUNK-FILE-IN

    *> check for failure - if CHUNK-INDEX is 0, there was no initial section
    IF CHUNK-INDEX = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> mark the chunk as present and clean (i.e., not needing to be saved)
    MOVE 1 TO WORLD-CHUNK-PRESENT(CHUNK-INDEX)
    MOVE 0 TO WORLD-CHUNK-DIRTY(CHUNK-INDEX)

    GOBACK.

END PROGRAM World-LoadChunk.

*> --- World-EnsureChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-EnsureChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 IO-FAILURE           BINARY-CHAR UNSIGNED.
    *> World data
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-CHUNK-X           BINARY-LONG.
    01 LK-CHUNK-Z           BINARY-LONG.
    01 LK-CHUNK-INDEX       BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX.
    *> attempt to find the chunk
    CALL "World-FindChunkIndex" USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX
    IF LK-CHUNK-INDEX > 0
        GOBACK
    END-IF
    *> not found, load or generate
    CALL "World-LoadChunk" USING LK-CHUNK-X LK-CHUNK-Z IO-FAILURE
    IF IO-FAILURE NOT = 0
        DISPLAY "Generating chunk: " LK-CHUNK-X " " LK-CHUNK-Z
        MOVE 0 TO IO-FAILURE
        CALL "World-GenerateChunk" USING LK-CHUNK-X LK-CHUNK-Z
    END-IF
    *> find the chunk again
    CALL "World-FindChunkIndex" USING LK-CHUNK-X LK-CHUNK-Z LK-CHUNK-INDEX
    GOBACK.

END PROGRAM World-EnsureChunk.

*> --- World-UnloadChunks ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-UnloadChunks.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 CHUNK-X              BINARY-LONG.
    01 CHUNK-Z              BINARY-LONG.
    01 CHUNK-BLOCK-X        BINARY-LONG.
    01 CHUNK-BLOCK-Z        BINARY-LONG.
    01 MAX-DISTANCE         BINARY-LONG.
    01 PLAYER-INDEX         BINARY-LONG UNSIGNED.
    *> World data
    COPY DD-WORLD.
    *> Player data
    COPY DD-PLAYERS.
LINKAGE SECTION.
    01 LK-VIEW-DISTANCE     BINARY-LONG UNSIGNED.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-VIEW-DISTANCE LK-FAILURE.
    MOVE 0 TO LK-FAILURE
    PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > WORLD-CHUNK-COUNT
        IF WORLD-CHUNK-PRESENT(CHUNK-INDEX) > 0
            MOVE WORLD-CHUNK-X(CHUNK-INDEX) TO CHUNK-X
            MOVE WORLD-CHUNK-Z(CHUNK-INDEX) TO CHUNK-Z
            COMPUTE CHUNK-BLOCK-X = CHUNK-X * 16 + 8
            COMPUTE CHUNK-BLOCK-Z = CHUNK-Z * 16 + 8
            *> Compute the maximum distance to any player on any axis
            MOVE 0 TO MAX-DISTANCE
            PERFORM VARYING PLAYER-INDEX FROM 1 BY 1 UNTIL PLAYER-INDEX > MAX-PLAYERS
                IF PLAYER-CLIENT(PLAYER-INDEX) > 0
                    COMPUTE MAX-DISTANCE = FUNCTION MAX(MAX-DISTANCE, FUNCTION ABS(CHUNK-BLOCK-X - PLAYER-X(PLAYER-INDEX)))
                    COMPUTE MAX-DISTANCE = FUNCTION MAX(MAX-DISTANCE, FUNCTION ABS(CHUNK-BLOCK-Z - PLAYER-Z(PLAYER-INDEX)))
                END-IF
            END-PERFORM
            *> If the chunk is outside the view distance + 2 (for tolerance against thrashing), unload it
            COMPUTE MAX-DISTANCE = MAX-DISTANCE / 16 - LK-VIEW-DISTANCE
            IF MAX-DISTANCE > 2
                IF WORLD-CHUNK-DIRTY(CHUNK-INDEX) > 0
                    CALL "World-SaveChunk" USING CHUNK-X CHUNK-Z LK-FAILURE
                END-IF
                MOVE 0 TO WORLD-CHUNK-PRESENT(CHUNK-INDEX)
            END-IF
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM World-UnloadChunks.

*> --- World-Save ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-Save.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT OPTIONAL FD-WORLD-FILE-OUT
        ASSIGN TO "save/world.dat"
        ORGANIZATION IS SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD FD-WORLD-FILE-OUT.
    COPY DD-WORLD-FILE.
WORKING-STORAGE SECTION.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 CHUNK-X              BINARY-LONG.
    01 CHUNK-Z              BINARY-LONG.
    *> World data
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> Create directories. Ignore errors, as they are likely to be caused by the directories already existing.
    CALL "CBL_CREATE_DIR" USING "save"
    CALL "CBL_CREATE_DIR" USING "save/overworld"

    *> Save world metadata
    OPEN OUTPUT FD-WORLD-FILE-OUT
    MOVE WORLD-AGE TO WORLD-FILE-AGE
    MOVE WORLD-TIME TO WORLD-FILE-TIME
    WRITE WORLD-FILE-RECORD
    CLOSE FD-WORLD-FILE-OUT

    *> Save dirty chunks
    PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > WORLD-CHUNK-COUNT
        IF WORLD-CHUNK-PRESENT(CHUNK-INDEX) > 0 AND WORLD-CHUNK-DIRTY(CHUNK-INDEX) > 0
            MOVE WORLD-CHUNK-X(CHUNK-INDEX) TO CHUNK-X
            MOVE WORLD-CHUNK-Z(CHUNK-INDEX) TO CHUNK-Z
            CALL "World-SaveChunk" USING CHUNK-X CHUNK-Z LK-FAILURE
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

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT OPTIONAL FD-WORLD-FILE-IN
        ASSIGN TO "save/world.dat"
        ORGANIZATION IS SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD FD-WORLD-FILE-IN.
    COPY DD-WORLD-FILE.
WORKING-STORAGE SECTION.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 CHUNK-X              BINARY-LONG.
    01 CHUNK-Z              BINARY-LONG.
    01 IO-FAILURE           BINARY-CHAR UNSIGNED.
    01 SAVE-REQUIRED        BINARY-CHAR UNSIGNED.
    *> World data
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    MOVE 0 TO SAVE-REQUIRED

    *> Load the world metadata
    OPEN INPUT FD-WORLD-FILE-IN
    READ FD-WORLD-FILE-IN
    AT END
        *> Use default values
        MOVE 0 TO WORLD-AGE
        MOVE 0 TO WORLD-TIME
        MOVE 1 TO SAVE-REQUIRED
    NOT AT END
        *> Use the loaded values
        MOVE WORLD-FILE-AGE TO WORLD-AGE
        MOVE WORLD-FILE-TIME TO WORLD-TIME
    END-READ
    CLOSE FD-WORLD-FILE-IN

    *> Mark all chunks as absent
    PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > WORLD-CHUNK-COUNT
        MOVE 0 TO WORLD-CHUNK-PRESENT(CHUNK-INDEX)
    END-PERFORM

    *> Load a 3x3 spawn area. If necessary, generate new chunks.
    PERFORM VARYING CHUNK-Z FROM -1 BY 1 UNTIL CHUNK-Z > 1
        PERFORM VARYING CHUNK-X FROM -1 BY 1 UNTIL CHUNK-X > 1
            CALL "World-LoadChunk" USING CHUNK-X CHUNK-Z IO-FAILURE
            IF IO-FAILURE NOT = 0
                DISPLAY "Generating chunk: " CHUNK-X " " CHUNK-Z
                MOVE 0 TO IO-FAILURE
                CALL "World-GenerateChunk" USING CHUNK-X CHUNK-Z
                MOVE 1 TO SAVE-REQUIRED
            END-IF
        END-PERFORM
    END-PERFORM

    *> Save the world if necessary
    IF SAVE-REQUIRED > 0
        CALL "World-Save" USING LK-FAILURE
    END-IF

    GOBACK.

END PROGRAM World-Load.

*> --- World-CheckBounds ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-CheckBounds.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.
    01 LK-RESULT            BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-POSITION LK-RESULT.
    IF LK-Y < -64 OR LK-Y > 319 THEN
        MOVE 1 TO LK-RESULT
    ELSE
        MOVE 0 TO LK-RESULT
    END-IF
    GOBACK.

END PROGRAM World-CheckBounds.

*> --- World-GetBlock ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-GetBlock.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LOCAL-STORAGE SECTION.
    01 CHUNK-X              BINARY-LONG.
    01 CHUNK-Z              BINARY-LONG.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 SECTION-INDEX        BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.
    01 LK-BLOCK-ID          BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-POSITION LK-BLOCK-ID.
    *> find the chunk
    DIVIDE LK-X BY 16 GIVING CHUNK-X ROUNDED MODE IS TOWARD-LESSER
    DIVIDE LK-Z BY 16 GIVING CHUNK-Z ROUNDED MODE IS TOWARD-LESSER
    CALL "World-FindChunkIndex" USING CHUNK-X CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        MOVE 0 TO LK-BLOCK-ID
        GOBACK
    END-IF
    *> compute the block index
    COMPUTE SECTION-INDEX = (LK-Y + 64) / 16 + 1
    COMPUTE BLOCK-INDEX = ((FUNCTION MOD(LK-Y + 64, 16)) * 16 + (FUNCTION MOD(LK-Z, 16))) * 16 + (FUNCTION MOD(LK-X, 16)) + 1
    MOVE WORLD-BLOCK-ID(CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX) TO LK-BLOCK-ID
    GOBACK.

END PROGRAM World-GetBlock.

*> --- World-SetBlock ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SetBlock.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CLIENT-STATES.
    COPY DD-CLIENTS.
    01 CHUNK-X              BINARY-LONG.
    01 CHUNK-Z              BINARY-LONG.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 SECTION-INDEX        BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
    01 CLIENT-ID            BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.
    01 LK-BLOCK-ID          BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-POSITION LK-BLOCK-ID.
    *> find the chunk, section, and block indices
    DIVIDE LK-X BY 16 GIVING CHUNK-X ROUNDED MODE IS TOWARD-LESSER
    DIVIDE LK-Z BY 16 GIVING CHUNK-Z ROUNDED MODE IS TOWARD-LESSER
    CALL "World-FindChunkIndex" USING CHUNK-X CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        GOBACK
    END-IF
    COMPUTE SECTION-INDEX = (LK-Y + 64) / 16 + 1
    COMPUTE BLOCK-INDEX = ((FUNCTION MOD(LK-Y + 64, 16)) * 16 + (FUNCTION MOD(LK-Z, 16))) * 16 + (FUNCTION MOD(LK-X, 16)) + 1

    *> skip if identical to the current block
    IF WORLD-BLOCK-ID(CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX) = LK-BLOCK-ID
        GOBACK
    END-IF

    *> check whether the block is becoming air or non-air
    EVALUATE TRUE
        WHEN LK-BLOCK-ID = 0
            SUBTRACT 1 FROM WORLD-SECTION-NON-AIR(CHUNK-INDEX, SECTION-INDEX)
        WHEN WORLD-BLOCK-ID(CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX) = 0
            ADD 1 TO WORLD-SECTION-NON-AIR(CHUNK-INDEX, SECTION-INDEX)
    END-EVALUATE

    *> set the block and mark the chunk as dirty
    MOVE LK-BLOCK-ID TO WORLD-BLOCK-ID(CHUNK-INDEX, SECTION-INDEX, BLOCK-INDEX)
    MOVE 1 TO WORLD-CHUNK-DIRTY(CHUNK-INDEX)

    *> notify clients
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
            CALL "SendPacket-BlockUpdate" USING CLIENT-ID LK-POSITION LK-BLOCK-ID
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM World-SetBlock.

*> --- World-GetAge ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-GetAge.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-AGE               BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-AGE.
    MOVE WORLD-AGE TO LK-AGE
    GOBACK.

END PROGRAM World-GetAge.

*> --- World-GetTime ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-GetTime.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-TIME              BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-TIME.
    MOVE WORLD-TIME TO LK-TIME
    GOBACK.

END PROGRAM World-GetTime.

*> --- World-SetTime ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SetTime.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-TIME              BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-TIME.
    MOVE LK-TIME TO WORLD-TIME
    GOBACK.

END PROGRAM World-SetTime.

*> --- World-UpdateAge ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-UpdateAge.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.

PROCEDURE DIVISION.
    ADD 1 TO WORLD-AGE
    COMPUTE WORLD-TIME = FUNCTION MOD(WORLD-TIME + 1, 24000)
    GOBACK.

END PROGRAM World-UpdateAge.
