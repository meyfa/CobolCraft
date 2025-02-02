*> --- World-FindChunkIndex ---
*> Find a chunk that is present and has the given coordinates.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-FindChunkIndex.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
LINKAGE SECTION.
    01 LK-CHUNK-X           BINARY-LONG.
    01 LK-CHUNK-Z           BINARY-LONG.
    01 LK-CHUNK-INDEX       BINARY-LONG UNSIGNED.

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

*> --- World-AllocateChunk ---
*> Find a free chunk slot. If a chunk with the given coordinates is present, it is freed first.
*> All blocks in the chunk are set to air, and the coordinates are set. The chunk is, however, not marked as present.
IDENTIFICATION DIVISION.
PROGRAM-ID. World-AllocateChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    01 BIOME-ID             BINARY-LONG UNSIGNED.
    01 SECTION-INDEX        BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CHUNK-X           BINARY-LONG.
    01 LK-CHUNK-Z           BINARY-LONG.
    01 LK-CHUNK-INDEX       BINARY-LONG UNSIGNED.

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
LINKAGE SECTION.
    01 LK-CHUNK-INDEX       BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-INDEX.
    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(LK-CHUNK-INDEX)

    FREE WORLD-CHUNK-POINTER(LK-CHUNK-INDEX)

    *> move the last slot into the freed slot
    SET WORLD-CHUNK-POINTER(LK-CHUNK-INDEX) TO WORLD-CHUNK-POINTER(WORLD-CHUNK-COUNT)
    SUBTRACT 1 FROM WORLD-CHUNK-COUNT

    GOBACK.

END PROGRAM World-FreeChunk.

*> --- World-GenerateChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-GenerateChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
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

    *> chunk needs to be saved
    MOVE 1 TO CHUNK-DIRTY

    GOBACK.

END PROGRAM World-GenerateChunk.

*> --- World-SaveChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SaveChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NBT-BUFFER               PIC X(1048576).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    01 CHUNK-SECTION-MIN-Y      BINARY-LONG             VALUE -4.
    *> Temporary variables
    01 TAG-NAME                 PIC X(256).
    01 NAME-LEN                 BINARY-LONG UNSIGNED.
    01 STR                      PIC X(256).
    01 STR-LEN                  BINARY-LONG UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT32                    BINARY-LONG.
    01 SECTION-INDEX            BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX              BINARY-LONG UNSIGNED.
    01 CURRENT-BLOCK-ID         BINARY-LONG UNSIGNED.
    01 BIOME-INDEX              BINARY-LONG UNSIGNED.
    01 CURRENT-BIOME-ID         BINARY-LONG UNSIGNED.
    01 PALETTE-LENGTH           BINARY-LONG UNSIGNED.
    01 PALETTE-BITS             BINARY-LONG UNSIGNED.
    01 PALETTE-BITS-POW         BINARY-LONG UNSIGNED.
    01 ENTRIES-PER-LONG         BINARY-LONG UNSIGNED.
    01 LONG-ARRAY-LENGTH        BINARY-LONG UNSIGNED.
    01 LONG-ARRAY-ENTRY         BINARY-LONG-LONG UNSIGNED.
    01 LONG-ARRAY-ENTRY-SIGNED  REDEFINES LONG-ARRAY-ENTRY BINARY-LONG-LONG.
    01 LONG-ARRAY-MULTIPLIER    BINARY-LONG-LONG UNSIGNED.
    COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==PALETTE-BLOCK==.
    01 PROPERTY-INDEX           BINARY-LONG UNSIGNED.
    01 ENTITY-COUNT             BINARY-LONG UNSIGNED.
    01 ENTITY-X                 BINARY-LONG.
    01 ENTITY-Y                 BINARY-LONG.
    01 ENTITY-Z                 BINARY-LONG.
    *> World data
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    *> A map of block state indices to palette indices
    78 PALETTE-CAPACITY VALUE 100000.
    01 PALETTE-INDICES.
        02 PALETTE-INDEX OCCURS PALETTE-CAPACITY TIMES BINARY-SHORT UNSIGNED.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-ENCODER.
LINKAGE SECTION.
    01 LK-CHUNK-INDEX           BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-INDEX LK-FAILURE.
    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(LK-CHUNK-INDEX)

    MOVE 0 TO LK-FAILURE

    *> start root tag
    MOVE 1 TO NBT-ENCODER-OFFSET
    CALL "NbtEncode-RootCompound" USING NBT-ENCODER-STATE NBT-BUFFER

    *> chunk position
    MOVE 4 TO NAME-LEN
    MOVE "xPos" TO TAG-NAME
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN CHUNK-X
    MOVE "zPos" TO TAG-NAME
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN CHUNK-Z
    MOVE "yPos" TO TAG-NAME
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN CHUNK-SECTION-MIN-Y

    *> start chunk sections
    MOVE "sections" TO TAG-NAME
    MOVE 8 TO NAME-LEN
    CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

    PERFORM VARYING SECTION-INDEX FROM 1 BY 1 UNTIL SECTION-INDEX > CHUNK-SECTION-COUNT
        *> only write sections that are not entirely air
        *> Note: The official format stores all sections, but it seems unnecessary, so we don't.
        IF CHUNK-SECTION-NON-AIR(SECTION-INDEX) > 0
            *> start section
            CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED

            *> section position
            MOVE "Y" TO TAG-NAME
            MOVE 1 TO NAME-LEN
            COMPUTE INT8 = SECTION-INDEX - 1 + CHUNK-SECTION-MIN-Y
            CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN INT8

            *> block states - palette and data
            MOVE "block_states" TO TAG-NAME
            MOVE 12 TO NAME-LEN
            CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

            *> palette
            MOVE "palette" TO TAG-NAME
            MOVE 7 TO NAME-LEN
            CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

            MOVE 0 TO PALETTE-LENGTH
            INITIALIZE PALETTE-INDICES

            PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > 4096
                *> If the block is not in the palette, add it
                MOVE CHUNK-SECTION-BLOCK(SECTION-INDEX, BLOCK-INDEX) TO CURRENT-BLOCK-ID
                IF PALETTE-INDEX(CURRENT-BLOCK-ID + 1) = 0
                    ADD 1 TO PALETTE-LENGTH
                    MOVE PALETTE-LENGTH TO PALETTE-INDEX(CURRENT-BLOCK-ID + 1)
                    CALL "Blocks-Get-StateDescription" USING CURRENT-BLOCK-ID PALETTE-BLOCK-DESCRIPTION

                    *> start palette entry
                    CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED

                    *> name
                    MOVE "Name" TO TAG-NAME
                    MOVE 4 TO NAME-LEN
                    MOVE FUNCTION STORED-CHAR-LENGTH(PALETTE-BLOCK-NAME) TO STR-LEN
                    CALL "NbtEncode-String" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PALETTE-BLOCK-NAME STR-LEN

                    IF PALETTE-BLOCK-PROPERTY-COUNT > 0
                        *> start properties
                        MOVE "Properties" TO TAG-NAME
                        MOVE 10 TO NAME-LEN
                        CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

                        PERFORM VARYING PROPERTY-INDEX FROM 1 BY 1 UNTIL PROPERTY-INDEX > PALETTE-BLOCK-PROPERTY-COUNT
                            MOVE PALETTE-BLOCK-PROPERTY-NAME(PROPERTY-INDEX) TO TAG-NAME
                            MOVE FUNCTION STORED-CHAR-LENGTH(TAG-NAME) TO NAME-LEN
                            MOVE PALETTE-BLOCK-PROPERTY-VALUE(PROPERTY-INDEX) TO STR
                            MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
                            CALL "NbtEncode-String" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN STR STR-LEN
                        END-PERFORM

                        *> end properties
                        CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER
                    END-IF

                    *> end palette entry
                    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER
                END-IF
            END-PERFORM

            *> end palette
            CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER

            *> Note: We only need to encode data if the palette length is greater than 1
            IF PALETTE-LENGTH > 1
                *> number of bits needed = ceil(log2(palette length - 1)) = bits needed to store (palette length - 1)
                COMPUTE INT32 = PALETTE-LENGTH - 1
                CALL "LeadingZeros32" USING INT32 PALETTE-BITS
                *> However, Minecraft uses a minimum of 4 bits
                COMPUTE PALETTE-BITS = FUNCTION MAX(32 - PALETTE-BITS, 4)
                COMPUTE PALETTE-BITS-POW = 2 ** PALETTE-BITS

                *> length of packed long array
                DIVIDE 64 BY PALETTE-BITS GIVING ENTRIES-PER-LONG
                DIVIDE 4096 BY ENTRIES-PER-LONG GIVING LONG-ARRAY-LENGTH ROUNDED MODE IS TOWARD-GREATER

                *> data (packed long array)
                MOVE "data" TO TAG-NAME
                MOVE 4 TO NAME-LEN
                CALL "NbtEncode-LongArray" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN LONG-ARRAY-LENGTH

                MOVE 1 TO BLOCK-INDEX
                PERFORM LONG-ARRAY-LENGTH TIMES
                    MOVE 0 TO LONG-ARRAY-ENTRY
                    MOVE 1 TO LONG-ARRAY-MULTIPLIER
                    PERFORM FUNCTION MIN(ENTRIES-PER-LONG, 4096 - BLOCK-INDEX + 1) TIMES
                        MOVE CHUNK-SECTION-BLOCK(SECTION-INDEX, BLOCK-INDEX) TO CURRENT-BLOCK-ID
                        COMPUTE LONG-ARRAY-ENTRY = LONG-ARRAY-ENTRY + LONG-ARRAY-MULTIPLIER * (PALETTE-INDEX(CURRENT-BLOCK-ID + 1) - 1)
                        MULTIPLY LONG-ARRAY-MULTIPLIER BY PALETTE-BITS-POW GIVING LONG-ARRAY-MULTIPLIER
                        ADD 1 TO BLOCK-INDEX
                    END-PERFORM
                    CALL "NbtEncode-Long" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED LONG-ARRAY-ENTRY-SIGNED
                END-PERFORM
            END-IF

            *> end block states
            CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER

            *> biomes - palette and data
            MOVE "biomes" TO TAG-NAME
            MOVE 6 TO NAME-LEN
            CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

            *> palette
            MOVE "palette" TO TAG-NAME
            MOVE 7 TO NAME-LEN
            CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

            MOVE 0 TO PALETTE-LENGTH
            INITIALIZE PALETTE-INDICES

            PERFORM VARYING BIOME-INDEX FROM 1 BY 1 UNTIL BIOME-INDEX > 64
                *> If the biome is not in the palette, add it
                MOVE CHUNK-SECTION-BIOME(SECTION-INDEX, BIOME-INDEX) TO CURRENT-BIOME-ID
                IF PALETTE-INDEX(CURRENT-BIOME-ID + 1) = 0
                    ADD 1 TO PALETTE-LENGTH
                    MOVE PALETTE-LENGTH TO PALETTE-INDEX(CURRENT-BIOME-ID + 1)
                    CALL "Registries-Get-EntryName" USING "minecraft:worldgen/biome" CURRENT-BIOME-ID STR
                    MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
                    CALL "NbtEncode-String" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED STR STR-LEN
                END-IF
            END-PERFORM

            *> end palette
            CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER

            *> Note: We only need to encode data if the palette length is greater than 1
            IF PALETTE-LENGTH > 1
                *> number of bits needed = ceil(log2(palette length - 1)) = bits needed to store (palette length - 1)
                COMPUTE INT32 = PALETTE-LENGTH - 1
                CALL "LeadingZeros32" USING INT32 PALETTE-BITS
                COMPUTE PALETTE-BITS = 32 - PALETTE-BITS
                COMPUTE PALETTE-BITS-POW = 2 ** PALETTE-BITS

                *> length of packed long array
                DIVIDE 64 BY PALETTE-BITS GIVING ENTRIES-PER-LONG
                DIVIDE 64 BY ENTRIES-PER-LONG GIVING LONG-ARRAY-LENGTH ROUNDED MODE IS TOWARD-GREATER

                *> data (packed long array)
                MOVE "data" TO TAG-NAME
                MOVE 4 TO NAME-LEN
                CALL "NbtEncode-LongArray" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN LONG-ARRAY-LENGTH

                MOVE 1 TO BIOME-INDEX
                PERFORM LONG-ARRAY-LENGTH TIMES
                    MOVE 0 TO LONG-ARRAY-ENTRY
                    MOVE 1 TO LONG-ARRAY-MULTIPLIER
                    PERFORM FUNCTION MIN(ENTRIES-PER-LONG, 64 - BIOME-INDEX + 1) TIMES
                        MOVE CHUNK-SECTION-BIOME(SECTION-INDEX, BIOME-INDEX) TO CURRENT-BIOME-ID
                        COMPUTE LONG-ARRAY-ENTRY = LONG-ARRAY-ENTRY + LONG-ARRAY-MULTIPLIER * (PALETTE-INDEX(CURRENT-BIOME-ID + 1) - 1)
                        MULTIPLY LONG-ARRAY-MULTIPLIER BY PALETTE-BITS-POW GIVING LONG-ARRAY-MULTIPLIER
                        ADD 1 TO BIOME-INDEX
                    END-PERFORM
                    CALL "NbtEncode-Long" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED LONG-ARRAY-ENTRY-SIGNED
                END-PERFORM
            END-IF

            *> end biomes
            CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER

            *> end section
            CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER
        END-IF
    END-PERFORM

    *> end chunk sections
    CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER

    *> start block entities
    MOVE "block_entities" TO TAG-NAME
    MOVE 14 TO NAME-LEN
    CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

    IF CHUNK-BLOCK-ENTITY-COUNT > 0
        MOVE 1 TO BLOCK-INDEX
        MOVE 0 TO ENTITY-COUNT
        PERFORM 98304 TIMES
            IF CHUNK-BLOCK-ENTITY-ID(BLOCK-INDEX) >= 0
                *> start block entity
                CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER

                *> id (registry: "minecraft:block_entity_type")
                MOVE "id" TO TAG-NAME
                MOVE 2 TO NAME-LEN
                MOVE CHUNK-BLOCK-ENTITY-ID(BLOCK-INDEX) TO INT32
                CALL "Registries-Get-EntryName" USING "minecraft:block_entity_type" INT32 STR
                MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
                CALL "NbtEncode-String" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN STR STR-LEN

                *> x, y, z
                SUBTRACT 1 FROM BLOCK-INDEX GIVING INT32
                DIVIDE INT32 BY 16 GIVING INT32 REMAINDER ENTITY-X
                DIVIDE INT32 BY 16 GIVING INT32 REMAINDER ENTITY-Z
                SUBTRACT 64 FROM INT32 GIVING ENTITY-Y
                *> x and z are in the world coordinate system
                COMPUTE ENTITY-X = ENTITY-X + CHUNK-X * 16
                COMPUTE ENTITY-Z = ENTITY-Z + CHUNK-Z * 16
                *> store the coordinates
                MOVE 1 TO NAME-LEN
                MOVE "x" TO TAG-NAME
                CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN ENTITY-X
                MOVE "y" TO TAG-NAME
                CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN ENTITY-Y
                MOVE "z" TO TAG-NAME
                CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN ENTITY-Z

                *> TODO: write the block entity-specific data

                *> end block entity
                CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER

                *> stop the loop once all block entities have been written
                ADD 1 TO ENTITY-COUNT
                IF ENTITY-COUNT >= CHUNK-BLOCK-ENTITY-COUNT
                    EXIT PERFORM
                END-IF
            END-IF
            ADD 1 TO BLOCK-INDEX
        END-PERFORM
    END-IF

    *> end block entities
    CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER

    *> end root tag
    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER

    *> Save the chunk
    COMPUTE NBT-BUFFER-LENGTH = NBT-ENCODER-OFFSET - 1
    CALL "Region-WriteChunkData" USING CHUNK-X CHUNK-Z NBT-BUFFER NBT-BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    MOVE 0 TO CHUNK-DIRTY

    GOBACK.

END PROGRAM World-SaveChunk.

*> --- World-LoadChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-LoadChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NBT-BUFFER               PIC X(1048576).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    *> Temporary variables
    01 SEEK-FOUND               BINARY-LONG UNSIGNED.
    COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==NBT-SEEK==.
    01 EXPECTED-TAG             PIC X(256).
    01 AT-END                   BINARY-CHAR UNSIGNED.
    01 TAG-NAME                 PIC X(256).
    01 NAME-LEN                 BINARY-LONG UNSIGNED.
    01 STR                      PIC X(256).
    01 STR-LEN                  BINARY-LONG UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT32                    BINARY-LONG.
    01 CHUNK-STATUS             PIC X(64).
    01 CHUNK-STATUS-ID          BINARY-LONG.
    01 ACCEPTED-CHUNK-STATUS-ID BINARY-LONG.
    01 X-POS                    BINARY-LONG.
    01 Z-POS                    BINARY-LONG.
    01 CHUNK-SECTION-MIN-Y      BINARY-LONG.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 LOADED-SECTION-COUNT     BINARY-LONG UNSIGNED.
    01 SECTION-INDEX            BINARY-LONG UNSIGNED.
    *> block entity data
    01 ENTITY-COUNT             BINARY-LONG UNSIGNED.
    01 ENTITY-ID                BINARY-LONG.
    01 ENTITY-X                 BINARY-LONG.
    01 ENTITY-Y                 BINARY-LONG.
    01 ENTITY-Z                 BINARY-LONG.
    *> World data
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-DECODER.
LINKAGE SECTION.
    01 LK-CHUNK-X               BINARY-LONG.
    01 LK-CHUNK-Z               BINARY-LONG.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-FAILURE.
    CALL "Region-ReadChunkData" USING LK-CHUNK-X LK-CHUNK-Z NBT-BUFFER NBT-BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0 OR NBT-BUFFER-LENGTH = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> start root tag
    MOVE 1 TO NBT-DECODER-OFFSET
    CALL "NbtDecode-RootCompound" USING NBT-DECODER-STATE NBT-BUFFER

    MOVE SPACES TO CHUNK-STATUS

    *> Do a first pass to get the chunk X, Z, and Y values, as well as the chunk generation status.
    *> The way we write NBT, they should come before any larger pieces of data, but this is not strictly guaranteed.
    MOVE NBT-DECODER-STATE TO NBT-SEEK-STATE
    MOVE 0 TO SEEK-FOUND
    PERFORM UNTIL EXIT
        CALL "NbtDecode-Peek" USING NBT-SEEK-STATE NBT-BUFFER AT-END TAG-NAME NAME-LEN
        IF AT-END > 0
            EXIT PERFORM
        END-IF
        EVALUATE TAG-NAME(1:NAME-LEN)
            WHEN "xPos"
                CALL "NbtDecode-Int" USING NBT-SEEK-STATE NBT-BUFFER X-POS
                ADD 1 TO SEEK-FOUND
            WHEN "zPos"
                CALL "NbtDecode-Int" USING NBT-SEEK-STATE NBT-BUFFER Z-POS
                ADD 1 TO SEEK-FOUND
            WHEN "yPos"
                CALL "NbtDecode-Int" USING NBT-SEEK-STATE NBT-BUFFER CHUNK-SECTION-MIN-Y
                ADD 1 TO SEEK-FOUND
            WHEN "Status"
                CALL "NbtDecode-String" USING NBT-SEEK-STATE NBT-BUFFER STR STR-LEN
                MOVE STR(1:STR-LEN) TO CHUNK-STATUS
            WHEN OTHER
                CALL "NbtDecode-Skip" USING NBT-SEEK-STATE NBT-BUFFER
        END-EVALUATE
        IF SEEK-FOUND = 3
            EXIT PERFORM
        END-IF
    END-PERFORM

    *> We are unable to continue generating partially-generated chunks ("proto-chunks"). Since we don't require light
    *> or heightmap data, any chunk that has progressed to at least the "surface" stage is acceptable.
    IF CHUNK-STATUS NOT = SPACES
        CALL "Registries-Get-EntryId" USING "minecraft:chunk_status" CHUNK-STATUS CHUNK-STATUS-ID
        IF CHUNK-STATUS-ID < 0
            DISPLAY "Unknown chunk status: " FUNCTION TRIM(CHUNK-STATUS)
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
        CALL "Registries-Get-EntryId" USING "minecraft:chunk_status" "minecraft:surface" ACCEPTED-CHUNK-STATUS-ID
        IF ACCEPTED-CHUNK-STATUS-ID < 0
            DISPLAY "Unknown chunk status: minecraft:surface"
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
        IF CHUNK-STATUS-ID < ACCEPTED-CHUNK-STATUS-ID
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
    END-IF

    *> Allocate a chunk slot
    CALL "World-AllocateChunk" USING X-POS Z-POS CHUNK-INDEX
    IF CHUNK-INDEX = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF
    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)

    *> Skip ahead until we find the sections tag.
    MOVE "sections" TO EXPECTED-TAG
    CALL "NbtDecode-SkipUntilTag" USING NBT-DECODER-STATE NBT-BUFFER EXPECTED-TAG AT-END
    IF AT-END > 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> start sections
    CALL "NbtDecode-List" USING NBT-DECODER-STATE NBT-BUFFER LOADED-SECTION-COUNT

    PERFORM LOADED-SECTION-COUNT TIMES
        *> start section
        CALL "NbtDecode-Compound" USING NBT-DECODER-STATE NBT-BUFFER

        *> section Y value
        MOVE NBT-DECODER-STATE TO NBT-SEEK-STATE
        CALL "DecodeSectionY" USING NBT-SEEK-STATE NBT-BUFFER INT8 LK-FAILURE
        IF LK-FAILURE > 0
            GOBACK
        END-IF
        COMPUTE SECTION-INDEX = INT8 + 1 - CHUNK-SECTION-MIN-Y

        *> section block states
        MOVE NBT-DECODER-STATE TO NBT-SEEK-STATE
        CALL "DecodeSectionBlockStates" USING NBT-SEEK-STATE NBT-BUFFER CHUNK-SECTION-BLOCKS(SECTION-INDEX)
                CHUNK-SECTION-NON-AIR(SECTION-INDEX) LK-FAILURE
        IF LK-FAILURE > 0
            GOBACK
        END-IF

        *> section biomes
        CALL "DecodeSectionBiomes" USING NBT-DECODER-STATE NBT-BUFFER CHUNK-SECTION-BIOMES(SECTION-INDEX) LK-FAILURE
        IF LK-FAILURE > 0
            GOBACK
        END-IF

        *> end section
        CALL "NbtDecode-SkipRemainingTags" USING NBT-DECODER-STATE NBT-BUFFER
        CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER
    END-PERFORM

    *> end sections
    CALL "NbtDecode-EndList" USING NBT-DECODER-STATE NBT-BUFFER

    *> Skip to the block entities
    *> TODO: make this position-independent
    MOVE "block_entities" TO EXPECTED-TAG
    CALL "NbtDecode-SkipUntilTag" USING NBT-DECODER-STATE NBT-BUFFER EXPECTED-TAG AT-END
    IF AT-END = 0
        CALL "NbtDecode-List" USING NBT-DECODER-STATE NBT-BUFFER ENTITY-COUNT
        MOVE ENTITY-COUNT TO CHUNK-BLOCK-ENTITY-COUNT
        PERFORM ENTITY-COUNT TIMES
            CALL "NbtDecode-Compound" USING NBT-DECODER-STATE NBT-BUFFER
            PERFORM UNTIL EXIT
                CALL "NbtDecode-Peek" USING NBT-DECODER-STATE NBT-BUFFER AT-END TAG-NAME NAME-LEN
                IF AT-END > 0
                    EXIT PERFORM
                END-IF
                EVALUATE TAG-NAME(1:NAME-LEN)
                    WHEN "id"
                        CALL "NbtDecode-String" USING NBT-DECODER-STATE NBT-BUFFER STR STR-LEN
                        CALL "Registries-Get-EntryId" USING "minecraft:block_entity_type" STR(1:STR-LEN) ENTITY-ID
                    WHEN "x"
                        CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER ENTITY-X
                    WHEN "y"
                        CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER ENTITY-Y
                    WHEN "z"
                        CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER ENTITY-Z
                    WHEN OTHER
                        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER
                END-EVALUATE
            END-PERFORM
            *> convert to chunk-relative coordinates
            COMPUTE INT32 = FUNCTION MOD(ENTITY-X, 16) + 16 * (FUNCTION MOD(ENTITY-Z, 16) + 16 * (ENTITY-Y + 64)) + 1
            MOVE ENTITY-ID TO CHUNK-BLOCK-ENTITY-ID(INT32)
            CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER
        END-PERFORM
        CALL "NbtDecode-EndList" USING NBT-DECODER-STATE NBT-BUFFER
    END-IF

    *> end root tag
    CALL "NbtDecode-SkipRemainingTags" USING NBT-DECODER-STATE NBT-BUFFER
    CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER

    *> chunk does not need to be saved
    MOVE 0 TO CHUNK-DIRTY

    GOBACK.

    *> --- DecodeSectionY ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. DecodeSectionY.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 TAG-NAME             PIC X(1)                    VALUE "Y".
        01 NAME-LEN             BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
        01 LK-BUFFER            PIC X ANY LENGTH.
        01 LK-Y                 BINARY-CHAR.
        01 LK-FAILURE           BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-Y LK-FAILURE.
        CALL "NbtDecode-SkipUntilTag" USING LK-STATE LK-BUFFER TAG-NAME LK-FAILURE
        IF LK-FAILURE > 0
            GOBACK
        END-IF
        CALL "NbtDecode-Byte" USING LK-STATE LK-BUFFER LK-Y
        GOBACK.

    END PROGRAM DecodeSectionY.

    *> --- DecodeSectionBlockStates ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. DecodeSectionBlockStates.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==NBT-BEGIN==.
        01 AT-END                   BINARY-CHAR UNSIGNED.
        01 TAG-NAME                 PIC X(256).
        01 NAME-LEN                 BINARY-LONG UNSIGNED.
        01 STR                      PIC X(256).
        01 STR-LEN                  BINARY-LONG UNSIGNED.
        01 INT32                    BINARY-LONG.
        01 BLOCK-INDEX              BINARY-LONG UNSIGNED.
        01 CURRENT-BLOCK-ID         BINARY-LONG UNSIGNED.
        01 PALETTE-LENGTH           BINARY-LONG UNSIGNED.
        01 PALETTE-INDEX            BINARY-SHORT UNSIGNED.
        01 PALETTE-BITS             BINARY-LONG UNSIGNED.
        01 PALETTE-BITS-POW         BINARY-LONG UNSIGNED.
        01 ENTRIES-PER-LONG         BINARY-LONG UNSIGNED.
        01 LONG-ARRAY-LENGTH        BINARY-LONG UNSIGNED.
        01 LONG-ARRAY-ENTRY         BINARY-LONG-LONG UNSIGNED.
        01 LONG-ARRAY-ENTRY-SIGNED  REDEFINES LONG-ARRAY-ENTRY BINARY-LONG-LONG.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==PALETTE-BLOCK==.
        *> A map of palette indices to block state IDs
        01 BLOCK-STATE-IDS          BINARY-LONG UNSIGNED OCCURS 4096 TIMES.
    LINKAGE SECTION.
        COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
        01 LK-BUFFER                PIC X ANY LENGTH.
        01 LK-BLOCKS.
            02 LK-BLOCK-ID          BINARY-LONG UNSIGNED OCCURS 4096 TIMES.
        01 LK-NON-AIR               BINARY-LONG UNSIGNED.
        01 LK-FAILURE               BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-BLOCKS LK-NON-AIR LK-FAILURE.
        MOVE 0 TO LK-FAILURE

        MOVE "block_states" TO TAG-NAME
        CALL "NbtDecode-SkipUntilTag" USING LK-STATE LK-BUFFER TAG-NAME AT-END
        IF AT-END > 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        *> start block states
        CALL "NbtDecode-Compound" USING LK-STATE LK-BUFFER
        MOVE LK-STATE TO NBT-BEGIN-STATE

        *> Skip to the palette
        MOVE "palette" TO TAG-NAME
        CALL "NbtDecode-SkipUntilTag" USING LK-STATE LK-BUFFER TAG-NAME AT-END
        IF AT-END > 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        *> start palette
        CALL "NbtDecode-List" USING LK-STATE LK-BUFFER PALETTE-LENGTH

        PERFORM VARYING PALETTE-INDEX FROM 1 BY 1 UNTIL PALETTE-INDEX > PALETTE-LENGTH
            *> start palette entry
            CALL "NbtDecode-Compound" USING LK-STATE LK-BUFFER
            MOVE 0 TO PALETTE-BLOCK-PROPERTY-COUNT

            PERFORM UNTIL EXIT
                CALL "NbtDecode-Peek" USING LK-STATE LK-BUFFER AT-END TAG-NAME NAME-LEN
                IF AT-END > 0
                    EXIT PERFORM
                END-IF
                EVALUATE TAG-NAME(1:NAME-LEN)
                    WHEN "Name"
                        CALL "NbtDecode-String" USING LK-STATE LK-BUFFER STR STR-LEN
                        MOVE STR(1:STR-LEN) TO PALETTE-BLOCK-NAME

                    WHEN "Properties"
                        CALL "NbtDecode-Compound" USING LK-STATE LK-BUFFER
                        PERFORM UNTIL EXIT
                            CALL "NbtDecode-Peek" USING LK-STATE LK-BUFFER AT-END TAG-NAME NAME-LEN
                            IF AT-END > 0
                                EXIT PERFORM
                            END-IF
                            ADD 1 TO PALETTE-BLOCK-PROPERTY-COUNT
                            CALL "NbtDecode-String" USING LK-STATE LK-BUFFER STR STR-LEN
                            MOVE TAG-NAME(1:NAME-LEN) TO PALETTE-BLOCK-PROPERTY-NAME(PALETTE-BLOCK-PROPERTY-COUNT)
                            MOVE STR(1:STR-LEN) TO PALETTE-BLOCK-PROPERTY-VALUE(PALETTE-BLOCK-PROPERTY-COUNT)
                        END-PERFORM
                        CALL "NbtDecode-EndCompound" USING LK-STATE LK-BUFFER

                    WHEN OTHER
                        CALL "NbtDecode-Skip" USING LK-STATE LK-BUFFER
                END-EVALUATE
            END-PERFORM

            *> end palette entry
            CALL "NbtDecode-EndCompound" USING LK-STATE LK-BUFFER
            CALL "Blocks-Get-StateId" USING PALETTE-BLOCK-DESCRIPTION BLOCK-STATE-IDS(PALETTE-INDEX)
        END-PERFORM

        *> end palette
        CALL "NbtDecode-EndList" USING LK-STATE LK-BUFFER

        *> If the palette has length 1, we don't care about the data. In fact, it might not be there.
        IF PALETTE-LENGTH = 1
            *> Fill the section with the singular block state (unless it is air).
            MOVE BLOCK-STATE-IDS(1) TO CURRENT-BLOCK-ID
            IF CURRENT-BLOCK-ID > 0
                INITIALIZE LK-BLOCKS REPLACING NUMERIC BY CURRENT-BLOCK-ID
                MOVE 4096 TO LK-NON-AIR
            END-IF
        ELSE
            *> Reset the state to the beginning of the block states compound, since "data" may come before "palette".
            *> We don't write NBT this way, but Minecraft does.
            MOVE NBT-BEGIN-STATE TO LK-STATE

            *> Skip to the data
            MOVE "data" TO TAG-NAME
            CALL "NbtDecode-SkipUntilTag" USING LK-STATE LK-BUFFER TAG-NAME AT-END
            IF AT-END > 0
                MOVE 1 TO LK-FAILURE
                GOBACK
            END-IF

            *> read packed long array
            CALL "NbtDecode-List" USING LK-STATE LK-BUFFER LONG-ARRAY-LENGTH

            *> number of bits per block = ceil(log2(palette length - 1)) = bits needed to store (palette length - 1)
            COMPUTE INT32 = PALETTE-LENGTH - 1
            CALL "LeadingZeros32" USING INT32 PALETTE-BITS
            *> However, Minecraft uses a minimum of 4 bits
            COMPUTE PALETTE-BITS = FUNCTION MAX(32 - PALETTE-BITS, 4)
            DIVIDE 64 BY PALETTE-BITS GIVING ENTRIES-PER-LONG
            COMPUTE PALETTE-BITS-POW = 2 ** PALETTE-BITS
            >>IF GCVERSION >= 32
                SUBTRACT 1 FROM PALETTE-BITS-POW
            >>END-IF

            MOVE 1 TO BLOCK-INDEX
            PERFORM LONG-ARRAY-LENGTH TIMES
                CALL "NbtDecode-Long" USING LK-STATE LK-BUFFER LONG-ARRAY-ENTRY-SIGNED
                PERFORM FUNCTION MIN(ENTRIES-PER-LONG, 4096 - BLOCK-INDEX + 1) TIMES
                    >>IF GCVERSION >= 32
                        COMPUTE PALETTE-INDEX = LONG-ARRAY-ENTRY B-AND PALETTE-BITS-POW
                        COMPUTE LONG-ARRAY-ENTRY = LONG-ARRAY-ENTRY B-SHIFT-R PALETTE-BITS
                    >>ELSE
                        DIVIDE LONG-ARRAY-ENTRY BY PALETTE-BITS-POW GIVING LONG-ARRAY-ENTRY REMAINDER PALETTE-INDEX
                    >>END-IF
                    MOVE BLOCK-STATE-IDS(PALETTE-INDEX + 1) TO CURRENT-BLOCK-ID
                    IF CURRENT-BLOCK-ID > 0
                        MOVE CURRENT-BLOCK-ID TO LK-BLOCK-ID(BLOCK-INDEX)
                        ADD 1 TO LK-NON-AIR
                    END-IF
                    ADD 1 TO BLOCK-INDEX
                END-PERFORM
            END-PERFORM

            *> end data
            CALL "NbtDecode-EndList" USING LK-STATE LK-BUFFER
        END-IF

        *> end block states
        CALL "NbtDecode-SkipRemainingTags" USING LK-STATE LK-BUFFER
        CALL "NbtDecode-EndCompound" USING LK-STATE LK-BUFFER

        GOBACK.

    END PROGRAM DecodeSectionBlockStates.

    *> --- DecodeSectionBiomes ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. DecodeSectionBiomes.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==NBT-BEGIN==.
        01 AT-END                   BINARY-CHAR UNSIGNED.
        01 TAG-NAME                 PIC X(256).
        01 STR                      PIC X(256).
        01 STR-LEN                  BINARY-LONG UNSIGNED.
        01 INT32                    BINARY-LONG.
        01 PALETTE-LENGTH           BINARY-LONG UNSIGNED.
        01 PALETTE-INDEX            BINARY-SHORT UNSIGNED.
        01 PALETTE-BITS             BINARY-LONG UNSIGNED.
        01 PALETTE-BITS-POW         BINARY-LONG UNSIGNED.
        01 ENTRIES-PER-LONG         BINARY-LONG UNSIGNED.
        01 LONG-ARRAY-LENGTH        BINARY-LONG UNSIGNED.
        01 LONG-ARRAY-ENTRY         BINARY-LONG-LONG UNSIGNED.
        01 LONG-ARRAY-ENTRY-SIGNED  REDEFINES LONG-ARRAY-ENTRY BINARY-LONG-LONG.
        01 PALETTE-BIOME-IDS        BINARY-LONG UNSIGNED OCCURS 64 TIMES.
        01 CURRENT-BIOME-ID         BINARY-LONG UNSIGNED.
        01 BIOME-INDEX              BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
        01 LK-BUFFER            PIC X ANY LENGTH.
        01 LK-BIOMES.
            02 LK-BIOME-ID          BINARY-LONG UNSIGNED OCCURS 64 TIMES.
        01 LK-FAILURE           BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-BIOMES LK-FAILURE.
        MOVE 0 TO LK-FAILURE

        MOVE "biomes" TO TAG-NAME
        CALL "NbtDecode-SkipUntilTag" USING LK-STATE LK-BUFFER TAG-NAME AT-END

        *> Biomes are optional since previous versions of CobolCraft did not write them.
        IF AT-END > 0
            GOBACK
        END-IF

        *> start biomes
        CALL "NbtDecode-Compound" USING LK-STATE LK-BUFFER
        MOVE LK-STATE TO NBT-BEGIN-STATE

        *> Skip to the palette
        MOVE "palette" TO TAG-NAME
        CALL "NbtDecode-SkipUntilTag" USING LK-STATE LK-BUFFER TAG-NAME AT-END
        IF AT-END > 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        *> palette
        CALL "NbtDecode-List" USING LK-STATE LK-BUFFER PALETTE-LENGTH
        PERFORM VARYING PALETTE-INDEX FROM 1 BY 1 UNTIL PALETTE-INDEX > PALETTE-LENGTH
            CALL "NbtDecode-String" USING LK-STATE LK-BUFFER STR STR-LEN
            CALL "Registries-Get-EntryId" USING "minecraft:worldgen/biome" STR(1:STR-LEN) PALETTE-BIOME-IDS(PALETTE-INDEX)
        END-PERFORM
        CALL "NbtDecode-EndList" USING LK-STATE LK-BUFFER

        *> If the palette has length 1, we don't care about the data. In fact, it might not be there.
        IF PALETTE-LENGTH = 1
            INITIALIZE LK-BIOMES REPLACING NUMERIC BY PALETTE-BIOME-IDS(1)
        ELSE
            *> Reset the state to the beginning of the biomes compound, since "data" may come before "palette".
            MOVE NBT-BEGIN-STATE TO LK-STATE

            *> Skip to the data
            MOVE "data" TO TAG-NAME
            CALL "NbtDecode-SkipUntilTag" USING LK-STATE LK-BUFFER TAG-NAME AT-END
            IF AT-END > 0
                MOVE 1 TO LK-FAILURE
                GOBACK
            END-IF

            *> read packed long array
            CALL "NbtDecode-List" USING LK-STATE LK-BUFFER LONG-ARRAY-LENGTH

            *> number of bits per block = ceil(log2(palette length - 1)) = bits needed to store (palette length - 1)
            COMPUTE INT32 = PALETTE-LENGTH - 1
            CALL "LeadingZeros32" USING INT32 PALETTE-BITS
            COMPUTE PALETTE-BITS = 32 - PALETTE-BITS
            DIVIDE 64 BY PALETTE-BITS GIVING ENTRIES-PER-LONG
            COMPUTE PALETTE-BITS-POW = 2 ** PALETTE-BITS
            >>IF GCVERSION >= 32
                SUBTRACT 1 FROM PALETTE-BITS-POW
            >>END-IF

            MOVE 1 TO BIOME-INDEX
            PERFORM LONG-ARRAY-LENGTH TIMES
                CALL "NbtDecode-Long" USING LK-STATE LK-BUFFER LONG-ARRAY-ENTRY-SIGNED
                PERFORM FUNCTION MIN(ENTRIES-PER-LONG, 64 - BIOME-INDEX + 1) TIMES
                    >>IF GCVERSION >= 32
                        COMPUTE PALETTE-INDEX = LONG-ARRAY-ENTRY B-AND PALETTE-BITS-POW
                        COMPUTE LONG-ARRAY-ENTRY = LONG-ARRAY-ENTRY B-SHIFT-R PALETTE-BITS
                    >>ELSE
                        DIVIDE LONG-ARRAY-ENTRY BY PALETTE-BITS-POW GIVING LONG-ARRAY-ENTRY REMAINDER PALETTE-INDEX
                    >>END-IF
                    MOVE PALETTE-BIOME-IDS(PALETTE-INDEX + 1) TO LK-BIOME-ID(BIOME-INDEX)
                    ADD 1 TO BIOME-INDEX
                END-PERFORM
            END-PERFORM

            *> end data
            CALL "NbtDecode-EndList" USING LK-STATE LK-BUFFER
        END-IF

        *> end biomes
        CALL "NbtDecode-SkipRemainingTags" USING LK-STATE LK-BUFFER
        CALL "NbtDecode-EndCompound" USING LK-STATE LK-BUFFER

        GOBACK.

    END PROGRAM DecodeSectionBiomes.

END PROGRAM World-LoadChunk.

*> --- World-EnsureChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-EnsureChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    01 IO-FAILURE           BINARY-CHAR UNSIGNED.
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
    01 X-START              BINARY-LONG.
    01 X-END                BINARY-LONG.
    01 Z-START              BINARY-LONG.
    01 Z-END                BINARY-LONG.
    01 X-POS                BINARY-LONG.
    01 Z-POS                BINARY-LONG.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-SAVE-REQUIRED     BINARY-CHAR UNSIGNED.

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
            IF CHUNK-DIRTY > 0
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
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 CHUNK-BLOCK-X        BINARY-LONG.
    01 CHUNK-BLOCK-Z        BINARY-LONG.
    01 SPAWN-X-START        BINARY-LONG.
    01 SPAWN-X-END          BINARY-LONG.
    01 SPAWN-Z-START        BINARY-LONG.
    01 SPAWN-Z-END          BINARY-LONG.
    01 MIN-DISTANCE         BINARY-LONG.
    01 PLAYER-INDEX         BINARY-LONG UNSIGNED.
    *> World data
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    *> Player data
    COPY DD-PLAYERS.
    COPY DD-SERVER-PROPERTIES.
LINKAGE SECTION.
    01 LK-VIEW-DISTANCE     BINARY-LONG UNSIGNED.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

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
                IF CHUNK-DIRTY > 0
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

*> --- World-SaveLevel ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SaveLevel.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> File name and data
    01 LEVEL-FILE-NAME      PIC X(255)              VALUE "save/level.dat".
    01 ERRNO                BINARY-LONG.
    01 NBT-BUFFER           PIC X(64000).
    01 NBT-BUFFER-LENGTH    BINARY-LONG UNSIGNED.
    01 COMPRESSED-BUFFER    PIC X(64000).
    01 COMPRESSED-LENGTH    BINARY-LONG UNSIGNED.
    *> Temporary variables
    01 TAG-NAME             PIC X(256).
    01 NAME-LEN             BINARY-LONG UNSIGNED.
    *> World data
    COPY DD-WORLD.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-ENCODER.
LINKAGE SECTION.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    MOVE 0 TO LK-FAILURE
    MOVE ALL X"00" TO NBT-BUFFER

    *> root tag
    MOVE 1 TO NBT-ENCODER-OFFSET
    CALL "NbtEncode-RootCompound" USING NBT-ENCODER-STATE NBT-BUFFER

    *> "Data" tag
    MOVE "Data" TO TAG-NAME
    MOVE 4 TO NAME-LEN
    CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

    *> "Time": world age
    MOVE "Time" TO TAG-NAME
    MOVE 4 TO NAME-LEN
    CALL "NbtEncode-Long" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-AGE

    *> "DayTime": world time
    MOVE "DayTime" TO TAG-NAME
    MOVE 7 TO NAME-LEN
    CALL "NbtEncode-Long" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-TIME

    *> "SpawnX", "SpawnY", "SpawnZ": spawn point
    MOVE 6 TO NAME-LEN
    MOVE "SpawnX" TO TAG-NAME
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-SPAWN-X
    MOVE "SpawnY" TO TAG-NAME
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-SPAWN-Y
    MOVE "SpawnZ" TO TAG-NAME
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-SPAWN-Z

    *> hardcore mode
    MOVE "hardcore" TO TAG-NAME
    MOVE 8 TO NAME-LEN
    CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-HARDCORE

    *> end "Data" and root tags
    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER
    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER

    *> write the data to disk in gzip-compressed form
    COMPUTE NBT-BUFFER-LENGTH = NBT-ENCODER-OFFSET - 1
    MOVE LENGTH OF COMPRESSED-BUFFER TO COMPRESSED-LENGTH
    CALL "GzipCompress" USING NBT-BUFFER NBT-BUFFER-LENGTH COMPRESSED-BUFFER COMPRESSED-LENGTH GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF
    CALL "Files-WriteAll" USING LEVEL-FILE-NAME COMPRESSED-BUFFER COMPRESSED-LENGTH LK-FAILURE

    GOBACK.

END PROGRAM World-SaveLevel.

*> --- World-LoadLevel ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-LoadLevel.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> File name and data
    01 LEVEL-FILE-NAME          PIC X(255)              VALUE "save/level.dat".
    01 ERRNO                    BINARY-LONG.
    01 COMPRESSED-BUFFER        PIC X(64000).
    01 COMPRESSED-LENGTH        BINARY-LONG UNSIGNED.
    01 NBT-BUFFER               PIC X(64000).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    *> Temporary variables
    01 STR-VALUE                PIC X(256).
    01 STR-LEN                  BINARY-LONG UNSIGNED.
    01 AT-END                   BINARY-CHAR UNSIGNED.
    *> World data
    COPY DD-WORLD.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-DECODER.
LINKAGE SECTION.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> Set defaults
    MOVE 0 TO WORLD-AGE WORLD-TIME
    MOVE 0 TO WORLD-SPAWN-X WORLD-SPAWN-Y WORLD-SPAWN-Z
    MOVE 0 TO WORLD-HARDCORE

    *> Read the file
    CALL "Files-ReadAll" USING LEVEL-FILE-NAME NBT-BUFFER NBT-BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0 OR NBT-BUFFER-LENGTH = 0
        GOBACK
    END-IF

    *> Check for the gzip magic number, and decompress if present
    IF NBT-BUFFER(1:2) = X"1F8B"
        MOVE NBT-BUFFER(1:NBT-BUFFER-LENGTH) TO COMPRESSED-BUFFER(1:NBT-BUFFER-LENGTH)
        MOVE NBT-BUFFER-LENGTH TO COMPRESSED-LENGTH
        MOVE LENGTH OF NBT-BUFFER TO NBT-BUFFER-LENGTH
        CALL "GzipDecompress" USING COMPRESSED-BUFFER COMPRESSED-LENGTH NBT-BUFFER NBT-BUFFER-LENGTH GIVING ERRNO
        IF ERRNO NOT = 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
    END-IF

    *> root tag containing the "Data" compound
    MOVE 1 TO NBT-DECODER-OFFSET
    CALL "NbtDecode-RootCompound" USING NBT-DECODER-STATE NBT-BUFFER
    CALL "NbtDecode-Compound" USING NBT-DECODER-STATE NBT-BUFFER

    PERFORM UNTIL EXIT
        CALL "NbtDecode-Peek" USING NBT-DECODER-STATE NBT-BUFFER AT-END STR-VALUE STR-LEN
        IF AT-END > 0
            EXIT PERFORM
        END-IF
        EVALUATE STR-VALUE(1:STR-LEN)
            WHEN "Time"
                CALL "NbtDecode-Long" USING NBT-DECODER-STATE NBT-BUFFER WORLD-AGE
            WHEN "DayTime"
                CALL "NbtDecode-Long" USING NBT-DECODER-STATE NBT-BUFFER WORLD-TIME
            WHEN "SpawnX"
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER WORLD-SPAWN-X
            WHEN "SpawnY"
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER WORLD-SPAWN-Y
            WHEN "SpawnZ"
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER WORLD-SPAWN-Z
            WHEN "hardcore"
                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER WORLD-HARDCORE
            WHEN OTHER
                CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER
        END-EVALUATE
    END-PERFORM

    *> end of "Data" and root tags
    CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER
    CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER

    GOBACK.

END PROGRAM World-LoadLevel.

*> --- World-Save ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-Save.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    *> Create directories. Ignore errors, as they are likely to be caused by the directories already existing.
    CALL "CBL_CREATE_DIR" USING "save"
    CALL "CBL_CREATE_DIR" USING "save/region"

    *> Save world metadata
    CALL "World-SaveLevel" USING LK-FAILURE
    IF LK-FAILURE > 0
        GOBACK
    END-IF

    *> Save dirty chunks
    PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > WORLD-CHUNK-COUNT
        SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)
        IF CHUNK-DIRTY NOT = 0
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
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 LEVEL-SAVE-REQUIRED  BINARY-CHAR UNSIGNED.
    01 CHUNK-SAVE-REQUIRED  BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-FAILURE           BINARY-CHAR UNSIGNED.

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
    COPY DD-CHUNK-REF.
    01 POS-CHUNK-X          BINARY-LONG.
    01 POS-CHUNK-Z          BINARY-LONG.
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
    DIVIDE LK-X BY 16 GIVING POS-CHUNK-X ROUNDED MODE IS TOWARD-LESSER
    DIVIDE LK-Z BY 16 GIVING POS-CHUNK-Z ROUNDED MODE IS TOWARD-LESSER
    CALL "World-FindChunkIndex" USING POS-CHUNK-X POS-CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        MOVE 0 TO LK-BLOCK-ID
        GOBACK
    END-IF
    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)
    *> compute the block index
    COMPUTE SECTION-INDEX = (LK-Y + 64) / 16 + 1
    COMPUTE BLOCK-INDEX = ((FUNCTION MOD(LK-Y + 64, 16)) * 16 + (FUNCTION MOD(LK-Z, 16))) * 16 + (FUNCTION MOD(LK-X, 16)) + 1
    MOVE CHUNK-SECTION-BLOCK(SECTION-INDEX, BLOCK-INDEX) TO LK-BLOCK-ID
    GOBACK.

END PROGRAM World-GetBlock.

*> --- World-SetBlock ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SetBlock.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 WORLD-EVENT-BLOCK-BREAK      BINARY-LONG UNSIGNED    VALUE 2001.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    COPY DD-CLIENT-STATES.
    COPY DD-CLIENTS.
    COPY DD-SERVER-PROPERTIES.
    01 POS-CHUNK-X          BINARY-LONG.
    01 POS-CHUNK-Z          BINARY-LONG.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 SECTION-INDEX        BINARY-LONG UNSIGNED.
    01 BLOCK-IN-CHUNK-INDEX BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
    01 PREVIOUS-BLOCK-ID    BINARY-LONG UNSIGNED.
    01 IS-SAME-BLOCK-TYPE   BINARY-CHAR UNSIGNED.
    01 CLIENT-ID            BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    *> The client that performed the action, to avoid playing sounds/particles for them (optional)
    01 LK-CLIENT            BINARY-LONG UNSIGNED.
    01 LK-POSITION.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.
    01 LK-BLOCK-ID          BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING OPTIONAL LK-CLIENT LK-POSITION LK-BLOCK-ID.
    *> Find the chunk, section, and block indices
    DIVIDE LK-X BY 16 GIVING POS-CHUNK-X ROUNDED MODE IS TOWARD-LESSER
    DIVIDE LK-Z BY 16 GIVING POS-CHUNK-Z ROUNDED MODE IS TOWARD-LESSER
    CALL "World-FindChunkIndex" USING POS-CHUNK-X POS-CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        GOBACK
    END-IF
    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)

    COMPUTE SECTION-INDEX = (LK-Y + 64) / 16 + 1
    COMPUTE BLOCK-INDEX = ((FUNCTION MOD(LK-Y + 64, 16)) * 16 + (FUNCTION MOD(LK-Z, 16))) * 16 + (FUNCTION MOD(LK-X, 16)) + 1

    *> Skip if identical to the current block
    MOVE CHUNK-SECTION-BLOCK(SECTION-INDEX, BLOCK-INDEX) TO PREVIOUS-BLOCK-ID
    IF PREVIOUS-BLOCK-ID = LK-BLOCK-ID
        GOBACK
    END-IF

    *> Check whether the block is becoming air or non-air
    EVALUATE TRUE
        WHEN LK-BLOCK-ID = 0
            SUBTRACT 1 FROM CHUNK-SECTION-NON-AIR(SECTION-INDEX)
        WHEN PREVIOUS-BLOCK-ID = 0
            ADD 1 TO CHUNK-SECTION-NON-AIR(SECTION-INDEX)
    END-EVALUATE

    *> Set the block and mark the chunk as dirty
    MOVE LK-BLOCK-ID TO CHUNK-SECTION-BLOCK(SECTION-INDEX, BLOCK-INDEX)
    MOVE 1 TO CHUNK-DIRTY

    *> If the block is changing to a different type (not just state), remove any block entity
    IF PREVIOUS-BLOCK-ID NOT = 0
        CALL "Blocks-CompareBlockType" USING PREVIOUS-BLOCK-ID LK-BLOCK-ID IS-SAME-BLOCK-TYPE
        IF IS-SAME-BLOCK-TYPE = 0
            COMPUTE BLOCK-IN-CHUNK-INDEX = ((LK-Y + 64) * 16 + (FUNCTION MOD(LK-Z, 16))) * 16 + (FUNCTION MOD(LK-X, 16)) + 1
            IF CHUNK-BLOCK-ENTITY-ID(BLOCK-IN-CHUNK-INDEX) >= 0
                MOVE -1 TO CHUNK-BLOCK-ENTITY-ID(BLOCK-IN-CHUNK-INDEX)
                SUBTRACT 1 FROM CHUNK-BLOCK-ENTITY-COUNT
            END-IF
        END-IF
    END-IF

    *> Notify clients
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
            CALL "SendPacket-BlockUpdate" USING CLIENT-ID LK-POSITION LK-BLOCK-ID
            *> play block break sound and particles
            IF (LK-CLIENT IS OMITTED OR CLIENT-ID NOT = LK-CLIENT) AND LK-BLOCK-ID = 0
                CALL "SendPacket-WorldEvent" USING CLIENT-ID WORLD-EVENT-BLOCK-BREAK LK-POSITION PREVIOUS-BLOCK-ID
            END-IF
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM World-SetBlock.

*> --- World-SetBlockEntity ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SetBlockEntity.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    COPY DD-CLIENT-STATES.
    COPY DD-CLIENTS.
    COPY DD-SERVER-PROPERTIES.
    01 POS-CHUNK-X          BINARY-LONG.
    01 POS-CHUNK-Z          BINARY-LONG.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 BLOCK-IN-CHUNK-INDEX BINARY-LONG UNSIGNED.
    01 CLIENT-ID            BINARY-LONG UNSIGNED.
    *> TODO support entity data
    *> Currently, only block entities without any data (= empty compound tag) are supported.
    01 ENTITY-DATA          PIC X(2)                        VALUE X"0A00".
    01 ENTITY-DATA-LENGTH   BINARY-LONG UNSIGNED            VALUE 2.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.
    01 LK-BLOCK-ENTITY-ID   BINARY-LONG.

PROCEDURE DIVISION USING LK-POSITION LK-BLOCK-ENTITY-ID.
    *> Find the chunk and block indices
    DIVIDE LK-X BY 16 GIVING POS-CHUNK-X ROUNDED MODE IS TOWARD-LESSER
    DIVIDE LK-Z BY 16 GIVING POS-CHUNK-Z ROUNDED MODE IS TOWARD-LESSER
    CALL "World-FindChunkIndex" USING POS-CHUNK-X POS-CHUNK-Z CHUNK-INDEX
    IF CHUNK-INDEX = 0
        GOBACK
    END-IF
    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)

    COMPUTE BLOCK-IN-CHUNK-INDEX = ((LK-Y + 64) * 16 + (FUNCTION MOD(LK-Z, 16))) * 16 + (FUNCTION MOD(LK-X, 16)) + 1

    IF CHUNK-BLOCK-ENTITY-ID(BLOCK-IN-CHUNK-INDEX) >= 0
        SUBTRACT 1 FROM CHUNK-BLOCK-ENTITY-COUNT
    END-IF

    *> Set the block entity ID
    MOVE LK-BLOCK-ENTITY-ID TO CHUNK-BLOCK-ENTITY-ID(BLOCK-IN-CHUNK-INDEX)
    ADD 1 TO CHUNK-BLOCK-ENTITY-COUNT

    *> Notify clients
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
            CALL "SendPacket-BlockEntityData" USING CLIENT-ID LK-POSITION LK-BLOCK-ENTITY-ID ENTITY-DATA ENTITY-DATA-LENGTH
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM World-SetBlockEntity.

*> --- World-DropItem ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-DropItem.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                 FLOAT-LONG.
        02 LK-Y                 FLOAT-LONG.
        02 LK-Z                 FLOAT-LONG.
    01 LK-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.

PROCEDURE DIVISION USING LK-POSITION LK-SLOT.
    GOBACK.

END PROGRAM World-DropItem.

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
    ADD 1 TO WORLD-TIME
    GOBACK.

END PROGRAM World-UpdateAge.

*> --- World-IsHardcore ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-IsHardcore.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-HARDCORE              BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-HARDCORE.
    MOVE WORLD-HARDCORE TO LK-HARDCORE
    GOBACK.

END PROGRAM World-IsHardcore.

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
