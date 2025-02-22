*> --- World-SaveChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SaveChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NBT-BUFFER               PIC X(1048576).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    01 CHUNK-SECTION-MIN-Y      BINARY-LONG                 VALUE -4.
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
