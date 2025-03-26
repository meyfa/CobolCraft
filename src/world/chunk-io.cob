*> --- World-SaveChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SaveChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BIOME-REGISTRY-ID        BINARY-LONG                 VALUE -1.
    01 BLOCK-ENTITY-REGISTRY-ID BINARY-LONG                 VALUE -1.
    01 ENTITY-REGISTRY-ID       BINARY-LONG                 VALUE -1.
    COPY DD-CALLBACKS.
    COPY DD-REGION-FILES.
    01 FILE-TYPE                BINARY-CHAR UNSIGNED.
    01 BUFFER                   PIC X(1048576).
    01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
    COPY DD-NBT-ENCODER.
    01 CHUNK-SECTION-MIN-Y      BINARY-LONG                 VALUE -4.
    *> Temporary variables
    01 TAG                      PIC X(256).
    01 STR                      PIC X(256).
    01 LEN                      BINARY-LONG UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT16                    BINARY-SHORT.
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
    01 BLOCK-ENTITY-COUNT       BINARY-LONG UNSIGNED.
    01 BLOCK-ENTITY-X           BINARY-LONG.
    01 BLOCK-ENTITY-Y           BINARY-LONG.
    01 BLOCK-ENTITY-Z           BINARY-LONG.
    *> World data
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    COPY DD-CHUNK-ENTITY.
    *> A map of block state indices to palette indices
    78 PALETTE-CAPACITY VALUE 100000.
    01 PALETTE-INDICES.
        02 PALETTE-INDEX OCCURS PALETTE-CAPACITY TIMES BINARY-SHORT UNSIGNED.
    *> entity data
    01 ENTITY-PTR               POINTER.
    01 SERIALIZE-PTR            PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-CHUNK-INDEX           BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-INDEX LK-FAILURE.
    IF BIOME-REGISTRY-ID < 0
        CALL "Registries-LookupRegistry" USING "minecraft:worldgen/biome" BIOME-REGISTRY-ID
        CALL "Registries-LookupRegistry" USING "minecraft:block_entity_type" BLOCK-ENTITY-REGISTRY-ID
        CALL "Registries-LookupRegistry" USING "minecraft:entity_type" ENTITY-REGISTRY-ID
    END-IF

    SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(LK-CHUNK-INDEX)
    MOVE 0 TO LK-FAILURE

    IF CHUNK-DIRTY-BLOCKS > 0
        INITIALIZE NBTENC
        PERFORM SaveChunkRegion
    END-IF

    IF LK-FAILURE = 0 AND CHUNK-DIRTY-ENTITIES > 0
        INITIALIZE NBTENC
        PERFORM SaveChunkEntities
    END-IF

    GOBACK.

SaveChunkRegion.
    *> start root tag
    MOVE 1 TO NBTENC-OFFSET
    CALL "NbtEncode-RootCompound" USING NBTENC BUFFER

    CALL "NbtEncode-Int" USING NBTENC BUFFER "xPos" CHUNK-X
    CALL "NbtEncode-Int" USING NBTENC BUFFER "zPos" CHUNK-Z
    CALL "NbtEncode-Int" USING NBTENC BUFFER "yPos" CHUNK-SECTION-MIN-Y

    *> start chunk sections
    CALL "NbtEncode-List" USING NBTENC BUFFER "sections"

    PERFORM VARYING SECTION-INDEX FROM 1 BY 1 UNTIL SECTION-INDEX > CHUNK-SECTION-COUNT
        *> only write sections that are not entirely air
        *> Note: The official format stores all sections, but it seems unnecessary, so we don't.
        IF CHUNK-SECTION-NON-AIR(SECTION-INDEX) > 0
            *> start section
            CALL "NbtEncode-Compound" USING NBTENC BUFFER OMITTED

            COMPUTE INT8 = SECTION-INDEX - 1 + CHUNK-SECTION-MIN-Y
            CALL "NbtEncode-Byte" USING NBTENC BUFFER "Y" INT8

            *> block states - palette and data
            CALL "NbtEncode-Compound" USING NBTENC BUFFER "block_states"

            *> palette
            CALL "NbtEncode-List" USING NBTENC BUFFER "palette"

            MOVE 0 TO PALETTE-LENGTH
            INITIALIZE PALETTE-INDICES

            PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > 4096
                *> If the block is not in the palette, add it
                MOVE CHUNK-SECTION-BLOCK(SECTION-INDEX, BLOCK-INDEX) TO CURRENT-BLOCK-ID
                IF PALETTE-INDEX(CURRENT-BLOCK-ID + 1) = 0
                    ADD 1 TO PALETTE-LENGTH
                    MOVE PALETTE-LENGTH TO PALETTE-INDEX(CURRENT-BLOCK-ID + 1)
                    CALL "Blocks-ToDescription" USING CURRENT-BLOCK-ID PALETTE-BLOCK-DESCRIPTION

                    CALL "NbtEncode-Compound" USING NBTENC BUFFER OMITTED

                    MOVE FUNCTION STORED-CHAR-LENGTH(PALETTE-BLOCK-NAME) TO LEN
                    CALL "NbtEncode-String" USING NBTENC BUFFER "Name" PALETTE-BLOCK-NAME LEN

                    IF PALETTE-BLOCK-PROPERTY-COUNT > 0
                        CALL "NbtEncode-Compound" USING NBTENC BUFFER "Properties"
                        PERFORM VARYING PROPERTY-INDEX FROM 1 BY 1 UNTIL PROPERTY-INDEX > PALETTE-BLOCK-PROPERTY-COUNT
                            MOVE PALETTE-BLOCK-PROPERTY-NAME(PROPERTY-INDEX) TO TAG
                            MOVE PALETTE-BLOCK-PROPERTY-VALUE(PROPERTY-INDEX) TO STR
                            MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO LEN
                            CALL "NbtEncode-String" USING NBTENC BUFFER TAG(1:FUNCTION STORED-CHAR-LENGTH(TAG)) STR LEN
                        END-PERFORM
                        CALL "NbtEncode-EndCompound" USING NBTENC BUFFER
                    END-IF

                    CALL "NbtEncode-EndCompound" USING NBTENC BUFFER
                END-IF
            END-PERFORM

            *> end palette
            CALL "NbtEncode-EndList" USING NBTENC BUFFER

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
                CALL "NbtEncode-LongArray" USING NBTENC BUFFER "data" LONG-ARRAY-LENGTH
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
                    CALL "NbtEncode-Long" USING NBTENC BUFFER OMITTED LONG-ARRAY-ENTRY-SIGNED
                END-PERFORM
            END-IF

            *> end block states
            CALL "NbtEncode-EndCompound" USING NBTENC BUFFER

            *> biomes - palette and data
            CALL "NbtEncode-Compound" USING NBTENC BUFFER "biomes"

            *> palette
            CALL "NbtEncode-List" USING NBTENC BUFFER "palette"

            MOVE 0 TO PALETTE-LENGTH
            INITIALIZE PALETTE-INDICES

            PERFORM VARYING BIOME-INDEX FROM 1 BY 1 UNTIL BIOME-INDEX > 64
                *> If the biome is not in the palette, add it
                MOVE CHUNK-SECTION-BIOME(SECTION-INDEX, BIOME-INDEX) TO CURRENT-BIOME-ID
                IF PALETTE-INDEX(CURRENT-BIOME-ID + 1) = 0
                    ADD 1 TO PALETTE-LENGTH
                    MOVE PALETTE-LENGTH TO PALETTE-INDEX(CURRENT-BIOME-ID + 1)
                    CALL "Registries-EntryName" USING BIOME-REGISTRY-ID CURRENT-BIOME-ID STR
                    MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO LEN
                    CALL "NbtEncode-String" USING NBTENC BUFFER OMITTED STR LEN
                END-IF
            END-PERFORM

            *> end palette
            CALL "NbtEncode-EndList" USING NBTENC BUFFER

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
                CALL "NbtEncode-LongArray" USING NBTENC BUFFER "data" LONG-ARRAY-LENGTH
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
                    CALL "NbtEncode-Long" USING NBTENC BUFFER OMITTED LONG-ARRAY-ENTRY-SIGNED
                END-PERFORM
            END-IF

            *> end biomes
            CALL "NbtEncode-EndCompound" USING NBTENC BUFFER

            *> end section
            CALL "NbtEncode-EndCompound" USING NBTENC BUFFER
        END-IF
    END-PERFORM

    *> end chunk sections
    CALL "NbtEncode-EndList" USING NBTENC BUFFER

    *> start block entities
    CALL "NbtEncode-List" USING NBTENC BUFFER "block_entities"

    MOVE 0 TO BLOCK-ENTITY-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-ENTITY-COUNT >= CHUNK-BLOCK-ENTITY-COUNT
        IF CHUNK-BLOCK-ENTITY-ID(BLOCK-INDEX) >= 0
            ADD 1 TO BLOCK-ENTITY-COUNT

            *> start block entity
            CALL "NbtEncode-Compound" USING NBTENC BUFFER OMITTED

            MOVE CHUNK-BLOCK-ENTITY-ID(BLOCK-INDEX) TO INT32
            SET SERIALIZE-PTR TO CB-PTR-BLOCK-ENTITY-SERIALIZE(INT32 + 1)

            CALL "Registries-EntryName" USING BLOCK-ENTITY-REGISTRY-ID INT32 STR
            MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO LEN
            CALL "NbtEncode-String" USING NBTENC BUFFER "id" STR LEN

            *> x, y, z
            SUBTRACT 1 FROM BLOCK-INDEX GIVING INT32
            DIVIDE INT32 BY 16 GIVING INT32 REMAINDER BLOCK-ENTITY-X
            DIVIDE INT32 BY 16 GIVING INT32 REMAINDER BLOCK-ENTITY-Z
            SUBTRACT 64 FROM INT32 GIVING BLOCK-ENTITY-Y
            *> x and z are in the world coordinate system
            COMPUTE BLOCK-ENTITY-X = BLOCK-ENTITY-X + CHUNK-X * 16
            COMPUTE BLOCK-ENTITY-Z = BLOCK-ENTITY-Z + CHUNK-Z * 16

            CALL "NbtEncode-Int" USING NBTENC BUFFER "x" BLOCK-ENTITY-X
            CALL "NbtEncode-Int" USING NBTENC BUFFER "y" BLOCK-ENTITY-Y
            CALL "NbtEncode-Int" USING NBTENC BUFFER "z" BLOCK-ENTITY-Z

            *> block entity data
            IF SERIALIZE-PTR NOT = NULL
                CALL SERIALIZE-PTR USING CHUNK-BLOCK-ENTITY(BLOCK-INDEX) NBTENC BUFFER
            END-IF

            *> end block entity
            CALL "NbtEncode-EndCompound" USING NBTENC BUFFER
        END-IF
    END-PERFORM

    *> end block entities
    CALL "NbtEncode-EndList" USING NBTENC BUFFER

    *> end root tag
    CALL "NbtEncode-EndCompound" USING NBTENC BUFFER

    *> Save the chunk
    COMPUTE BUFFER-LENGTH = NBTENC-OFFSET - 1
    MOVE FILE-TYPE-REGION TO FILE-TYPE
    CALL "Region-WriteChunkData" USING FILE-TYPE CHUNK-X CHUNK-Z BUFFER BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    MOVE 0 TO CHUNK-DIRTY-BLOCKS
    .

SaveChunkEntities.
    *> Save the chunk entities to the entity data file

    *> start root tag
    MOVE 1 TO NBTENC-OFFSET
    CALL "NbtEncode-RootCompound" USING NBTENC BUFFER

    *> chunk position
    MOVE 2 TO INT32
    CALL "NbtEncode-IntArray" USING NBTENC BUFFER "Position" INT32
    CALL "NbtEncode-Int" USING NBTENC BUFFER OMITTED CHUNK-X
    CALL "NbtEncode-Int" USING NBTENC BUFFER OMITTED CHUNK-Z

    *> start entities
    CALL "NbtEncode-List" USING NBTENC BUFFER "Entities"

    SET ENTITY-PTR TO CHUNK-ENTITY-LIST
    PERFORM UNTIL ENTITY-PTR = NULL
        SET ADDRESS OF ENTITY-LIST TO ENTITY-PTR

        CALL "NbtEncode-Compound" USING NBTENC BUFFER OMITTED

        CALL "Registries-EntryName" USING ENTITY-REGISTRY-ID ENTITY-TYPE STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO LEN
        CALL "NbtEncode-String" USING NBTENC BUFFER "id" STR LEN

        CALL "GetCallback-EntitySerialize" USING ENTITY-TYPE SERIALIZE-PTR
        IF SERIALIZE-PTR NOT = NULL
            CALL SERIALIZE-PTR USING ENTITY-LIST-ENTITY NBTENC BUFFER
        END-IF

        CALL "NbtEncode-EndCompound" USING NBTENC BUFFER

        SET ENTITY-PTR TO ENTITY-LIST-NEXT
    END-PERFORM

    *> end entities
    CALL "NbtEncode-EndList" USING NBTENC BUFFER

    *> end root tag
    CALL "NbtEncode-EndCompound" USING NBTENC BUFFER

    *> Save the entity data
    COMPUTE BUFFER-LENGTH = NBTENC-OFFSET - 1
    MOVE FILE-TYPE-ENTITY TO FILE-TYPE
    CALL "Region-WriteChunkData" USING FILE-TYPE CHUNK-X CHUNK-Z BUFFER BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    MOVE 0 TO CHUNK-DIRTY-ENTITIES
    .

END PROGRAM World-SaveChunk.

*> --- World-LoadChunk ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-LoadChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CALLBACKS.
    COPY DD-REGION-FILES.
    01 FILE-TYPE                BINARY-CHAR UNSIGNED.
    01 BUFFER                   PIC X(1048576).
    01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
    COPY DD-NBT-DECODER.
    *> Temporary variables
    01 SEEK-FOUND               BINARY-LONG UNSIGNED.
    COPY DD-NBT-DECODER REPLACING LEADING ==NBTDEC== BY ==SEEK-NBTDEC==.
    01 AT-END                   BINARY-CHAR UNSIGNED.
    01 TAG                      PIC X(256).
    01 STR                      PIC X(256).
    01 LEN                      BINARY-LONG UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT32                    BINARY-LONG.
    01 CHUNK-STATUS             PIC X(64).
    01 CHUNK-STATUS-ID          BINARY-LONG.
    01 ACCEPTED-CHUNK-STATUS-ID BINARY-LONG.
    01 X-POS                    BINARY-LONG.
    01 Z-POS                    BINARY-LONG.
    01 DID-DECODE-SECTIONS      BINARY-CHAR UNSIGNED.
    01 CHUNK-SECTION-MIN-Y      BINARY-LONG.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 LOADED-SECTION-COUNT     BINARY-LONG UNSIGNED.
    01 SECTION-INDEX            BINARY-LONG UNSIGNED.
    *> block entity data
    01 BLOCK-ENTITY-COUNT       BINARY-LONG UNSIGNED.
    01 BLOCK-ENTITY-ID          BINARY-LONG.
    01 BLOCK-ENTITY-X           BINARY-LONG.
    01 BLOCK-ENTITY-Y           BINARY-LONG.
    01 BLOCK-ENTITY-Z           BINARY-LONG.
    *> World data
    COPY DD-WORLD.
    COPY DD-CHUNK-REF.
    COPY DD-CHUNK-ENTITY.
    *> entity data
    01 ENTITY-PTR               POINTER.
    01 ALLOCATE-PTR             PROGRAM-POINTER.
    01 DESERIALIZE-PTR          PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-CHUNK-X               BINARY-LONG.
    01 LK-CHUNK-Z               BINARY-LONG.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CHUNK-X LK-CHUNK-Z LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    INITIALIZE NBTDEC
    PERFORM LoadChunkRegion
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    INITIALIZE NBTDEC
    PERFORM LoadChunkEntities
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    *> Success
    MOVE 0 TO CHUNK-DIRTY-BLOCKS
    MOVE 0 TO CHUNK-DIRTY-ENTITIES

    GOBACK.

LoadChunkRegion.
    MOVE FILE-TYPE-REGION TO FILE-TYPE

    CALL "Region-ReadChunkData" USING FILE-TYPE LK-CHUNK-X LK-CHUNK-Z BUFFER BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0 OR BUFFER-LENGTH = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> start root tag
    MOVE 1 TO NBTDEC-OFFSET
    CALL "NbtDecode-RootCompound" USING NBTDEC BUFFER

    MOVE SPACES TO CHUNK-STATUS

    *> Do a first pass to get the chunk X, Z, and Y values, as well as the chunk generation status.
    *> The way we write NBT, they should come before any larger pieces of data, but this is not strictly guaranteed.
    MOVE NBTDEC TO SEEK-NBTDEC
    MOVE 0 TO SEEK-FOUND
    PERFORM UNTIL EXIT
        CALL "NbtDecode-Peek" USING SEEK-NBTDEC BUFFER AT-END TAG
        IF AT-END > 0
            EXIT PERFORM
        END-IF
        EVALUATE TAG
            WHEN "xPos"
                CALL "NbtDecode-Int" USING SEEK-NBTDEC BUFFER X-POS
                ADD 1 TO SEEK-FOUND
            WHEN "zPos"
                CALL "NbtDecode-Int" USING SEEK-NBTDEC BUFFER Z-POS
                ADD 1 TO SEEK-FOUND
            WHEN "yPos"
                CALL "NbtDecode-Int" USING SEEK-NBTDEC BUFFER CHUNK-SECTION-MIN-Y
                ADD 1 TO SEEK-FOUND
            WHEN "Status"
                CALL "NbtDecode-String" USING SEEK-NBTDEC BUFFER STR LEN
                MOVE STR(1:LEN) TO CHUNK-STATUS
            WHEN OTHER
                CALL "NbtDecode-Skip" USING SEEK-NBTDEC BUFFER
        END-EVALUATE
        IF SEEK-FOUND = 3
            EXIT PERFORM
        END-IF
    END-PERFORM

    *> We are unable to continue generating partially-generated chunks ("proto-chunks"). Since we don't require light
    *> or heightmap data, any chunk that has progressed to at least the "surface" stage is acceptable.
    IF CHUNK-STATUS NOT = SPACES
        CALL "Registries-Lookup" USING "minecraft:chunk_status" CHUNK-STATUS CHUNK-STATUS-ID
        IF CHUNK-STATUS-ID < 0
            DISPLAY "Unknown chunk status: " FUNCTION TRIM(CHUNK-STATUS)
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
        CALL "Registries-Lookup" USING "minecraft:chunk_status" "minecraft:surface" ACCEPTED-CHUNK-STATUS-ID
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

    *> Decode NBT
    MOVE 0 TO DID-DECODE-SECTIONS
    PERFORM UNTIL EXIT
        CALL "NbtDecode-Peek" USING NBTDEC BUFFER AT-END TAG
        EVALUATE TRUE
            WHEN AT-END > 0
                EXIT PERFORM
            WHEN TAG = "sections"
                PERFORM DecodeSections
                MOVE 1 TO DID-DECODE-SECTIONS
            WHEN TAG = "block_entities"
                PERFORM DecodeBlockEntities
            WHEN OTHER
                CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        END-EVALUATE
    END-PERFORM

    IF DID-DECODE-SECTIONS = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    CALL "NbtDecode-SkipRemainingTags" USING NBTDEC BUFFER

    *> end root tag
    CALL "NbtDecode-EndCompound" USING NBTDEC BUFFER
    .

DecodeSections.
    *> start sections
    CALL "NbtDecode-List" USING NBTDEC BUFFER LOADED-SECTION-COUNT

    PERFORM LOADED-SECTION-COUNT TIMES
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER

        MOVE NBTDEC TO SEEK-NBTDEC
        CALL "DecodeSectionY" USING SEEK-NBTDEC BUFFER INT8 LK-FAILURE
        IF LK-FAILURE > 0
            GOBACK
        END-IF
        COMPUTE SECTION-INDEX = INT8 + 1 - CHUNK-SECTION-MIN-Y

        MOVE NBTDEC TO SEEK-NBTDEC
        CALL "DecodeSectionBlockStates" USING SEEK-NBTDEC BUFFER CHUNK-SECTION-BLOCKS(SECTION-INDEX)
                CHUNK-SECTION-NON-AIR(SECTION-INDEX) LK-FAILURE
        IF LK-FAILURE > 0
            GOBACK
        END-IF

        CALL "DecodeSectionBiomes" USING NBTDEC BUFFER CHUNK-SECTION-BIOMES(SECTION-INDEX) LK-FAILURE
        IF LK-FAILURE > 0
            GOBACK
        END-IF

        CALL "NbtDecode-SkipRemainingTags" USING NBTDEC BUFFER
        CALL "NbtDecode-EndCompound" USING NBTDEC BUFFER
    END-PERFORM

    *> end sections
    CALL "NbtDecode-EndList" USING NBTDEC BUFFER
    .

DecodeBlockEntities.
    *> start block entities
    CALL "NbtDecode-List" USING NBTDEC BUFFER BLOCK-ENTITY-COUNT

    PERFORM BLOCK-ENTITY-COUNT TIMES
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER

        *> Backup the NBT decoder so we can decode the block entity twice
        MOVE NBTDEC TO SEEK-NBTDEC

        *> Decode once to find the coordinates and type
        PERFORM UNTIL EXIT
            CALL "NbtDecode-Peek" USING NBTDEC BUFFER AT-END TAG
            IF AT-END > 0
                EXIT PERFORM
            END-IF
            EVALUATE TAG
                WHEN "id"
                    CALL "NbtDecode-String" USING NBTDEC BUFFER STR LEN
                    CALL "Registries-Lookup" USING "minecraft:block_entity_type" STR(1:LEN) BLOCK-ENTITY-ID
                WHEN "x"
                    CALL "NbtDecode-Int" USING NBTDEC BUFFER BLOCK-ENTITY-X
                WHEN "y"
                    CALL "NbtDecode-Int" USING NBTDEC BUFFER BLOCK-ENTITY-Y
                WHEN "z"
                    CALL "NbtDecode-Int" USING NBTDEC BUFFER BLOCK-ENTITY-Z
                WHEN OTHER
                    CALL "NbtDecode-Skip" USING NBTDEC BUFFER
            END-EVALUATE
        END-PERFORM

        *> convert to chunk-relative coordinates
        COMPUTE INT32 = FUNCTION MOD(BLOCK-ENTITY-X, 16) + 16 * (FUNCTION MOD(BLOCK-ENTITY-Z, 16) + 16 * (BLOCK-ENTITY-Y + 64)) + 1
        MOVE BLOCK-ENTITY-ID TO CHUNK-BLOCK-ENTITY-ID(INT32)
        ADD 1 TO CHUNK-BLOCK-ENTITY-COUNT

        *> Find the allocation and deserialize routines
        SET ALLOCATE-PTR TO CB-PTR-BLOCK-ENTITY-ALLOCATE(BLOCK-ENTITY-ID + 1)
        SET DESERIALIZE-PTR TO CB-PTR-BLOCK-ENTITY-DESERIALIZE(BLOCK-ENTITY-ID + 1)

        IF ALLOCATE-PTR NOT = NULL
            CALL ALLOCATE-PTR USING CHUNK-BLOCK-ENTITY-DATA(INT32)

            *> Deserialize the block entity
            IF DESERIALIZE-PTR NOT = NULL
                MOVE SEEK-NBTDEC TO NBTDEC
                PERFORM UNTIL EXIT
                    CALL "NbtDecode-Peek" USING NBTDEC BUFFER AT-END TAG
                    EVALUATE TRUE
                        WHEN AT-END > 0
                            EXIT PERFORM
                        WHEN TAG = "id" OR "x" OR "y" OR "z"
                            CALL "NbtDecode-Skip" USING NBTDEC BUFFER
                        WHEN OTHER
                            CALL DESERIALIZE-PTR USING CHUNK-BLOCK-ENTITY(INT32) NBTDEC BUFFER TAG
                    END-EVALUATE
                END-PERFORM
            END-IF
        END-IF

        CALL "NbtDecode-EndCompound" USING NBTDEC BUFFER
    END-PERFORM

    *> end block entities
    CALL "NbtDecode-EndList" USING NBTDEC BUFFER
    .

LoadChunkEntities.
    *> Load the chunk entities from the entity data file
    MOVE FILE-TYPE-ENTITY TO FILE-TYPE

    CALL "Region-ReadChunkData" USING FILE-TYPE LK-CHUNK-X LK-CHUNK-Z BUFFER BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0 OR BUFFER-LENGTH = 0
        *> It is not an error if the entity data file does not exist
        MOVE 0 TO LK-FAILURE
        GOBACK
    END-IF

    *> start root tag
    MOVE 1 TO NBTDEC-OFFSET
    CALL "NbtDecode-RootCompound" USING NBTDEC BUFFER

    *> Skip ahead until we find the entities tag.
    CALL "NbtDecode-SkipUntilTag" USING NBTDEC BUFFER "Entities" AT-END
    IF AT-END NOT = 0
        GOBACK
    END-IF

    *> start entities
    CALL "NbtDecode-List" USING NBTDEC BUFFER CHUNK-ENTITY-COUNT

    *> Load in the same order as they were saved
    PERFORM CHUNK-ENTITY-COUNT TIMES
        IF CHUNK-ENTITY-LIST = NULL
            *> First entity: allocate and set as list head
            ALLOCATE ENTITY-LIST
            INITIALIZE ENTITY-LIST
            SET ENTITY-PTR TO ADDRESS OF ENTITY-LIST
            SET CHUNK-ENTITY-LIST TO ADDRESS OF ENTITY-LIST
        ELSE
            *> Subsequent entities: allocate into next pointer
            ALLOCATE LENGTH OF ENTITY-LIST CHARACTERS RETURNING ENTITY-LIST-NEXT
            SET ADDRESS OF ENTITY-LIST TO ENTITY-LIST-NEXT
            INITIALIZE ENTITY-LIST
            SET ENTITY-PTR TO ENTITY-LIST-NEXT
        END-IF

        *> start entity
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER

        *> find the entity type
        MOVE NBTDEC TO SEEK-NBTDEC
        CALL "NbtDecode-SkipUntilTag" USING SEEK-NBTDEC BUFFER "id" AT-END
        IF AT-END > 0
            DISPLAY "ERROR: Entity missing id tag"
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        CALL "NbtDecode-String" USING SEEK-NBTDEC BUFFER STR LEN
        CALL "Registries-Lookup" USING "minecraft:entity_type" STR(1:LEN) ENTITY-TYPE
        IF ENTITY-TYPE < 0
            DISPLAY "ERROR: Unknown entity type: " FUNCTION TRIM(STR(1:LEN))
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        *> Initialize default fields
        CALL "World-NextEntityId" USING ENTITY-ID
        MOVE CHUNK-X TO ENTITY-CHUNK-X
        MOVE CHUNK-Z TO ENTITY-CHUNK-Z

        *> TODO remove these item entity-specific default fields
        MOVE 2 TO ENTITY-ITEM-SLOT-NBT-LENGTH
        MOVE X"0000" TO ENTITY-ITEM-SLOT-NBT-DATA

        *> obtain its deserialize routine
        CALL "GetCallback-EntityDeserialize" USING ENTITY-TYPE DESERIALIZE-PTR
        IF DESERIALIZE-PTR = NULL
            DISPLAY "No deserialize routine for entity type " ENTITY-TYPE
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        *> deserialize the entity
        PERFORM UNTIL EXIT
            CALL "NbtDecode-Peek" USING NBTDEC BUFFER AT-END TAG
            IF AT-END > 0
                EXIT PERFORM
            END-IF
            CALL DESERIALIZE-PTR USING ENTITY-LIST-ENTITY NBTDEC BUFFER TAG
        END-PERFORM

        *> end entity
        CALL "NbtDecode-EndCompound" USING NBTDEC BUFFER
    END-PERFORM

    *> end entities
    CALL "NbtDecode-EndList" USING NBTDEC BUFFER

    *> end root tag
    CALL "NbtDecode-SkipRemainingTags" USING NBTDEC BUFFER
    CALL "NbtDecode-EndCompound" USING NBTDEC BUFFER
    .

    *> --- DecodeSectionY ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. DecodeSectionY.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-NBT-DECODER REPLACING LEADING ==NBTDEC== BY ==LK-NBTDEC==.
        01 LK-BUFFER            PIC X ANY LENGTH.
        01 LK-Y                 BINARY-CHAR.
        01 LK-FAILURE           BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-NBTDEC LK-BUFFER LK-Y LK-FAILURE.
        CALL "NbtDecode-SkipUntilTag" USING LK-NBTDEC LK-BUFFER "Y" LK-FAILURE
        IF LK-FAILURE > 0
            GOBACK
        END-IF
        CALL "NbtDecode-Byte" USING LK-NBTDEC LK-BUFFER LK-Y
        GOBACK.

    END PROGRAM DecodeSectionY.

    *> --- DecodeSectionBlockStates ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. DecodeSectionBlockStates.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER REPLACING LEADING ==NBTDEC== BY ==BEGIN-NBTDEC==.
        01 AT-END                   BINARY-CHAR UNSIGNED.
        01 TAG                      PIC X(256).
        01 STR                      PIC X(256).
        01 LEN                      BINARY-LONG UNSIGNED.
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
        01 BLOCK-STATES             BINARY-LONG UNSIGNED OCCURS 4096 TIMES.
    LINKAGE SECTION.
        COPY DD-NBT-DECODER REPLACING LEADING ==NBTDEC== BY ==LK-NBTDEC==.
        01 LK-BUFFER                PIC X ANY LENGTH.
        01 LK-BLOCKS.
            02 LK-BLOCK-ID          BINARY-LONG UNSIGNED OCCURS 4096 TIMES.
        01 LK-NON-AIR               BINARY-LONG UNSIGNED.
        01 LK-FAILURE               BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-NBTDEC LK-BUFFER LK-BLOCKS LK-NON-AIR LK-FAILURE.
        MOVE 0 TO LK-FAILURE

        CALL "NbtDecode-SkipUntilTag" USING LK-NBTDEC LK-BUFFER "block_states" AT-END
        IF AT-END > 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        *> start block states
        CALL "NbtDecode-Compound" USING LK-NBTDEC LK-BUFFER
        MOVE LK-NBTDEC TO BEGIN-NBTDEC

        *> Skip to the palette
        CALL "NbtDecode-SkipUntilTag" USING LK-NBTDEC LK-BUFFER "palette" AT-END
        IF AT-END > 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        *> start palette
        CALL "NbtDecode-List" USING LK-NBTDEC LK-BUFFER PALETTE-LENGTH

        PERFORM VARYING PALETTE-INDEX FROM 1 BY 1 UNTIL PALETTE-INDEX > PALETTE-LENGTH
            *> start palette entry
            CALL "NbtDecode-Compound" USING LK-NBTDEC LK-BUFFER
            MOVE 0 TO PALETTE-BLOCK-PROPERTY-COUNT

            PERFORM UNTIL EXIT
                CALL "NbtDecode-Peek" USING LK-NBTDEC LK-BUFFER AT-END TAG
                IF AT-END > 0
                    EXIT PERFORM
                END-IF
                EVALUATE TAG
                    WHEN "Name"
                        CALL "NbtDecode-String" USING LK-NBTDEC LK-BUFFER STR LEN
                        MOVE STR(1:LEN) TO PALETTE-BLOCK-NAME

                    WHEN "Properties"
                        CALL "NbtDecode-Compound" USING LK-NBTDEC LK-BUFFER
                        PERFORM UNTIL EXIT
                            CALL "NbtDecode-Peek" USING LK-NBTDEC LK-BUFFER AT-END TAG
                            IF AT-END > 0
                                EXIT PERFORM
                            END-IF
                            ADD 1 TO PALETTE-BLOCK-PROPERTY-COUNT
                            CALL "NbtDecode-String" USING LK-NBTDEC LK-BUFFER STR LEN
                            MOVE TAG TO PALETTE-BLOCK-PROPERTY-NAME(PALETTE-BLOCK-PROPERTY-COUNT)
                            MOVE STR(1:LEN) TO PALETTE-BLOCK-PROPERTY-VALUE(PALETTE-BLOCK-PROPERTY-COUNT)
                        END-PERFORM
                        CALL "NbtDecode-EndCompound" USING LK-NBTDEC LK-BUFFER

                    WHEN OTHER
                        CALL "NbtDecode-Skip" USING LK-NBTDEC LK-BUFFER
                END-EVALUATE
            END-PERFORM

            *> end palette entry
            CALL "NbtDecode-EndCompound" USING LK-NBTDEC LK-BUFFER
            CALL "Blocks-FromDescription" USING PALETTE-BLOCK-DESCRIPTION BLOCK-STATES(PALETTE-INDEX)
        END-PERFORM

        *> end palette
        CALL "NbtDecode-EndList" USING LK-NBTDEC LK-BUFFER

        *> If the palette has length 1, we don't care about the data. In fact, it might not be there.
        IF PALETTE-LENGTH = 1
            *> Fill the section with the singular block state (unless it is air).
            MOVE BLOCK-STATES(1) TO CURRENT-BLOCK-ID
            IF CURRENT-BLOCK-ID > 0
                INITIALIZE LK-BLOCKS REPLACING NUMERIC BY CURRENT-BLOCK-ID
                MOVE 4096 TO LK-NON-AIR
            END-IF
        ELSE
            *> Reset the state to the beginning of the block states compound, since "data" may come before "palette".
            *> We don't write NBT this way, but Minecraft does.
            MOVE BEGIN-NBTDEC TO LK-NBTDEC

            *> Skip to the data
            CALL "NbtDecode-SkipUntilTag" USING LK-NBTDEC LK-BUFFER "data" AT-END
            IF AT-END > 0
                MOVE 1 TO LK-FAILURE
                GOBACK
            END-IF

            *> read packed long array
            CALL "NbtDecode-List" USING LK-NBTDEC LK-BUFFER LONG-ARRAY-LENGTH

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
                CALL "NbtDecode-Long" USING LK-NBTDEC LK-BUFFER LONG-ARRAY-ENTRY-SIGNED
                PERFORM FUNCTION MIN(ENTRIES-PER-LONG, 4096 - BLOCK-INDEX + 1) TIMES
                    >>IF GCVERSION >= 32
                        COMPUTE PALETTE-INDEX = LONG-ARRAY-ENTRY B-AND PALETTE-BITS-POW
                        COMPUTE LONG-ARRAY-ENTRY = LONG-ARRAY-ENTRY B-SHIFT-R PALETTE-BITS
                    >>ELSE
                        DIVIDE LONG-ARRAY-ENTRY BY PALETTE-BITS-POW GIVING LONG-ARRAY-ENTRY REMAINDER PALETTE-INDEX
                    >>END-IF
                    MOVE BLOCK-STATES(PALETTE-INDEX + 1) TO CURRENT-BLOCK-ID
                    IF CURRENT-BLOCK-ID > 0
                        MOVE CURRENT-BLOCK-ID TO LK-BLOCK-ID(BLOCK-INDEX)
                        ADD 1 TO LK-NON-AIR
                    END-IF
                    ADD 1 TO BLOCK-INDEX
                END-PERFORM
            END-PERFORM

            *> end data
            CALL "NbtDecode-EndList" USING LK-NBTDEC LK-BUFFER
        END-IF

        *> end block states
        CALL "NbtDecode-SkipRemainingTags" USING LK-NBTDEC LK-BUFFER
        CALL "NbtDecode-EndCompound" USING LK-NBTDEC LK-BUFFER

        GOBACK.

    END PROGRAM DecodeSectionBlockStates.

    *> --- DecodeSectionBiomes ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. DecodeSectionBiomes.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER REPLACING LEADING ==NBTDEC== BY ==BEGIN-NBTDEC==.
        01 AT-END                   BINARY-CHAR UNSIGNED.
        01 TAG                      PIC X(256).
        01 STR                      PIC X(256).
        01 LEN                      BINARY-LONG UNSIGNED.
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
        COPY DD-NBT-DECODER REPLACING LEADING ==NBTDEC== BY ==LK-NBTDEC==.
        01 LK-BUFFER                PIC X ANY LENGTH.
        01 LK-BIOMES.
            02 LK-BIOME-ID          BINARY-LONG UNSIGNED OCCURS 64 TIMES.
        01 LK-FAILURE               BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-NBTDEC LK-BUFFER LK-BIOMES LK-FAILURE.
        MOVE 0 TO LK-FAILURE

        CALL "NbtDecode-SkipUntilTag" USING LK-NBTDEC LK-BUFFER "biomes" AT-END

        *> Biomes are optional since previous versions of CobolCraft did not write them.
        IF AT-END > 0
            GOBACK
        END-IF

        *> start biomes
        CALL "NbtDecode-Compound" USING LK-NBTDEC LK-BUFFER
        MOVE LK-NBTDEC TO BEGIN-NBTDEC

        *> Skip to the palette
        CALL "NbtDecode-SkipUntilTag" USING LK-NBTDEC LK-BUFFER "palette" AT-END
        IF AT-END > 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        *> palette
        CALL "NbtDecode-List" USING LK-NBTDEC LK-BUFFER PALETTE-LENGTH
        PERFORM VARYING PALETTE-INDEX FROM 1 BY 1 UNTIL PALETTE-INDEX > PALETTE-LENGTH
            CALL "NbtDecode-String" USING LK-NBTDEC LK-BUFFER STR LEN
            CALL "Registries-Lookup" USING "minecraft:worldgen/biome" STR(1:LEN) PALETTE-BIOME-IDS(PALETTE-INDEX)
        END-PERFORM
        CALL "NbtDecode-EndList" USING LK-NBTDEC LK-BUFFER

        *> If the palette has length 1, we don't care about the data. In fact, it might not be there.
        IF PALETTE-LENGTH = 1
            INITIALIZE LK-BIOMES REPLACING NUMERIC BY PALETTE-BIOME-IDS(1)
        ELSE
            *> Reset the state to the beginning of the biomes compound, since "data" may come before "palette".
            MOVE BEGIN-NBTDEC TO LK-NBTDEC

            *> Skip to the data
            CALL "NbtDecode-SkipUntilTag" USING LK-NBTDEC LK-BUFFER "data" AT-END
            IF AT-END > 0
                MOVE 1 TO LK-FAILURE
                GOBACK
            END-IF

            *> read packed long array
            CALL "NbtDecode-List" USING LK-NBTDEC LK-BUFFER LONG-ARRAY-LENGTH

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
                CALL "NbtDecode-Long" USING LK-NBTDEC LK-BUFFER LONG-ARRAY-ENTRY-SIGNED
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
            CALL "NbtDecode-EndList" USING LK-NBTDEC LK-BUFFER
        END-IF

        *> end biomes
        CALL "NbtDecode-SkipRemainingTags" USING LK-NBTDEC LK-BUFFER
        CALL "NbtDecode-EndCompound" USING LK-NBTDEC LK-BUFFER

        GOBACK.

    END PROGRAM DecodeSectionBiomes.

END PROGRAM World-LoadChunk.
