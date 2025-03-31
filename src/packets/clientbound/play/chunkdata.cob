*> --- SendPacket-ChunkData ---
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-ChunkData.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:level_chunk_with_light".
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(512000).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
    *> constants
    01 BLOCK-COUNT              BINARY-SHORT                VALUE 4096.
    01 BLOCK-STATES-LENGTH      BINARY-LONG UNSIGNED        VALUE 4096.
    01 BIOMES-LENGTH            BINARY-LONG UNSIGNED        VALUE 64.
    *> highest possible ID values; determines the number of bits per entry
    01 MAX-BLOCK-STATE       BINARY-LONG UNSIGNED.
    01 MAX-BIOME-ID             BINARY-LONG UNSIGNED.
    *> temporary data
    01 UINT8                    BINARY-CHAR UNSIGNED.
    01 INT16                    BINARY-SHORT.
    01 INT32                    BINARY-LONG.
    01 CHUNK-SEC                BINARY-LONG UNSIGNED.
    01 CHUNK-DATA               PIC X(256000).
    01 CHUNK-DATA-POS           BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX              BINARY-LONG UNSIGNED.
    01 ENTITY-COUNT             BINARY-LONG UNSIGNED.
    01 BLOCK-X                  BINARY-CHAR UNSIGNED.
    01 BLOCK-Z                  BINARY-CHAR UNSIGNED.
    COPY DD-CALLBACKS.
    COPY DD-NBT-ENCODER.
    01 SERIALIZE-PTR            PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-CHUNK-X               BINARY-LONG.
    01 LK-CHUNK-Z               BINARY-LONG.
    01 LK-SECTIONS.
        02 LK-SECTION               OCCURS 24 TIMES.
            03 LK-SECTION-NON-AIR   BINARY-LONG UNSIGNED.
            *> block IDs (16x16x16) - X increases fastest, then Z, then Y
            03 LK-SECTION-BLOCKS.
                04 LK-SECTION-BLOCK OCCURS 4096 TIMES BINARY-LONG UNSIGNED.
            *> biome IDs (4x4x4)
            03 LK-SECTION-BIOMES.
                04 LK-SECTION-BIOME OCCURS 64 TIMES BINARY-LONG UNSIGNED.
    01 LK-BLOCK-ENTITIES.
        02 LK-BLOCK-ENTITY-COUNT    BINARY-LONG UNSIGNED.
        *> block entity IDs for each block
        02 LK-BLOCK-ENTITY          OCCURS 98304 TIMES.
            COPY DD-BLOCK-ENTITY REPLACING LEADING ==BLOCK-ENTITY== BY ==LK-BLOCK-ENTITY==.

PROCEDURE DIVISION USING LK-CLIENT LK-CHUNK-X LK-CHUNK-Z LK-SECTIONS LK-BLOCK-ENTITIES.
    COPY PROC-PACKET-INIT.

    CALL "Blocks-MaximumStateId" USING MAX-BLOCK-STATE

    CALL "Registries-LookupRegistry" USING "minecraft:worldgen/biome" INT32
    CALL "Registries-EntryCount" USING INT32 MAX-BIOME-ID
    SUBTRACT 1 FROM MAX-BIOME-ID

    MOVE 1 TO PAYLOADPOS

    *> chunk x
    CALL "Encode-Int" USING LK-CHUNK-X PAYLOAD PAYLOADPOS

    *> chunk z
    CALL "Encode-Int" USING LK-CHUNK-Z PAYLOAD PAYLOADPOS

    *> heightmap NBT - send an empty compound tag for now
    MOVE X"0a00" TO PAYLOAD(PAYLOADPOS:2)
    ADD 2 TO PAYLOADPOS

    *> construct chunk data
    MOVE 1 TO CHUNK-DATA-POS
    PERFORM VARYING CHUNK-SEC FROM 1 BY 1 UNTIL CHUNK-SEC > 24
        *> block count
        CALL "Encode-Short" USING BLOCK-COUNT CHUNK-DATA CHUNK-DATA-POS
        *> block states
        IF LK-SECTION-NON-AIR(CHUNK-SEC) = 0
            CALL "EncodePalette-SingleValued" USING CHUNK-DATA CHUNK-DATA-POS LK-SECTION-BLOCK(CHUNK-SEC, 1)
        ELSE
            CALL "EncodePalette" USING CHUNK-DATA CHUNK-DATA-POS BLOCK-STATES-LENGTH LK-SECTION-BLOCKS(CHUNK-SEC) MAX-BLOCK-STATE
        END-IF
        *> biomes
        CALL "EncodePalette" USING CHUNK-DATA CHUNK-DATA-POS BIOMES-LENGTH LK-SECTION-BIOMES(CHUNK-SEC) MAX-BIOME-ID
    END-PERFORM

    *> data prefixed by VarInt size
    COMPUTE INT32 = CHUNK-DATA-POS - 1
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
    MOVE CHUNK-DATA(1:INT32) TO PAYLOAD(PAYLOADPOS:INT32)
    ADD INT32 TO PAYLOADPOS

    *> block entities prefixed by count
    CALL "Encode-VarInt" USING LK-BLOCK-ENTITY-COUNT PAYLOAD PAYLOADPOS
    MOVE 0 TO ENTITY-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL ENTITY-COUNT >= LK-BLOCK-ENTITY-COUNT
        IF LK-BLOCK-ENTITY-ID(BLOCK-INDEX) >= 0
            ADD 1 TO ENTITY-COUNT

            *> packed XZ coordinates: (X << 4) | Z
            SUBTRACT 1 FROM BLOCK-INDEX GIVING INT32
            DIVIDE INT32 BY 16 GIVING INT32 REMAINDER BLOCK-X
            DIVIDE INT32 BY 16 GIVING INT32 REMAINDER BLOCK-Z
            COMPUTE UINT8 = BLOCK-X * 16 + BLOCK-Z
            MOVE FUNCTION CHAR(UINT8 + 1) TO PAYLOAD(PAYLOADPOS:1)
            ADD 1 TO PAYLOADPOS

            *> Y coordinate
            COMPUTE INT16 = INT32 - 64
            CALL "Encode-Short" USING INT16 PAYLOAD PAYLOADPOS

            *> block entity type
            MOVE LK-BLOCK-ENTITY-ID(BLOCK-INDEX) TO INT32
            CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

            *> block entity NBT
            SET SERIALIZE-PTR TO CB-PTR-BLOCK-ENTITY-SERIALIZE(INT32 + 1)
            INITIALIZE NBTENC
            MOVE PAYLOADPOS TO NBTENC-OFFSET
            CALL "NbtEncode-Compound" USING NBTENC PAYLOAD OMITTED
            IF SERIALIZE-PTR NOT = NULL
                CALL SERIALIZE-PTR USING LK-BLOCK-ENTITY(BLOCK-INDEX) NBTENC PAYLOAD
            END-IF
            CALL "NbtEncode-EndCompound" USING NBTENC PAYLOAD
            MOVE NBTENC-OFFSET TO PAYLOADPOS
        END-IF
    END-PERFORM

    *> sky light mask
    *> Note: Each 16x16x16 section needs a bit + 1 below the chunk + 1 above the chunk => 26 bits => 1 long.
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
    MOVE ALL X"FF" TO PAYLOAD(PAYLOADPOS:8)
    ADD 8 TO PAYLOADPOS

    *> block light mask
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
    MOVE ALL X"FF" TO PAYLOAD(PAYLOADPOS:8)
    ADD 8 TO PAYLOADPOS

    *> empty sky light mask
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
    MOVE ALL X"00" TO PAYLOAD(PAYLOADPOS:8)
    ADD 8 TO PAYLOADPOS

    *> empty block light mask
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
    MOVE ALL X"00" TO PAYLOAD(PAYLOADPOS:8)
    ADD 8 TO PAYLOADPOS

    *> sky light array count
    MOVE 26 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> sky light arrays
    MOVE 2048 TO INT32
    PERFORM 26 TIMES
        *> array length
        CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
        *> array data (light values - half a byte per block)
        MOVE ALL X"FF" TO PAYLOAD(PAYLOADPOS:2048)
        ADD 2048 TO PAYLOADPOS
    END-PERFORM

    *> block light array count
    MOVE 26 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> block light arrays
    MOVE 2048 TO INT32
    PERFORM 26 TIMES
        *> array length
        CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
        *> array data (light values - half a byte per block)
        MOVE ALL X"FF" TO PAYLOAD(PAYLOADPOS:2048)
        ADD 2048 TO PAYLOADPOS
    END-PERFORM

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

    *> --- EncodePalette-SingleValued ----
    IDENTIFICATION DIVISION.
    PROGRAM-ID. EncodePalette-SingleValued.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-BUFFER                PIC X ANY LENGTH.
        01 LK-BUFFERPOS             BINARY-LONG UNSIGNED.
        01 LK-PALETTE-VALUE         BINARY-LONG.

    PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-PALETTE-VALUE.
        *> bits per entry: 0 = single-valued
        MOVE X"00" TO LK-BUFFER(LK-BUFFERPOS:1)
        ADD 1 TO LK-BUFFERPOS
        *> palette entry
        CALL "Encode-VarInt" USING LK-PALETTE-VALUE LK-BUFFER LK-BUFFERPOS
        *> data array length
        MOVE X"00" TO LK-BUFFER(LK-BUFFERPOS:1)
        ADD 1 TO LK-BUFFERPOS
        GOBACK.

    END PROGRAM EncodePalette-SingleValued.

    *> --- EncodePalette ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. EncodePalette.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        *> palette entries are 8-byte signed integers
        01 PALETTE-ENTRY-LENGTH     BINARY-LONG UNSIGNED        VALUE 8.
        01 TEMP-UINT32              BINARY-LONG UNSIGNED.
        01 BITS-PER-ENTRY           BINARY-CHAR UNSIGNED.
        01 IDS-PER-ENTRY            BINARY-LONG UNSIGNED.
        01 DATA-ARRAY-LENGTH        BINARY-LONG.
        01 IDX                      BINARY-LONG UNSIGNED.
        01 SHIFT-MULTIPLIER         BINARY-LONG-LONG UNSIGNED.
        01 CURRENT-MULTIPLIER       BINARY-LONG-LONG UNSIGNED.
        01 LONG-BYTES.
            02 LONG                 PIC 9(18) USAGE COMP-X.
    LINKAGE SECTION.
        01 LK-BUFFER                PIC X ANY LENGTH.
        01 LK-BUFFERPOS             BINARY-LONG UNSIGNED.
        01 LK-IDS-LENGTH            BINARY-LONG UNSIGNED.
        01 LK-IDS.
            02 LK-ID OCCURS 1 TO 4096 TIMES DEPENDING ON LK-IDS-LENGTH BINARY-LONG UNSIGNED.
        01 LK-MAX-ID                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-IDS-LENGTH LK-IDS LK-MAX-ID.
        *> TODO: implement indirect palette to save bandwidth

        *> Direct palette is indicated by bits per entry = ceil(log2(id count))
        CALL "LeadingZeros32" USING LK-MAX-ID TEMP-UINT32
        COMPUTE BITS-PER-ENTRY = 32 - TEMP-UINT32

        MOVE FUNCTION CHAR(BITS-PER-ENTRY + 1) TO LK-BUFFER(LK-BUFFERPOS:1)
        ADD 1 TO LK-BUFFERPOS

        *> Here would be the palette (list of unique IDs), but this is empty for direct palettes.

        *> The data array length is the number of longs (8-byte integers) needed to store all IDs.
        *>     => ceil((length of ids table) / floor((64 bits per long) / (bits per entry)))
        DIVIDE 64 BY BITS-PER-ENTRY GIVING IDS-PER-ENTRY
        DIVIDE LK-IDS-LENGTH BY IDS-PER-ENTRY GIVING DATA-ARRAY-LENGTH ROUNDED MODE IS TOWARD-GREATER
        CALL "Encode-VarInt" USING DATA-ARRAY-LENGTH LK-BUFFER LK-BUFFERPOS

        *> Data array: pack as many IDs as possible into each long starting from the least significant bit
        >>IF GCVERSION >= 32
            MOVE 1 TO IDX
            PERFORM DATA-ARRAY-LENGTH TIMES
                MOVE 0 TO LONG
                MOVE 0 TO CURRENT-MULTIPLIER
                PERFORM FUNCTION MIN(IDS-PER-ENTRY, LK-IDS-LENGTH + 1 - IDX) TIMES
                    COMPUTE LONG = LONG B-OR (LK-ID(IDX) B-SHIFT-L CURRENT-MULTIPLIER)
                    COMPUTE CURRENT-MULTIPLIER = CURRENT-MULTIPLIER + BITS-PER-ENTRY
                    ADD 1 TO IDX
                END-PERFORM
                *> Encode-UnsignedLong (inlined for performance)
                MOVE LONG-BYTES TO LK-BUFFER(LK-BUFFERPOS:PALETTE-ENTRY-LENGTH)
                ADD PALETTE-ENTRY-LENGTH TO LK-BUFFERPOS
            END-PERFORM
        >>ELSE
            MOVE 1 TO IDX
            COMPUTE SHIFT-MULTIPLIER = 2 ** BITS-PER-ENTRY
            PERFORM DATA-ARRAY-LENGTH TIMES
                MOVE 0 TO LONG
                MOVE 1 TO CURRENT-MULTIPLIER
                PERFORM FUNCTION MIN(IDS-PER-ENTRY, LK-IDS-LENGTH + 1 - IDX) TIMES
                    COMPUTE LONG = LONG + LK-ID(IDX) * CURRENT-MULTIPLIER
                    COMPUTE CURRENT-MULTIPLIER = CURRENT-MULTIPLIER * SHIFT-MULTIPLIER
                    ADD 1 TO IDX
                END-PERFORM
                *> Encode-UnsignedLong (inlined for performance)
                MOVE LONG-BYTES TO LK-BUFFER(LK-BUFFERPOS:PALETTE-ENTRY-LENGTH)
                ADD PALETTE-ENTRY-LENGTH TO LK-BUFFERPOS
            END-PERFORM
        >>END-IF

        GOBACK.

    END PROGRAM EncodePalette.

END PROGRAM SendPacket-ChunkData.
