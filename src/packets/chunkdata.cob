*> --- SendPacket-ChunkData ---
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-ChunkData.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'28'.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(512000).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 UINT8            BINARY-CHAR UNSIGNED.
    01 INT16            BINARY-SHORT.
    01 INT32            BINARY-LONG.
    01 CHUNK-SEC        BINARY-LONG UNSIGNED.
    01 CHUNK-DATA       PIC X(256000).
    01 CHUNK-DATA-POS   BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX      BINARY-LONG UNSIGNED.
    01 ENTITY-COUNT     BINARY-LONG UNSIGNED.
    01 BLOCK-X          BINARY-CHAR UNSIGNED.
    01 BLOCK-Z          BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-CHUNK.
        02 LK-CHUNK-PRESENT BINARY-CHAR UNSIGNED.
        02 LK-CHUNK-DIRTY   BINARY-CHAR UNSIGNED.
        02 LK-CHUNK-X       BINARY-LONG.
        02 LK-CHUNK-Z       BINARY-LONG.
        02 LK-CHUNK-SECTION OCCURS 24 TIMES.
            03 LK-NON-AIR       BINARY-LONG UNSIGNED.
            *> block IDs (16x16x16) - X increases fastest, then Z, then Y
            03 LK-BLOCK OCCURS 4096 TIMES.
                04 LK-BLOCK-ID      BINARY-LONG UNSIGNED.
            *> biome IDs (4x4x4)
            03 LK-BIOME OCCURS 64 TIMES.
                04 LK-BIOME-ID      BINARY-LONG UNSIGNED.
        02 LK-CHUNK-BLOCK-ENTITY-COUNT BINARY-LONG UNSIGNED.
        *> block entity IDs for each block
        02 LK-CHUNK-BLOCK-ENTITIES.
            *> a value < 0 indicates no entity
            03 LK-CHUNK-BLOCK-ENTITY-ID OCCURS 98304 TIMES BINARY-CHAR.

PROCEDURE DIVISION USING LK-CLIENT LK-CHUNK.
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
    PERFORM VARYING CHUNK-SEC FROM 0 BY 1 UNTIL CHUNK-SEC >= 24
        CALL "EncodeChunkSection" USING CHUNK-SEC LK-CHUNK-SECTION(CHUNK-SEC + 1) CHUNK-DATA CHUNK-DATA-POS
    END-PERFORM

    *> data prefixed by VarInt size
    COMPUTE INT32 = CHUNK-DATA-POS - 1
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
    MOVE CHUNK-DATA(1:INT32) TO PAYLOAD(PAYLOADPOS:INT32)
    ADD INT32 TO PAYLOADPOS

    *> block entities prefixed by count
    CALL "Encode-VarInt" USING LK-CHUNK-BLOCK-ENTITY-COUNT PAYLOAD PAYLOADPOS
    IF LK-CHUNK-BLOCK-ENTITY-COUNT > 0
        MOVE 1 TO BLOCK-INDEX
        MOVE 0 TO ENTITY-COUNT
        PERFORM 98304 TIMES
            IF LK-CHUNK-BLOCK-ENTITY-ID(BLOCK-INDEX) >= 0
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
                MOVE LK-CHUNK-BLOCK-ENTITY-ID(BLOCK-INDEX) TO INT32
                CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

                *> block entity NBT - send an empty compound tag for now
                *> TODO: implement block entity NBT
                MOVE X"0a00" TO PAYLOAD(PAYLOADPOS:2)
                ADD 2 TO PAYLOADPOS

                *> stop the loop once all entities have been sent
                ADD 1 TO ENTITY-COUNT
                IF ENTITY-COUNT >= LK-CHUNK-BLOCK-ENTITY-COUNT
                    EXIT PERFORM
                END-IF
            END-IF
            ADD 1 TO BLOCK-INDEX
        END-PERFORM
    END-IF

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

    *> --- EncodeChunkSection ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. EncodeChunkSection.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 INT16                BINARY-SHORT.
        01 INT32                BINARY-LONG.
        01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
        01 BIOME-INDEX          BINARY-LONG UNSIGNED.
        01 PALETTE-ENTRY-LENGTH BINARY-LONG UNSIGNED        VALUE 8.
        01 PALETTE-ENTRY        BINARY-LONG-LONG UNSIGNED.
        01 PALETTE-ENTRY-BYTES  REDEFINES PALETTE-ENTRY PIC X(8).
    LINKAGE SECTION.
        01 LK-CHUNK-SEC         BINARY-LONG UNSIGNED.
        01 LK-SECTION.
            02 LK-NON-AIR           BINARY-LONG UNSIGNED.
            02 LK-BLOCK OCCURS 4096 TIMES.
                03 LK-BLOCK-ID          BINARY-LONG UNSIGNED.
            02 LK-BIOME OCCURS 64 TIMES.
                04 LK-BIOME-ID      BINARY-LONG UNSIGNED.
        01 LK-BUFFER            PIC X ANY LENGTH.
        01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-CHUNK-SEC LK-SECTION LK-BUFFER LK-BUFFERPOS.
        *> block count (16x16x16)
        MOVE 4096 TO INT16
        CALL "Encode-Short" USING INT16 LK-BUFFER LK-BUFFERPOS

        *> TODO: implement an actual palette to save bandwidth, and improve performance

        IF LK-NON-AIR = 0
            *> shortcut if the section is empty
            *> - bits per entry: 0 = single-valued
            MOVE X"00" TO LK-BUFFER(LK-BUFFERPOS:1)
            ADD 1 TO LK-BUFFERPOS
            *> - palette: 0 = id of the air block
            MOVE X"00" TO LK-BUFFER(LK-BUFFERPOS:1)
            ADD 1 TO LK-BUFFERPOS
            *> - data array length: 0, since we have a single-valued palette
            MOVE X"00" TO LK-BUFFER(LK-BUFFERPOS:1)
            ADD 1 TO LK-BUFFERPOS
        ELSE
            *> block states
            *> - bits per entry: 15 = direct palette
            MOVE X"0F" TO LK-BUFFER(LK-BUFFERPOS:1)
            ADD 1 TO LK-BUFFERPOS
            *> - palette: empty, since we have a direct palette
            *> - data array length: ceil((4096 entries) / floor(64 / (15 bits))) = 1024
            MOVE 1024 TO INT32
            CALL "Encode-VarInt" USING INT32 LK-BUFFER LK-BUFFERPOS
            *> - data array: block ids - x increases fastest, then z, then y
            PERFORM VARYING BLOCK-INDEX FROM 0 BY 4 UNTIL BLOCK-INDEX >= 4096
                *> Each block uses 15 bits, and 4 blocks are packed into a 64-bit long starting from the least
                *> significant bit. Since only 60 bits are used, the 4 most significant bits become padding (0).
                COMPUTE PALETTE-ENTRY = LK-BLOCK-ID(BLOCK-INDEX + 1) + 32768 * (LK-BLOCK-ID(BLOCK-INDEX + 2) + 32768 * (LK-BLOCK-ID(BLOCK-INDEX + 3) + 32768 * LK-BLOCK-ID(BLOCK-INDEX + 4)))
                *> The following code is an inlined version of Encode-UnsignedLong (improves performance by 20%)
                MOVE FUNCTION REVERSE(PALETTE-ENTRY-BYTES) TO LK-BUFFER(LK-BUFFERPOS:PALETTE-ENTRY-LENGTH)
                ADD PALETTE-ENTRY-LENGTH TO LK-BUFFERPOS
            END-PERFORM
        END-IF

        *> biomes
        *> - bits per entry: 7 = direct palette
        MOVE X"07" TO LK-BUFFER(LK-BUFFERPOS:1)
        ADD 1 TO LK-BUFFERPOS
        *> - palette: empty, since we have a direct palette
        *> - data array length: ceil(64 entries / floor(64 / (7 bits))) = 8
        MOVE 8 TO INT32
        CALL "Encode-VarInt" USING INT32 LK-BUFFER LK-BUFFERPOS
        *> - data array: biome ids - x increases fastest, then z, then y
        *> TODO make this code generic - 64 is not divisible by 9, so we need to handle the last entry separately
        PERFORM VARYING BIOME-INDEX FROM 0 BY 9 UNTIL BIOME-INDEX >= 63
            COMPUTE PALETTE-ENTRY = LK-BIOME-ID(BIOME-INDEX + 1) + 128 * (LK-BIOME-ID(BIOME-INDEX + 2) + 128 * (LK-BIOME-ID(BIOME-INDEX + 3)
                + 128 * (LK-BIOME-ID(BIOME-INDEX + 4) + 128 * (LK-BIOME-ID(BIOME-INDEX + 5) + 128 * (LK-BIOME-ID(BIOME-INDEX + 6)
                + 128 * (LK-BIOME-ID(BIOME-INDEX + 7) + 128 * (LK-BIOME-ID(BIOME-INDEX + 8) + 128 * LK-BIOME-ID(BIOME-INDEX + 9))))))))
            MOVE FUNCTION REVERSE(PALETTE-ENTRY-BYTES) TO LK-BUFFER(LK-BUFFERPOS:PALETTE-ENTRY-LENGTH)
            ADD PALETTE-ENTRY-LENGTH TO LK-BUFFERPOS
        END-PERFORM
        COMPUTE PALETTE-ENTRY = LK-BIOME-ID(BIOME-INDEX + 1)
        MOVE FUNCTION REVERSE(PALETTE-ENTRY-BYTES) TO LK-BUFFER(LK-BUFFERPOS:PALETTE-ENTRY-LENGTH)
        ADD PALETTE-ENTRY-LENGTH TO LK-BUFFERPOS

        GOBACK.

    END PROGRAM EncodeChunkSection.

END PROGRAM SendPacket-ChunkData.
