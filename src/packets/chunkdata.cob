*> --- SendPacket-ChunkData ---
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-ChunkData.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'27'.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(512000).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 INT32            BINARY-LONG.
    01 CHUNK-SEC        BINARY-LONG UNSIGNED.
    01 CHUNK-DATA       PIC X(256000).
    01 CHUNK-DATA-POS   BINARY-LONG UNSIGNED.
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
        CALL STATIC "EncodeChunkSection" USING CHUNK-SEC LK-CHUNK-SECTION(CHUNK-SEC + 1) CHUNK-DATA CHUNK-DATA-POS
    END-PERFORM

    *> data prefixed by VarInt size
    COMPUTE INT32 = CHUNK-DATA-POS - 1
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
    MOVE CHUNK-DATA(1:INT32) TO PAYLOAD(PAYLOADPOS:INT32)
    ADD INT32 TO PAYLOADPOS

    *> number of block entities
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> block entities

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
        01 PALETTE-ENTRY-LENGTH BINARY-LONG UNSIGNED        VALUE 8.
        01 PALETTE-ENTRY        BINARY-LONG-LONG UNSIGNED.
        01 PALETTE-ENTRY-BYTES  REDEFINES PALETTE-ENTRY PIC X(8).
    LINKAGE SECTION.
        01 LK-CHUNK-SEC         BINARY-LONG UNSIGNED.
        01 LK-SECTION.
            02 LK-NON-AIR           BINARY-LONG UNSIGNED.
            02 LK-BLOCK OCCURS 4096 TIMES.
                03 LK-BLOCK-ID          BINARY-LONG UNSIGNED.
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
            *> - data array length: (4096 / 4 = 1024)
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
        *> - bits per entry: 0 = single-valued
        MOVE X"00" TO LK-BUFFER(LK-BUFFERPOS:1)
        ADD 1 TO LK-BUFFERPOS
        *> - palette: id of the biome
        *> TODO: use plains biome
        MOVE 0 TO INT32
        CALL "Encode-VarInt" USING INT32 LK-BUFFER LK-BUFFERPOS
        *> - data array length: 0, since we have a single-valued palette
        MOVE 0 TO INT32
        CALL "Encode-VarInt" USING INT32 LK-BUFFER LK-BUFFERPOS
        *> - data array: empty

        GOBACK.

    END PROGRAM EncodeChunkSection.

END PROGRAM SendPacket-ChunkData.
