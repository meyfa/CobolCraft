*> --- SendPacket-ChunkData ---
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-ChunkData.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'25'.
    *> constants
    01 ALL-0-BITS       PIC X(8)                VALUE X"0000000000000000".
    01 ALL-1-BITS       PIC X(8)                VALUE X"ffffffffffffffff".
    01 EMPTY-COMP       PIC X(2)                VALUE X"0a00".
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(512000).
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 BUFFER           PIC X(256000).
    01 BUFFERLEN        BINARY-LONG UNSIGNED.
    01 INT32            BINARY-LONG.
    01 INT64            BINARY-LONG-LONG.
    *> chunk data
    01 CHUNK-SEC        BINARY-LONG UNSIGNED.
    01 CHUNK-DATA       PIC X(256000).
    01 DATA-LEN         BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-HNDL          PIC X(4).
    01 LK-ERRNO         PIC 9(3).
    01 LK-CHUNK.
        02 LK-CHUNK-X BINARY-LONG.
        02 LK-CHUNK-Z BINARY-LONG.
        *> block IDs (16x384x16) - X increases fastest, then Z, then Y
        02 LK-CHUNK-BLOCKS.
            03 LK-BLOCK OCCURS 98304 TIMES.
                04 LK-BLOCK-ID BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO LK-CHUNK.
    MOVE 0 TO PAYLOADLEN

    *> chunk x
    CALL "Encode-Int" USING LK-CHUNK-X BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> chunk z
    CALL "Encode-Int" USING LK-CHUNK-Z BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> heightmap NBT - send an empty compound tag for now
    MOVE EMPTY-COMP TO PAYLOAD(PAYLOADLEN + 1:2)
    ADD 2 TO PAYLOADLEN

    *> construct chunk data
    MOVE 0 TO DATA-LEN
    PERFORM VARYING CHUNK-SEC FROM 0 BY 1 UNTIL CHUNK-SEC >= 24
        CALL "EncodeChunkSection" USING CHUNK-SEC LK-CHUNK-BLOCKS BUFFER BUFFERLEN
        MOVE BUFFER(1:BUFFERLEN) TO CHUNK-DATA(DATA-LEN + 1:BUFFERLEN)
        ADD BUFFERLEN TO DATA-LEN
    END-PERFORM

    *> size of data
    CALL "Encode-VarInt" USING DATA-LEN BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> data
    MOVE CHUNK-DATA(1:DATA-LEN) TO PAYLOAD(PAYLOADLEN + 1:DATA-LEN)
    ADD DATA-LEN TO PAYLOADLEN

    *> number of block entities
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> block entities

    *> sky light mask
    *> Note: Each 16x16x16 section needs a bit + 1 below the chunk + 1 above the chunk => 26 bits => 1 long.
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN
    MOVE ALL-1-BITS TO PAYLOAD(PAYLOADLEN + 1:8)
    ADD 8 TO PAYLOADLEN

    *> block light mask
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN
    MOVE ALL-1-BITS TO PAYLOAD(PAYLOADLEN + 1:8)
    ADD 8 TO PAYLOADLEN

    *> empty sky light mask
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN
    MOVE ALL-0-BITS TO PAYLOAD(PAYLOADLEN + 1:8)
    ADD 8 TO PAYLOADLEN

    *> empty block light mask
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN
    MOVE ALL-1-BITS TO PAYLOAD(PAYLOADLEN + 1:8)
    ADD 8 TO PAYLOADLEN

    *> sky light array count
    MOVE 26 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> sky light arrays
    MOVE 2048 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    PERFORM 26 TIMES
        *> array length
        MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
        ADD BUFFERLEN TO PAYLOADLEN
        *> array data (light values - half a byte per block)
        PERFORM 256 TIMES
            MOVE ALL-1-BITS TO PAYLOAD(PAYLOADLEN + 1:8)
            ADD 8 TO PAYLOADLEN
        END-PERFORM
    END-PERFORM

    *> block light array count
    MOVE 26 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> block light arrays
    MOVE 2048 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    PERFORM 26 TIMES
        *> array length
        MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
        ADD BUFFERLEN TO PAYLOADLEN
        *> array data (light values - half a byte per block)
        PERFORM 256 TIMES
            MOVE ALL-1-BITS TO PAYLOAD(PAYLOADLEN + 1:8)
            ADD 8 TO PAYLOADLEN
        END-PERFORM
    END-PERFORM

    *> send packet
    CALL "SendPacket" USING LK-HNDL PACKET-ID PAYLOAD PAYLOADLEN LK-ERRNO
    GOBACK.

    *> --- EncodeChunkSection ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. EncodeChunkSection.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 INT16            BINARY-SHORT.
        01 INT32            BINARY-LONG.
        01 BUFFER           PIC X(16000).
        01 BUFFERLEN        BINARY-LONG UNSIGNED.
        01 BLOCK-INDEX      BINARY-LONG UNSIGNED.
        01 PALETTE-ENTRY    BINARY-LONG-LONG UNSIGNED.
        01 PALETTE-BUFF     PIC X(8).
        01 PALETTE-BUFFLEN  BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        01 LK-CHUNK-SEC     BINARY-LONG UNSIGNED.
        01 LK-CHUNK-BLOCKS.
            02 LK-BLOCK OCCURS 98304 TIMES.
                03 LK-BLOCK-ID BINARY-LONG UNSIGNED.
        01 LK-BUFFER        PIC X ANY LENGTH.
        01 LK-BUFFERLEN     BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-CHUNK-SEC LK-CHUNK-BLOCKS LK-BUFFER LK-BUFFERLEN.
        MOVE 0 TO LK-BUFFERLEN

        *> block count (16x16x16)
        MOVE 4096 TO INT16
        CALL "Encode-Short" USING INT16 BUFFER BUFFERLEN
        MOVE BUFFER(1:BUFFERLEN) TO LK-BUFFER(LK-BUFFERLEN + 1:BUFFERLEN)
        ADD BUFFERLEN TO LK-BUFFERLEN

        *> TODO: implement an actual palette to save bandwidth, and improve performance

        *> block states
        *> - bits per entry: 15 = direct palette
        MOVE X"0F" TO LK-BUFFER(LK-BUFFERLEN + 1:1)
        ADD 1 TO LK-BUFFERLEN
        *> - palette: empty, since we have a direct palette
        *> - data array length: (4096 / 4 = 1024)
        MOVE 1024 TO INT32
        CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
        MOVE BUFFER(1:BUFFERLEN) TO LK-BUFFER(LK-BUFFERLEN + 1:BUFFERLEN)
        ADD BUFFERLEN TO LK-BUFFERLEN
        *> - data array: block ids - x increases fastest, then z, then y
        COMPUTE BLOCK-INDEX = LK-CHUNK-SEC * 4096 + 1
        PERFORM 1024 TIMES
            *> Each block uses 15 bits, and 4 blocks are packed into a 64-bit long starting from the least
            *> significant bit. Since only 60 bits are used, the 4 most significant bits become padding (0).
            COMPUTE PALETTE-ENTRY = LK-BLOCK-ID(BLOCK-INDEX) + 32768 * (LK-BLOCK-ID(BLOCK-INDEX + 1) + 32768 * (LK-BLOCK-ID(BLOCK-INDEX + 2) + 32768 * LK-BLOCK-ID(BLOCK-INDEX + 3)))
            CALL "Encode-Long" USING PALETTE-ENTRY PALETTE-BUFF PALETTE-BUFFLEN
            MOVE PALETTE-BUFF(1:PALETTE-BUFFLEN) TO LK-BUFFER(LK-BUFFERLEN + 1:PALETTE-BUFFLEN)
            ADD PALETTE-BUFFLEN TO LK-BUFFERLEN
            ADD 4 TO BLOCK-INDEX
        END-PERFORM

        *> biomes
        *> - bits per entry: 0 = single-valued
        MOVE X"00" TO LK-BUFFER(LK-BUFFERLEN + 1:1)
        ADD 1 TO LK-BUFFERLEN
        *> - palette: id of the biome
        *> TODO: use plains biome
        MOVE 0 TO INT32
        CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
        MOVE BUFFER(1:BUFFERLEN) TO LK-BUFFER(LK-BUFFERLEN + 1:BUFFERLEN)
        ADD BUFFERLEN TO LK-BUFFERLEN
        *> - data array length: 0, since we have a single-valued palette
        MOVE 0 TO INT32
        CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
        MOVE BUFFER(1:BUFFERLEN) TO LK-BUFFER(LK-BUFFERLEN + 1:BUFFERLEN)
        ADD BUFFERLEN TO LK-BUFFERLEN
        *> - data array: empty

        GOBACK.

    END PROGRAM EncodeChunkSection.

END PROGRAM SendPacket-ChunkData.
