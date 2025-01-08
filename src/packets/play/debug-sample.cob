IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-DebugSample.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:debug_sample".
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(1024).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 INT32            BINARY-LONG.
    01 SAMPLE-INDEX     BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-SAMPLE.
        02 LK-SAMPLE-FULL   BINARY-LONG-LONG.
        02 LK-SAMPLE-MAIN   BINARY-LONG-LONG.
        02 LK-SAMPLE-TASKS  BINARY-LONG-LONG.
        02 LK-SAMPLE-IDLE   BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-CLIENT LK-SAMPLE.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> count = 4 (components of the sample)
    MOVE 4 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    CALL "Encode-Long" USING LK-SAMPLE-FULL PAYLOAD PAYLOADPOS
    CALL "Encode-Long" USING LK-SAMPLE-MAIN PAYLOAD PAYLOADPOS
    CALL "Encode-Long" USING LK-SAMPLE-TASKS PAYLOAD PAYLOADPOS
    CALL "Encode-Long" USING LK-SAMPLE-IDLE PAYLOAD PAYLOADPOS

    *> type = tick
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-DebugSample.
