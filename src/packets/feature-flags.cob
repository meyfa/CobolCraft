IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-FeatureFlags.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'0C'.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(1024).
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary
    01 INT32            BINARY-LONG.
    01 BUFFER           PIC X(8).
    01 BUFFERLEN        BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT.
    MOVE 0 TO PAYLOADLEN

    *> count = 1
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> feature flag: "minecraft:vanilla"
    MOVE FUNCTION CHAR(17 + 1) TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN
    MOVE "minecraft:vanilla" TO PAYLOAD(PAYLOADLEN + 1:17)
    ADD 17 TO PAYLOADLEN

    *> send packet
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-FeatureFlags.
