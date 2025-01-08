IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-SetHealth.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'62'.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(16).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-HEALTH        FLOAT-SHORT.
    01 LK-FOOD-LEVEL    BINARY-LONG.
    01 LK-SATURATION    FLOAT-SHORT.

PROCEDURE DIVISION USING LK-CLIENT LK-HEALTH LK-FOOD-LEVEL LK-SATURATION.
    MOVE 1 TO PAYLOADPOS

    CALL "Encode-Float" USING LK-HEALTH PAYLOAD PAYLOADPOS
    CALL "Encode-VarInt" USING LK-FOOD-LEVEL PAYLOAD PAYLOADPOS
    CALL "Encode-Float" USING LK-SATURATION PAYLOAD PAYLOADPOS

    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-SetHealth.