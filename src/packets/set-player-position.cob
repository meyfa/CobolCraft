IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-SetPlayerPosition.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'40'.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(64000).
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 BUFFER           PIC X(8).
    01 BUFFERLEN        BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-POSITION.
        02 LK-X         FLOAT-LONG.
        02 LK-Y         FLOAT-LONG.
        02 LK-Z         FLOAT-LONG.
    01 LK-ROTATION.
        02 LK-YAW       FLOAT-SHORT.
        02 LK-PITCH     FLOAT-SHORT.
    01 LK-TELEPORT-ID   BINARY-LONG.

PROCEDURE DIVISION USING LK-CLIENT LK-POSITION LK-ROTATION LK-TELEPORT-ID.
    MOVE 0 TO PAYLOADLEN

    *> X
    CALL "Encode-Double" USING LK-X BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> Y
    CALL "Encode-Double" USING LK-Y BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> Z
    CALL "Encode-Double" USING LK-Z BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> Yaw
    CALL "Encode-Float" USING LK-YAW BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> Pitch
    CALL "Encode-Float" USING LK-PITCH BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> flags (0 = all values are absolute)
    MOVE X"00" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> teleport ID
    CALL "Encode-VarInt" USING LK-TELEPORT-ID BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> send packet
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-SetPlayerPosition.
