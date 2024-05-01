IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-RemovePlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'3D'.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(1024).
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 INT32            BINARY-LONG.
    01 BUFFER           PIC X(16).
    01 BUFFERLEN        BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-UUID          PIC X(16).

PROCEDURE DIVISION USING LK-CLIENT LK-UUID.
    MOVE 0 TO PAYLOADLEN

    *> number of players
    *> TODO: support sending multiple players
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> player UUID
    MOVE LK-UUID(1:16) TO PAYLOAD(PAYLOADLEN + 1:16)
    ADD 16 TO PAYLOADLEN

    *> send packet
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-RemovePlayer.
