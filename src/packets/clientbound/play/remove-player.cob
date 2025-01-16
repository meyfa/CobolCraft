IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-RemovePlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:player_info_remove".
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(1024).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 INT32            BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-UUID          PIC X(16).

PROCEDURE DIVISION USING LK-CLIENT LK-UUID.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> number of players
    *> TODO: support sending multiple players
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> player UUID
    MOVE LK-UUID(1:16) TO PAYLOAD(PAYLOADPOS:16)
    ADD 16 TO PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-RemovePlayer.
