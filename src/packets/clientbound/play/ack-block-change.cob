IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-AckBlockChange.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:block_changed_ack".
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(8).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-SEQUENCE-ID   BINARY-LONG.

PROCEDURE DIVISION USING LK-CLIENT LK-SEQUENCE-ID.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> sequence ID
    CALL "Encode-VarInt" USING LK-SEQUENCE-ID PAYLOAD PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-AckBlockChange.
