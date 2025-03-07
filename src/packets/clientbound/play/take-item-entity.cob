IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-TakeItemEntity.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:take_item_entity".
    01 PAYLOAD                  PIC X(16).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-ITEM-ENTITY-ID        BINARY-LONG.
    01 LK-COLLECTOR-ENTITY-ID   BINARY-LONG.
    01 LK-PICKUP-COUNT          BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-ITEM-ENTITY-ID LK-COLLECTOR-ENTITY-ID LK-PICKUP-COUNT.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    CALL "Encode-VarInt" USING LK-ITEM-ENTITY-ID PAYLOAD PAYLOADPOS
    CALL "Encode-VarInt" USING LK-COLLECTOR-ENTITY-ID PAYLOAD PAYLOADPOS
    CALL "Encode-VarInt" USING LK-PICKUP-COUNT PAYLOAD PAYLOADPOS

    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-TakeItemEntity.
