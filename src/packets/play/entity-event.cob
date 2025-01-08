IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-EntityEvent.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:entity_event".
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(8).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-ENTITY-ID     BINARY-LONG.
    *> Possible values: https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Entity_statuses#Player
    01 LK-EVENT         BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-ENTITY-ID LK-EVENT.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> entity ID
    CALL "Encode-Int" USING LK-ENTITY-ID PAYLOAD PAYLOADPOS

    *> event (unsigned byte)
    MOVE FUNCTION CHAR(LK-EVENT + 1) TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-EntityEvent.
