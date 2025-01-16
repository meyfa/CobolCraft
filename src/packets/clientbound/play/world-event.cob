IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-WorldEvent.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:level_event".
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(32).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-EVENT         BINARY-LONG.
    01 LK-POSITION.
        02 LK-X             BINARY-LONG.
        02 LK-Y             BINARY-LONG.
        02 LK-Z             BINARY-LONG.
    01 LK-DATA          BINARY-LONG.

PROCEDURE DIVISION USING LK-CLIENT LK-EVENT LK-POSITION LK-DATA.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> event type
    CALL "Encode-Int" USING LK-EVENT PAYLOAD PAYLOADPOS

    *> position
    CALL "Encode-Position" USING LK-POSITION PAYLOAD PAYLOADPOS

    *> data
    CALL "Encode-Int" USING LK-DATA PAYLOAD PAYLOADPOS

    *> disable relative volume = false
    *> TODO set to true for some sounds (https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Protocol#World_Event)
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-WorldEvent.
