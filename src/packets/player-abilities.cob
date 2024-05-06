IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-PlayerAbilities.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'38'.
    01 FLYING-SPEED     FLOAT-SHORT             VALUE 0.05.
    01 FIELD-OF-VIEW    FLOAT-SHORT             VALUE 0.1.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(16).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-FLYING        BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-FLYING.
    MOVE 1 TO PAYLOADPOS

    *> flags bitmask: 0x01 invulnerable, 0x02 flying, 0x04 allow flying, 0x08 creative mode (instant break)
    IF LK-FLYING > 0
        MOVE X"0F" TO PAYLOAD(PAYLOADPOS:1)
    ELSE
        MOVE X"0D" TO PAYLOAD(PAYLOADPOS:1)
    END-IF
    ADD 1 TO PAYLOADPOS

    *> flying speed
    CALL "Encode-Float" USING FLYING-SPEED PAYLOAD PAYLOADPOS

    *> field of view modifier
    CALL "Encode-Float" USING FIELD-OF-VIEW PAYLOAD PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-PlayerAbilities.
