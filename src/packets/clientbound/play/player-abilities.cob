IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-PlayerAbilities.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:player_abilities".
    01 FLYING-SPEED     FLOAT-SHORT             VALUE 0.05.
    01 FIELD-OF-VIEW    FLOAT-SHORT             VALUE 0.1.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(16).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary variables
    01 TEMP-INT8        BINARY-CHAR.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-GAMEMODE      BINARY-CHAR UNSIGNED.
    01 LK-FLYING        BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-GAMEMODE LK-FLYING.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> flags bitmask: 0x01 invulnerable, 0x02 flying, 0x04 allow flying, 0x08 creative mode (instant break)
    MOVE 0 TO TEMP-INT8
    IF LK-GAMEMODE = 1 OR 3
        ADD H'05' TO TEMP-INT8
    END-IF
    IF LK-GAMEMODE = 1
        ADD H'08' TO TEMP-INT8
    END-IF
    IF LK-FLYING > 0
        ADD H'02' TO TEMP-INT8
    END-IF
    CALL "Encode-Byte" USING TEMP-INT8 PAYLOAD PAYLOADPOS

    *> flying speed
    CALL "Encode-Float" USING FLYING-SPEED PAYLOAD PAYLOADPOS

    *> field of view modifier
    CALL "Encode-Float" USING FIELD-OF-VIEW PAYLOAD PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-PlayerAbilities.
