IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-SpawnEntity.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'01'.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(1024).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> registry data (cached on first use)
    01 PLAYER-TYPE      BINARY-LONG             VALUE -1.
    *> constants
    01 C-ENTITY_TYPE    PIC X(64)               VALUE "minecraft:entity_type".
    01 C-PLAYER         PIC X(64)               VALUE "minecraft:player".
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-ENTITY-ID     BINARY-LONG.
    01 LK-ENTITY-UUID   PIC X(16).
    *> TODO: support entity types other than player
    *> 01 LK-TYPE          BINARY-LONG.
    01 LK-POSITION.
        02 LK-X         FLOAT-LONG.
        02 LK-Y         FLOAT-LONG.
        02 LK-Z         FLOAT-LONG.
    01 LK-ROTATION.
        02 LK-YAW       FLOAT-SHORT.
        02 LK-PITCH     FLOAT-SHORT.

PROCEDURE DIVISION USING LK-CLIENT LK-ENTITY-ID LK-ENTITY-UUID LK-POSITION LK-ROTATION.
    *> obtain and cache ID of player entity type
    IF PLAYER-TYPE < 0
        CALL "Registries-Get-EntryId" USING C-ENTITY_TYPE C-PLAYER PLAYER-TYPE
    END-IF

    MOVE 1 TO PAYLOADPOS

    *> entity ID
    CALL "Encode-VarInt" USING LK-ENTITY-ID PAYLOAD PAYLOADPOS

    *> entity UUID
    MOVE LK-ENTITY-UUID(1:16) TO PAYLOAD(PAYLOADPOS:16)
    ADD 16 TO PAYLOADPOS

    *> entity type
    CALL "Encode-VarInt" USING PLAYER-TYPE PAYLOAD PAYLOADPOS

    *> entity position
    CALL "Encode-Double" USING LK-X PAYLOAD PAYLOADPOS
    CALL "Encode-Double" USING LK-Y PAYLOAD PAYLOADPOS
    CALL "Encode-Double" USING LK-Z PAYLOAD PAYLOADPOS

    *> entity rotation
    CALL "Encode-Angle" USING LK-YAW PAYLOAD PAYLOADPOS
    CALL "Encode-Angle" USING LK-PITCH PAYLOAD PAYLOADPOS

    *> head yaw
    *> TODO: implement as a separate field
    CALL "Encode-Angle" USING LK-YAW PAYLOAD PAYLOADPOS

    *> data=0
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> velocity X/Y/Z
    MOVE X"000000000000" TO PAYLOAD(PAYLOADPOS:6)
    ADD 6 TO PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-SpawnEntity.
