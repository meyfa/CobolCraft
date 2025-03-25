IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-Respawn.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:respawn".
    *> constants
    01 MINECRAFT-OVERWORLD          PIC X(19)                   VALUE "minecraft:overworld".
    *> temporary data
    01 INT32                        BINARY-LONG.
    *> buffer used to store the packet data
    01 PAYLOAD                      PIC X(64000).
    01 PAYLOADPOS                   BINARY-LONG UNSIGNED.
    01 PAYLOADLEN                   BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                    BINARY-LONG UNSIGNED.
    01 LK-GAMEMODE                  BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-GAMEMODE.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> TODO deduplicate with login-play.cob

    *> dimension type (ID in the minecraft:dimension_type registry)
    CALL "Registries-Lookup" USING "minecraft:dimension_type" MINECRAFT-OVERWORLD INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> dimension name="minecraft:overworld"
    MOVE LENGTH OF MINECRAFT-OVERWORLD TO INT32
    CALL "Encode-String" USING MINECRAFT-OVERWORLD INT32 PAYLOAD PAYLOADPOS

    *> hashed seed=0 (8-byte long)
    MOVE X"0000000000000000" TO PAYLOAD(PAYLOADPOS:8)
    ADD 8 TO PAYLOADPOS

    *> gamemode
    CALL "Encode-Byte" USING LK-GAMEMODE PAYLOAD PAYLOADPOS

    *> previous gamemode=-1
    MOVE X"FF" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> is debug=false
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> is flat=false
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> has death location=false
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> portal cooldown=0
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> sea level=63
    MOVE X"3F" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> bitmask: 0x01 = keep attributes, 0x02 = keep metadata
    *> for deaths, keep neither
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> Send the packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-Respawn.
