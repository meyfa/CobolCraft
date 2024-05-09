IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-LoginPlay.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID                BINARY-LONG             VALUE H'2B'.
    *> constants
    01 C-MINECRAFT-OVERWORLD    PIC X(19) VALUE "minecraft:overworld".
    *> temporary data used during encoding
    01 INT32                    BINARY-LONG.
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(64000).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-ENTITY-ID             BINARY-LONG.
    01 LK-VIEW-DISTANCE         BINARY-LONG.
    01 LK-GAMEMODE              BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-ENTITY-ID LK-VIEW-DISTANCE LK-GAMEMODE.
    MOVE 1 TO PAYLOADPOS

    *> entity ID
    CALL "Encode-Int" USING LK-ENTITY-ID PAYLOAD PAYLOADPOS

    *> is hardcore=false
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> dimension count=1
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> dimension name array=["minecraft:overworld"]
    MOVE LENGTH OF C-MINECRAFT-OVERWORLD TO INT32
    CALL "Encode-String" USING C-MINECRAFT-OVERWORLD INT32 PAYLOAD PAYLOADPOS

    *> max players=1
    MOVE 10 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> view distance
    CALL "Encode-VarInt" USING LK-VIEW-DISTANCE PAYLOAD PAYLOADPOS

    *> simulation distance = view distance
    CALL "Encode-VarInt" USING LK-VIEW-DISTANCE PAYLOAD PAYLOADPOS

    *> reduced debug info=false
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> enable respawn screen=true
    MOVE X"01" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> do limited crafting=false
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> dimension type (ID in the minecraft:dimension_type registry)
    *> TODO: get this from the registry
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> dimension name="minecraft:overworld"
    MOVE LENGTH OF C-MINECRAFT-OVERWORLD TO INT32
    CALL "Encode-String" USING C-MINECRAFT-OVERWORLD INT32 PAYLOAD PAYLOADPOS

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

    *> enforces secure chat=false
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> Send the packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-LoginPlay.
