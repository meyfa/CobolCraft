IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-FeatureFlags.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "configuration/clientbound/minecraft:update_enabled_features".
    01 C-MINECRAFT-VANILLA      PIC X(17) VALUE "minecraft:vanilla".
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(64).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
    *> temporary data
    01 INT32                    BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> count = 1
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> feature flag: "minecraft:vanilla"
    MOVE LENGTH OF C-MINECRAFT-VANILLA TO INT32
    CALL "Encode-String" USING C-MINECRAFT-VANILLA INT32 PAYLOAD PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-FeatureFlags.
