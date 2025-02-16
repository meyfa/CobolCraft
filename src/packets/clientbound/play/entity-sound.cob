IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-EntitySound.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:sound_entity".
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(1024).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
    *> temporary variables
    01 TEMP-INT32               BINARY-LONG.
    01 TEMP-FLOAT               FLOAT-SHORT.
    01 SEED                     BINARY-LONG-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-ENTITY-ID             BINARY-LONG UNSIGNED.
    01 LK-SOUND-ID              BINARY-LONG UNSIGNED.
    01 LK-VOLUME                FLOAT-SHORT.

PROCEDURE DIVISION USING LK-CLIENT LK-ENTITY-ID LK-SOUND-ID LK-VOLUME.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    COMPUTE TEMP-INT32 = LK-SOUND-ID + 1
    CALL "Encode-VarInt" USING TEMP-INT32 PAYLOAD PAYLOADPOS

    *> category: https://gist.github.com/konwboj/7c0c380d3923443e9d55
    *> 7 = player
    *> TODO Can't we get this somewhere else?!
    MOVE 7 TO TEMP-INT32
    CALL "Encode-VarInt" USING TEMP-INT32 PAYLOAD PAYLOADPOS

    CALL "Encode-VarInt" USING LK-ENTITY-ID PAYLOAD PAYLOADPOS

    CALL "Encode-Float" USING LK-VOLUME PAYLOAD PAYLOADPOS

    *> TODO pitch
    MOVE 1.0 TO TEMP-FLOAT
    CALL "Encode-Float" USING TEMP-FLOAT PAYLOAD PAYLOADPOS

    *> TODO How can we generate a proper seed?
    CALL "Encode-Long" USING SEED PAYLOAD PAYLOADPOS

    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-EntitySound.
