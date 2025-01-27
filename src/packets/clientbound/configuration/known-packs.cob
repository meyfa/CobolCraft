IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-KnownPacks.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "configuration/clientbound/minecraft:select_known_packs".
    *> known pack data
    COPY DD-VERSION.
    01 PACK-NAMESPACE           PIC X(9)                VALUE "minecraft".
    01 PACK-ID                  PIC X(4)                VALUE "core".
    01 PACK-VERSION             PIC X(6)                VALUE GAME-VERSION-STRING.
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(1024).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
    *> temporary
    01 INT32                    BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> count
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> pack namespace
    MOVE LENGTH OF PACK-NAMESPACE TO INT32
    CALL "Encode-String" USING PACK-NAMESPACE INT32 PAYLOAD PAYLOADPOS

    *> pack id
    MOVE LENGTH OF PACK-ID TO INT32
    CALL "Encode-String" USING PACK-ID INT32 PAYLOAD PAYLOADPOS

    *> pack version
    MOVE FUNCTION STORED-CHAR-LENGTH(PACK-VERSION) TO INT32
    CALL "Encode-String" USING PACK-VERSION INT32 PAYLOAD PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-KnownPacks.
