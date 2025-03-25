IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-PlayerChat.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:player_chat".
    *> temporary data used during encoding
    01 UINT16                       BINARY-LONG-LONG UNSIGNED.
    01 INT32                        BINARY-LONG.
    *> buffer used to store the packet data
    01 PAYLOAD                      PIC X(64000).
    01 PAYLOADPOS                   BINARY-LONG UNSIGNED.
    01 PAYLOADLEN                   BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                    BINARY-LONG UNSIGNED.
    01 LK-SENDER-UUID               PIC X(16).
    01 LK-SENDER-NAME               PIC X ANY LENGTH.
    01 LK-MESSAGE                   PIC X ANY LENGTH.
    01 LK-MESSAGE-LEN               BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-SENDER-UUID LK-SENDER-NAME LK-MESSAGE LK-MESSAGE-LEN.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> --- header ---

    *> sender UUID
    MOVE LK-SENDER-UUID(1:16) TO PAYLOAD(PAYLOADPOS:16)
    ADD 16 TO PAYLOADPOS

    *> index
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> message signature present
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> --- body ---

    *> message
    CALL "Encode-String" USING LK-MESSAGE LK-MESSAGE-LEN PAYLOAD PAYLOADPOS

    *> signature timestamp and salt
    MOVE X"00000000000000000000000000000000" TO PAYLOAD(PAYLOADPOS:16)
    ADD 16 TO PAYLOADPOS

    *> --- previous messages ---

    *> total previous messages
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> --- other ---

    *> unsigned content present
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> filter type enum (0: not filtered)
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> --- chat formatting ---

    *> chat type
    CALL "Registries-Lookup" USING "minecraft:chat_type" "minecraft:chat" INT32
    ADD 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> sender name (NBT string tag)
    MOVE X"08" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS
    COMPUTE UINT16 = FUNCTION STORED-CHAR-LENGTH(LK-SENDER-NAME)
    CALL "Encode-Short" USING UINT16 PAYLOAD PAYLOADPOS
    *> TODO: implement modified UTF-8: https://docs.oracle.com/javase/8/docs/api/java/io/DataInput.html#modified-utf-8
    MOVE LK-SENDER-NAME(1:UINT16) TO PAYLOAD(PAYLOADPOS:UINT16)
    ADD UINT16 TO PAYLOADPOS

    *> has target name
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> Send the packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN

    GOBACK.

END PROGRAM SendPacket-PlayerChat.
