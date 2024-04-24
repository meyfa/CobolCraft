IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-PlayerChat.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'39'.
    *> temporary data used during encoding
    01 UINT16           BINARY-SHORT UNSIGNED.
    01 INT32            BINARY-LONG.
    01 INT64            BINARY-LONG-LONG.
    01 BUFFER           PIC X(16).
    01 BUFFERLEN        BINARY-LONG UNSIGNED.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(64000).
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-HNDL          PIC X(4).
    01 LK-ERRNO         PIC 9(3).
    01 LK-SENDER-UUID   PIC X(16).
    01 LK-SENDER-NAME   PIC X ANY LENGTH.
    01 LK-MESSAGE       PIC X ANY LENGTH.
    01 LK-MESSAGE-LEN   BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO LK-SENDER-UUID LK-SENDER-NAME LK-MESSAGE LK-MESSAGE-LEN.
    MOVE 0 TO PAYLOADLEN

    *> --- header ---

    *> sender UUID
    MOVE LK-SENDER-UUID(1:16) TO PAYLOAD(PAYLOADLEN + 1:16)
    ADD 16 TO PAYLOADLEN

    *> index
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> message signature present
    MOVE X"00" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> --- body ---

    *> message
    CALL "Encode-VarInt" USING LK-MESSAGE-LEN BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN
    MOVE LK-MESSAGE(1:LK-MESSAGE-LEN) TO PAYLOAD(PAYLOADLEN + 1:LK-MESSAGE-LEN)
    ADD LK-MESSAGE-LEN TO PAYLOADLEN

    *> signature timestamp and salt
    MOVE X"00000000000000000000000000000000" TO PAYLOAD(PAYLOADLEN + 1:16)
    ADD 16 TO PAYLOADLEN

    *> --- previous messages ---

    *> total previous messages
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> --- other ---

    *> unsigned content present
    MOVE X"00" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> filter type enum (0: not filtered)
    MOVE X"00" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> --- chat formatting ---

    *> chat type
    *> TODO: This is a reference to the "minecraft:chat_type" registry. Get the correct value from the registry.
    MOVE X"00" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> sender name (NBT string tag)
    MOVE X"08" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN
    COMPUTE UINT16 = FUNCTION STORED-CHAR-LENGTH(LK-SENDER-NAME)
    CALL "Encode-Short" USING UINT16 BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN
    *> TODO: implement modified UTF-8: https://docs.oracle.com/javase/8/docs/api/java/io/DataInput.html#modified-utf-8
    MOVE LK-SENDER-NAME(1:UINT16) TO PAYLOAD(PAYLOADLEN + 1:UINT16)
    ADD UINT16 TO PAYLOADLEN

    *> has target name
    MOVE X"00" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> Send the packet
    CALL "SendPacket" USING LK-HNDL PACKET-ID PAYLOAD PAYLOADLEN LK-ERRNO

    GOBACK.

END PROGRAM SendPacket-PlayerChat.
