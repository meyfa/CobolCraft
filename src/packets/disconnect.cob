IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-Disconnect.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENT-STATES.
    *> packet id depending on connection state
    78 PACKET-ID-LOGIN          VALUE H'00'.
    78 PACKET-ID-CONFIGURATION  VALUE H'01'.
    78 PACKET-ID-PLAY           VALUE H'1D'.
    01 PACKET-ID                BINARY-LONG.
    *> temporary data used during encoding
    01 JSONBUFFER               PIC X(64000).
    01 JSONPOS                  BINARY-LONG UNSIGNED.
    01 UINT16                   BINARY-SHORT UNSIGNED.
    01 INT32                    BINARY-LONG.
    01 STR                      PIC X(1000).
    01 STRLEN                   BINARY-LONG UNSIGNED.
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(64000).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-STATE                 BINARY-CHAR.
    01 LK-REASON                PIC X ANY LENGTH.
    01 LK-REASONLEN             BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-STATE LK-REASON LK-REASONLEN.
    MOVE 1 TO PAYLOADPOS

    *> Special case for login, which uses a JSON Text Component as payload, while all others use an NBT Text Component.
    IF LK-STATE = CLIENT-STATE-LOGIN
        MOVE PACKET-ID-LOGIN TO PACKET-ID

        *> Encode the JSON payload {"text":"<reason>"}
        MOVE 1 TO JSONPOS
        CALL "JsonEncode-ObjectStart" USING JSONBUFFER JSONPOS
        MOVE "text" TO STR
        MOVE 4 TO STRLEN
        CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
        CALL "JsonEncode-String" USING JSONBUFFER JSONPOS LK-REASON LK-REASONLEN
        CALL "JsonEncode-ObjectEnd" USING JSONBUFFER JSONPOS

        *> Payload: JSON string length, JSON string
        COMPUTE INT32 = JSONPOS - 1
        CALL "Encode-String" USING JSONBUFFER INT32 PAYLOAD PAYLOADPOS

        *> Send the packet
        COMPUTE PAYLOADLEN = PAYLOADPOS - 1
        CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
        GOBACK
    ELSE

    EVALUATE LK-STATE
        WHEN CLIENT-STATE-CONFIGURATION
            MOVE PACKET-ID-CONFIGURATION TO PACKET-ID
        WHEN CLIENT-STATE-PLAY
            MOVE PACKET-ID-PLAY TO PACKET-ID
        WHEN OTHER
            DISPLAY "Invalid state for Disconnect packet: " LK-STATE
            GOBACK
    END-EVALUATE

    *> NBT compound tag
    MOVE X"0A" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> "text" key
    MOVE X"08" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS
    MOVE 4 TO UINT16
    CALL "Encode-UnsignedShort" USING UINT16 PAYLOAD PAYLOADPOS
    MOVE "text" TO PAYLOAD(PAYLOADPOS:4)
    ADD 4 TO PAYLOADPOS

    *> text
    MOVE LK-REASONLEN TO UINT16
    CALL "Encode-UnsignedShort" USING UINT16 PAYLOAD PAYLOADPOS
    *> TODO: implement modified UTF-8: https://docs.oracle.com/javase/8/docs/api/java/io/DataInput.html#modified-utf-8
    MOVE LK-REASON(1:LK-REASONLEN) TO PAYLOAD(PAYLOADPOS:LK-REASONLEN)
    ADD LK-REASONLEN TO PAYLOADPOS

    *> NBT end tag
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> Send the packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-Disconnect.
