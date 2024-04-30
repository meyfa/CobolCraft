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
    *> buffer used to store the JSON string
    01 JSONBUFFER               PIC X(64000).
    01 JSONPOS                  BINARY-LONG UNSIGNED.
    *> temporary data used during encoding
    01 INT32                    BINARY-LONG.
    01 STR                      PIC X(1000).
    01 STRLEN                   BINARY-LONG UNSIGNED.
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(64000).
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-HNDL                  PIC X(4).
    01 LK-ERRNO                 PIC 9(3).
    01 LK-STATE                 BINARY-CHAR.
    01 LK-REASON                PIC X ANY LENGTH.
    01 LK-REASONLEN             BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO LK-STATE LK-REASON LK-REASONLEN.
    EVALUATE LK-STATE
        WHEN CLIENT-STATE-LOGIN
            MOVE PACKET-ID-LOGIN TO PACKET-ID
        WHEN CLIENT-STATE-CONFIGURATION
            MOVE PACKET-ID-CONFIGURATION TO PACKET-ID
        WHEN CLIENT-STATE-PLAY
            MOVE PACKET-ID-PLAY TO PACKET-ID
        WHEN OTHER
            DISPLAY "Invalid state for Disconnect packet: " LK-STATE
            GOBACK
    END-EVALUATE

    *> Encode the JSON payload {"text":"<reason>"}
    MOVE 1 TO JSONPOS
    CALL "JsonEncode-ObjectStart" USING JSONBUFFER JSONPOS
    MOVE "text" TO STR
    MOVE 4 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-String" USING JSONBUFFER JSONPOS LK-REASON LK-REASONLEN
    CALL "JsonEncode-ObjectEnd" USING JSONBUFFER JSONPOS

    *> Build the payload: VarInt (JSON length) + JSON
    COMPUTE INT32 = JSONPOS - 1
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADLEN
    MOVE JSONBUFFER TO PAYLOAD(PAYLOADLEN + 1:JSONPOS)
    ADD INT32 TO PAYLOADLEN

    *> Send the packet
    CALL "SendPacket" USING LK-HNDL PACKET-ID PAYLOAD PAYLOADLEN LK-ERRNO.

END PROGRAM SendPacket-Disconnect.
