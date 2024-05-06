IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-Status.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'00'.
    *> buffer used to store the JSON string
    01 JSONBUFFER       PIC X(64000).
    01 JSONPOS          BINARY-LONG UNSIGNED.
    *> temporary data used during encoding
    01 INT32            BINARY-LONG.
    01 STR              PIC X(64).
    01 STRLEN           BINARY-LONG UNSIGNED.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(64000).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-MOTD          PIC X ANY LENGTH.
    01 LK-PLAYER-LIMIT  BINARY-LONG UNSIGNED.
    01 LK-PLAYER-COUNT  BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-MOTD LK-PLAYER-LIMIT LK-PLAYER-COUNT.
    *> Encode the JSON payload
    MOVE 1 TO JSONPOS

    *> {
    CALL "JsonEncode-ObjectStart" USING JSONBUFFER JSONPOS

    *>   "version": {
    MOVE "version" TO STR
    MOVE 7 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-ObjectStart" USING JSONBUFFER JSONPOS
    *>     "name": "X.Y.Z",
    MOVE "name" TO STR
    MOVE 4 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    MOVE "1.20.6" TO STR
    MOVE 6 TO STRLEN
    CALL "JsonEncode-String" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-Comma" USING JSONBUFFER JSONPOS
    *>     "protocol": 766
    MOVE "protocol" TO STR
    MOVE 8 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    MOVE 766 TO INT32
    CALL "JsonEncode-Integer" USING JSONBUFFER JSONPOS INT32
    *>   },
    CALL "JsonEncode-ObjectEnd" USING JSONBUFFER JSONPOS
    CALL "JsonEncode-Comma" USING JSONBUFFER JSONPOS

    *>   "players": {
    MOVE "players" TO STR
    MOVE 7 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-ObjectStart" USING JSONBUFFER JSONPOS
    *>     "max": <LK-PLAYER-LIMIT>,
    MOVE "max" TO STR
    MOVE 3 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-Integer" USING JSONBUFFER JSONPOS LK-PLAYER-LIMIT
    CALL "JsonEncode-Comma" USING JSONBUFFER JSONPOS
    *>     "online": 0,
    MOVE "online" TO STR
    MOVE 6 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-Integer" USING JSONBUFFER JSONPOS LK-PLAYER-COUNT
    CALL "JsonEncode-Comma" USING JSONBUFFER JSONPOS
    *>     "sample": []
    MOVE "sample" TO STR
    MOVE 7 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-ArrayStart" USING JSONBUFFER JSONPOS
    CALL "JsonEncode-ArrayEnd" USING JSONBUFFER JSONPOS
    *>   },
    CALL "JsonEncode-ObjectEnd" USING JSONBUFFER JSONPOS
    CALL "JsonEncode-Comma" USING JSONBUFFER JSONPOS

    *>   "description":
    MOVE "description" TO STR
    MOVE 11 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    *> "<MOTD>"
    MOVE FUNCTION STORED-CHAR-LENGTH(LK-MOTD) TO STRLEN
    CALL "JsonEncode-String" USING JSONBUFFER JSONPOS LK-MOTD STRLEN
    *> }
    CALL "JsonEncode-ObjectEnd" USING JSONBUFFER JSONPOS

    *> Build the payload: VarInt (JSON length) + JSON
    COMPUTE INT32 = JSONPOS - 1
    MOVE 1 TO PAYLOADPOS
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS
    MOVE JSONBUFFER TO PAYLOAD(PAYLOADPOS:INT32)
    ADD INT32 TO PAYLOADPOS

    *> Send the packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-Status.
