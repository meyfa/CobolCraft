IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-Status.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID    PIC 9(10)       VALUE 0.
    *> buffer used to store the JSON string
    01 JSONBUFFER   PIC X(64000).
    01 JSONPOS      PIC 9(5).
    *> temporary data used during encoding
    01 SINT32       PIC S9(10).
    01 STR          PIC X(64).
    01 STRLEN       PIC 9(5).
    *> buffer used to store the packet data
    01 PAYLOAD      PIC X(64000).
    01 PAYLOADLEN   PIC 9(5).
LINKAGE SECTION.
    01 LK-HNDL      PIC X(4).
    01 LK-ERRNO     PIC 9(3).
    01 LK-MOTD      PIC X(64).

PROCEDURE DIVISION USING BY REFERENCE LK-HNDL LK-ERRNO LK-MOTD.
    *> Encode the JSON string into the buffer.
    MOVE 1 TO JSONPOS

    *> {
    CALL "JsonEncode-ObjectStart" USING JSONBUFFER JSONPOS

    *>   "version": {
    MOVE "version" TO STR
    MOVE 7 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-ObjectStart" USING JSONBUFFER JSONPOS
    *>     "name": "1.20.4",
    MOVE "name" TO STR
    MOVE 4 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    MOVE "1.20.4" TO STR
    MOVE 6 TO STRLEN
    CALL "JsonEncode-String" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-Comma" USING JSONBUFFER JSONPOS
    *>     "protocol": 765
    MOVE "protocol" TO STR
    MOVE 8 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    MOVE 765 TO SINT32
    CALL "JsonEncode-Integer" USING JSONBUFFER JSONPOS SINT32
    *>   },
    CALL "JsonEncode-ObjectEnd" USING JSONBUFFER JSONPOS
    CALL "JsonEncode-Comma" USING JSONBUFFER JSONPOS

    *>   "players": {
    MOVE "players" TO STR
    MOVE 7 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    CALL "JsonEncode-ObjectStart" USING JSONBUFFER JSONPOS
    *>     "max": 1,
    MOVE "max" TO STR
    MOVE 3 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    MOVE 1 TO SINT32
    CALL "JsonEncode-Integer" USING JSONBUFFER JSONPOS SINT32
    CALL "JsonEncode-Comma" USING JSONBUFFER JSONPOS
    *>     "online": 0,
    MOVE "online" TO STR
    MOVE 6 TO STRLEN
    CALL "JsonEncode-ObjectKey" USING JSONBUFFER JSONPOS STR STRLEN
    MOVE 0 TO SINT32
    CALL "JsonEncode-Integer" USING JSONBUFFER JSONPOS SINT32
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
    MOVE LK-MOTD TO STR
    MOVE 0 TO STRLEN
    INSPECT STR TALLYING STRLEN FOR TRAILING SPACES
    COMPUTE STRLEN = (FUNCTION LENGTH(STR)) - STRLEN
    CALL "JsonEncode-String" USING JSONBUFFER JSONPOS STR STRLEN
    *> }
    CALL "JsonEncode-ObjectEnd" USING JSONBUFFER JSONPOS

    *> Build the payload: VarInt (JSON length) + JSON
    COMPUTE SINT32 = JSONPOS - 1
    CALL "Encode-VarInt" USING SINT32 PAYLOAD PAYLOADLEN
    MOVE JSONBUFFER TO PAYLOAD(PAYLOADLEN + 1:JSONPOS)
    ADD SINT32 TO PAYLOADLEN

    *> Send the packet
    CALL "SendPacket" USING LK-HNDL PACKET-ID PAYLOAD PAYLOADLEN LK-ERRNO.

END PROGRAM SendPacket-Status.
