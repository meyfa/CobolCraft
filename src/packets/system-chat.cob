IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-SystemChat.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'69'.
    *> temporary data used during encoding
    01 UINT16           BINARY-SHORT UNSIGNED.
    01 BUFFER           PIC X(8).
    01 BUFFERLEN        BINARY-LONG UNSIGNED.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(64000).
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-HNDL          PIC X(4).
    01 LK-ERRNO         PIC 9(3).
    01 LK-MESSAGE       PIC X ANY LENGTH.
    01 LK-MESSAGE-LEN   BINARY-LONG UNSIGNED.
    01 LK-COLOR         PIC X(16).

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO LK-MESSAGE LK-MESSAGE-LEN LK-COLOR.
    MOVE 0 TO PAYLOADLEN

    *> NBT compound tag
    MOVE X"0A" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> "text" key
    MOVE X"08" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN
    MOVE 4 TO UINT16
    CALL "Encode-UnsignedShort" USING UINT16 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN
    MOVE "text" TO PAYLOAD(PAYLOADLEN + 1:4)
    ADD 4 TO PAYLOADLEN

    *> text
    MOVE LK-MESSAGE-LEN TO UINT16
    CALL "Encode-UnsignedShort" USING UINT16 BUFFER BUFFERLEN
    MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN
    *> TODO: implement modified UTF-8: https://docs.oracle.com/javase/8/docs/api/java/io/DataInput.html#modified-utf-8
    MOVE LK-MESSAGE(1:LK-MESSAGE-LEN) TO PAYLOAD(PAYLOADLEN + 1:LK-MESSAGE-LEN)
    ADD LK-MESSAGE-LEN TO PAYLOADLEN

    IF LK-COLOR NOT = SPACES
       *> "color" key
       MOVE X"08" TO PAYLOAD(PAYLOADLEN + 1:1)
       ADD 1 TO PAYLOADLEN
       MOVE 5 TO UINT16
       CALL "Encode-UnsignedShort" USING UINT16 BUFFER BUFFERLEN
       MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
       ADD BUFFERLEN TO PAYLOADLEN
       MOVE "color" TO PAYLOAD(PAYLOADLEN + 1:5)
       ADD 5 TO PAYLOADLEN

       *> color
       MOVE FUNCTION STORED-CHAR-LENGTH(LK-COLOR) TO UINT16
       CALL "Encode-UnsignedShort" USING UINT16 BUFFER BUFFERLEN
       MOVE BUFFER TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
       ADD BUFFERLEN TO PAYLOADLEN
       MOVE LK-COLOR(1:UINT16) TO PAYLOAD(PAYLOADLEN + 1:UINT16)
       ADD UINT16 TO PAYLOADLEN
    END-IF

    *> NBT end tag
    MOVE X"00" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> "overlay" flag
    MOVE X"00" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> Send the packet
    CALL "SendPacket" USING LK-HNDL PACKET-ID PAYLOAD PAYLOADLEN LK-ERRNO

    GOBACK.

END PROGRAM SendPacket-SystemChat.
