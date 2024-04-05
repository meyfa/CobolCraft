*> --- SendPacket ---
*> Send a raw packet to the client.
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BUFFER                   PIC X(64000).
    01 PACKET-LENGTH            BINARY-LONG UNSIGNED.
LOCAL-STORAGE SECTION.
    01 NUM-BYTES                BINARY-LONG.
LINKAGE SECTION.
    01 LK-HNDL                  PIC X(4).
    01 LK-PACKET-ID             BINARY-LONG.
    01 LK-PAYLOAD               PIC X(64000).
    01 LK-PAYLOAD-LENGTH        BINARY-LONG UNSIGNED.
    01 LK-ERRNO                 PIC 9(3).

PROCEDURE DIVISION USING LK-HNDL LK-PACKET-ID LK-PAYLOAD LK-PAYLOAD-LENGTH LK-ERRNO.
    *> Encode packet ID to find the total payload length (TODO: optimize this)
    CALL "Encode-VarInt" USING LK-PACKET-ID BUFFER NUM-BYTES
    COMPUTE PACKET-LENGTH = NUM-BYTES + LK-PAYLOAD-LENGTH

    *> Send payload length
    CALL "Encode-VarInt" USING PACKET-LENGTH BUFFER NUM-BYTES
    CALL "Socket-Write" USING LK-HNDL LK-ERRNO NUM-BYTES BUFFER
    PERFORM HandleError

    *> Send packet ID
    CALL "Encode-VarInt" USING LK-PACKET-ID BUFFER NUM-BYTES
    CALL "Socket-Write" USING LK-HNDL LK-ERRNO NUM-BYTES BUFFER
    PERFORM HandleError

    *> Send packet data
    CALL "Socket-Write" USING LK-HNDL LK-ERRNO LK-PAYLOAD-LENGTH LK-PAYLOAD
    PERFORM HandleError

    GOBACK.

HandleError SECTION.
    IF LK-ERRNO NOT = 0
        EXIT PROGRAM
    END-IF

    EXIT SECTION.

END PROGRAM SendPacket.
