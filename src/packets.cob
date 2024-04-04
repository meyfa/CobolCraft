*> --- SendPacket ---
*> Send a raw packet to the client.
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BUFFER                   PIC X(64000).
    01 PACKET-LENGTH            PIC 9(5).
LOCAL-STORAGE SECTION.
    01 NUM-BYTES                PIC 9(5).
    01 INT32                    PIC S9(10).
LINKAGE SECTION.
    01 LK-HNDL                  PIC X(4).
    01 LK-PACKET-ID             PIC S9(10).
    01 LK-PAYLOAD               PIC X(64000).
    01 LK-PAYLOAD-LENGTH        PIC 9(5).
    01 LK-ERRNO                 PIC 9(3).

PROCEDURE DIVISION USING LK-HNDL LK-PACKET-ID LK-PAYLOAD LK-PAYLOAD-LENGTH LK-ERRNO.
    *> Encode packet ID to find the total payload length (TODO: optimize this)
    MOVE LK-PACKET-ID TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER NUM-BYTES
    COMPUTE PACKET-LENGTH = NUM-BYTES + LK-PAYLOAD-LENGTH

    *> Send payload length
    MOVE PACKET-LENGTH TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER NUM-BYTES
    MOVE NUM-BYTES TO INT32
    CALL "Socket-Write" USING LK-HNDL LK-ERRNO INT32 BUFFER
    PERFORM HandleError

    *> Send packet ID
    MOVE LK-PACKET-ID TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER NUM-BYTES
    MOVE NUM-BYTES TO INT32
    CALL "Socket-Write" USING LK-HNDL LK-ERRNO INT32 BUFFER
    PERFORM HandleError

    *> Send packet data
    MOVE LK-PAYLOAD-LENGTH TO INT32
    CALL "Socket-Write" USING LK-HNDL LK-ERRNO INT32 LK-PAYLOAD
    PERFORM HandleError

    GOBACK.

HandleError SECTION.
    IF LK-ERRNO NOT = 0
        EXIT PROGRAM
    END-IF

    EXIT SECTION.

END PROGRAM SendPacket.
