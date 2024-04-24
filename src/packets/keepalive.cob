IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-KeepAlive.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID    BINARY-LONG             VALUE H'26'.
    *> buffer used to store the packet data
    01 PAYLOAD      PIC X(8).
    01 PAYLOADLEN   BINARY-LONG UNSIGNED    VALUE 8.
LINKAGE SECTION.
    01 LK-HNDL          PIC X(4).
    01 LK-ERRNO         PIC 9(3).
    01 LK-KEEPALIVE-ID  BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO LK-KEEPALIVE-ID.
    CALL "Encode-Long" USING LK-KEEPALIVE-ID PAYLOAD PAYLOADLEN
    CALL "SendPacket" USING LK-HNDL PACKET-ID PAYLOAD PAYLOADLEN LK-ERRNO
    GOBACK.

END PROGRAM SendPacket-KeepAlive.
