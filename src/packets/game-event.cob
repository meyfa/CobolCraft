IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-GameEvent.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE H'22'.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(8).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-EVENT         BINARY-CHAR UNSIGNED.
    01 LK-VALUE         FLOAT-SHORT.

PROCEDURE DIVISION USING LK-CLIENT LK-EVENT LK-VALUE.
    MOVE 1 TO PAYLOADPOS

    *> event type
    MOVE FUNCTION CHAR(LK-EVENT + 1) TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> event value
    CALL "Encode-Float" USING LK-VALUE PAYLOAD PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-GameEvent.
