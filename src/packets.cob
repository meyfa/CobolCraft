*> --- SendPacket ---
*> Send a raw packet to the client.
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    01 NUM-BYTES                BINARY-LONG UNSIGNED.
    01 HEADER                   PIC X(10).
    01 HEADER-OFFSET            BINARY-LONG UNSIGNED.
    01 TOTAL-LENGTH             BINARY-LONG UNSIGNED.
    01 HNDL                     PIC X(4).
    01 ERRNO                    PIC 9(3).
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-PACKET-ID             BINARY-LONG.
    01 LK-PAYLOAD               PIC X ANY LENGTH.
    01 LK-PAYLOAD-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-PACKET-ID LK-PAYLOAD LK-PAYLOAD-LENGTH.
    *> Don't send packet if the client is already in an error state. It will be disconnected on the next tick.
    IF CLIENT-ERRNO-SEND(LK-CLIENT) NOT = 0
        EXIT PROGRAM
    END-IF
    MOVE CLIENT-HNDL(LK-CLIENT) TO HNDL

    *> Packet length = length of packet ID + length of payload
    CALL "Encode-GetVarIntLength" USING LK-PACKET-ID NUM-BYTES
    COMPUTE TOTAL-LENGTH = NUM-BYTES + LK-PAYLOAD-LENGTH

    *> Send header: payload length, packet ID
    MOVE 1 TO HEADER-OFFSET
    CALL "Encode-VarInt" USING TOTAL-LENGTH HEADER HEADER-OFFSET
    CALL "Encode-VarInt" USING LK-PACKET-ID HEADER HEADER-OFFSET
    COMPUTE NUM-BYTES = HEADER-OFFSET - 1
    CALL "SocketWrite" USING HNDL NUM-BYTES HEADER GIVING ERRNO
    PERFORM HandleError

    *> Send packet data
    CALL "SocketWrite" USING HNDL LK-PAYLOAD-LENGTH LK-PAYLOAD GIVING ERRNO
    PERFORM HandleError

    GOBACK.

HandleError SECTION.
    IF ERRNO NOT = 0
        *> Mark the client as errored
        MOVE ERRNO TO CLIENT-ERRNO-SEND(LK-CLIENT)
        EXIT PROGRAM
    END-IF

    EXIT SECTION.

END PROGRAM SendPacket.
