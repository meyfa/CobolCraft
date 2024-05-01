*> --- SendPacket ---
*> Send a raw packet to the client.
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    01 BUFFER                   PIC X(8).
    01 TOTAL-LENGTH             BINARY-LONG UNSIGNED.
    01 HNDL                     PIC X(4).
    01 ERRNO                    PIC 9(3).
LOCAL-STORAGE SECTION.
    01 NUM-BYTES                BINARY-LONG UNSIGNED.
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

    *> Add length of packet ID to payload length
    CALL "Encode-GetVarIntLength" USING LK-PACKET-ID NUM-BYTES
    COMPUTE TOTAL-LENGTH = NUM-BYTES + LK-PAYLOAD-LENGTH

    *> Send payload length
    CALL "Encode-VarInt" USING TOTAL-LENGTH BUFFER NUM-BYTES
    CALL "Socket-Write" USING HNDL ERRNO NUM-BYTES BUFFER
    PERFORM HandleError

    *> Send packet ID
    CALL "Encode-VarInt" USING LK-PACKET-ID BUFFER NUM-BYTES
    CALL "Socket-Write" USING HNDL ERRNO NUM-BYTES BUFFER
    PERFORM HandleError

    *> Send packet data
    CALL "Socket-Write" USING HNDL ERRNO LK-PAYLOAD-LENGTH LK-PAYLOAD
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
