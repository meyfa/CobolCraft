IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-MovePlayerStatus.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    01 PLAYER-ID                BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    *> TODO: "on ground" flag
    ADD 1 TO LK-OFFSET

    *> ignore movement until position is confirmed
    IF TELEPORT-RECV(LK-CLIENT) NOT = TELEPORT-SENT(LK-CLIENT)
        GOBACK
    END-IF

    GOBACK.

END PROGRAM RecvPacket-MovePlayerStatus.
