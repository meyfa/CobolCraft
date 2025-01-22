IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-ContainerClose.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    01 PLAYER-ID                BINARY-LONG.
    01 WINDOW-ID                BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    *> TODO handle carried item (mouse item) - transfer to inventory when closing

    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET WINDOW-ID

    IF WINDOW-ID = 0 OR PLAYER-WINDOW-ID(PLAYER-ID) NOT = WINDOW-ID
        *> player inventory, or different window than expected - ignore
        GOBACK
    END-IF

    MOVE 0 TO PLAYER-WINDOW-ID(PLAYER-ID)

    GOBACK.

END PROGRAM RecvPacket-ContainerClose.
