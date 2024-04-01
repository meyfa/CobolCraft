IDENTIFICATION DIVISION.
PROGRAM-ID. server.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 PORT             PIC X(5) VALUE "25565".
01 LISTEN           PIC X(4).
01 HNDL             PIC X(4).
01 ERRNO            PIC 9(3) VALUE 0.
*> State of the player (0 = handshake, 1 = status, 2 = login, 3 = play, 255 = disconnect)
01 CLIENT-STATE     PIC 9(3) VALUE 0.

PROCEDURE DIVISION.

Main.
    DISPLAY "Starting server...".
    CALL "Socket-Listen" USING PORT LISTEN ERRNO.
    PERFORM HandleError.

AcceptConnection.
    DISPLAY "Waiting for client..."
    CALL "Socket-Accept" USING LISTEN HNDL ERRNO.
    PERFORM HandleError.

    MOVE 0 TO CLIENT-STATE.
    PERFORM ReceivePacket UNTIL CLIENT-STATE = 255.

    DISPLAY "Disconnecting..."
    CALL "Socket-Close" USING HNDL ERRNO.
    PERFORM HandleError.

    GO TO AcceptConnection.

    STOP RUN.

ReceivePacket SECTION.
    *> TODO: Implement packet handling
    MOVE 255 TO CLIENT-STATE.

    EXIT SECTION.

HandleError SECTION.
    IF ERRNO NOT = 0 THEN
        DISPLAY "Error: " ERRNO
        STOP RUN
    END-IF.

END PROGRAM server.
