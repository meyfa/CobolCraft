IDENTIFICATION DIVISION.
PROGRAM-ID. server.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PORT             PIC X(5) VALUE "25565".
    01 WHITELIST-ENABLE PIC 9(1) VALUE 0.
    01 WHITELIST-PLAYER PIC X(16) VALUE "Notch".
    *> Socket variables (server socket handle, client socket handle, error number)
    01 LISTEN           PIC X(4).
    01 HNDL             PIC X(4).
    01 ERRNO            PIC 9(3) VALUE 0.
    *> State of the player (0 = handshake, 1 = status, 2 = login, 3 = play, 255 = disconnect)
    01 CLIENT-STATE     PIC 9(3) VALUE 0.
    *> Player data
    01 USERNAME         PIC X(16).
    01 USERNAME-LENGTH  PIC 9(3).
    *> Incoming packet data
    01 BYTE-COUNT       PIC 9(5).
    01 PACKET-LENGTH    PIC S9(10).
    01 PACKET-ID        PIC S9(10).
    01 BUFFER           PIC X(64000).

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
    *> Read packet length
    CALL "Read-VarInt" USING HNDL ERRNO BYTE-COUNT PACKET-LENGTH.
    PERFORM HandleError.

    *> Read packet ID
    CALL "Read-VarInt" USING HNDL ERRNO BYTE-COUNT PACKET-ID.
    PERFORM HandleError.
    SUBTRACT BYTE-COUNT FROM PACKET-LENGTH GIVING PACKET-LENGTH.

    DISPLAY "[state=" CLIENT-STATE "] Received packet ID: " PACKET-ID " with length " PACKET-LENGTH " bytes.".

    *> Handshake
    EVALUATE TRUE
        WHEN CLIENT-STATE = 0
            PERFORM HandleHandshake
        WHEN CLIENT-STATE = 1
            PERFORM HandleStatus
        WHEN CLIENT-STATE = 2
            PERFORM HandleLogin
        WHEN CLIENT-STATE = 3
            *> TODO: Implement play state
            MOVE 255 TO CLIENT-STATE
        WHEN OTHER
            DISPLAY "  Invalid state: " CLIENT-STATE
            MOVE 255 TO CLIENT-STATE
    END-EVALUATE.

    EXIT SECTION.

HandleHandshake SECTION.
    IF PACKET-ID NOT = 0 THEN
        DISPLAY "  Unexpected packet ID: " PACKET-ID
        MOVE 255 TO CLIENT-STATE
        EXIT SECTION
    END-IF

    *> Read payload. The final byte encodes the target state.
    MOVE PACKET-LENGTH TO BYTE-COUNT
    CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
    PERFORM HandleError
    MOVE FUNCTION ORD(BUFFER(BYTE-COUNT:1)) TO CLIENT-STATE
    SUBTRACT 1 FROM CLIENT-STATE

    *> Validate target state
    IF CLIENT-STATE NOT = 1 AND CLIENT-STATE NOT = 2 THEN
        DISPLAY "  Invalid target state: " CLIENT-STATE
        MOVE 255 TO CLIENT-STATE
    ELSE
        DISPLAY "  Target state: " CLIENT-STATE
    END-IF

    EXIT SECTION.

HandleStatus SECTION.
    EVALUATE TRUE
        WHEN PACKET-ID = 0
            *> Status request
            DISPLAY "  Responding to status request"
            MOVE 0 TO PACKET-ID
            MOVE " {""version"":{""name"":""1.20.4"",""protocol"":765},""players"":{""max"":1,""online"":0,""sample"":[]},""description"":{""text"":""CobolCraft""}}" TO BUFFER
            MOVE FUNCTION CHAR(123 + 1) TO BUFFER(1:1)
            MOVE 124 TO BYTE-COUNT
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleError
        WHEN PACKET-ID = 1
            *> Ping request: respond with the same payload and close the connection
            DISPLAY "  Responding to ping request"
            MOVE PACKET-LENGTH TO BYTE-COUNT
            CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
            PERFORM HandleError
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleError
            MOVE 255 TO CLIENT-STATE
        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
            MOVE 255 TO CLIENT-STATE
    END-EVALUATE.

    EXIT SECTION.

HandleLogin SECTION.
    IF PACKET-ID NOT = 0 THEN
        DISPLAY "  Unexpected packet ID: " PACKET-ID
        MOVE 255 TO CLIENT-STATE
        EXIT SECTION
    END-IF

    CALL "Read-String" USING HNDL ERRNO BYTE-COUNT USERNAME
    MOVE BYTE-COUNT TO USERNAME-LENGTH
    DISPLAY "  Login with username: " USERNAME

    IF WHITELIST-ENABLE > 0 AND USERNAME NOT = WHITELIST-PLAYER THEN
        DISPLAY "  Player not whitelisted: " USERNAME
        MOVE 0 TO PACKET-ID
        MOVE " {""text"":""Not whitelisted!""}" TO BUFFER
        MOVE FUNCTION CHAR(29 + 1) TO BUFFER(1:1)
        MOVE 30 TO BYTE-COUNT
        CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
        PERFORM HandleError
        MOVE 255 TO CLIENT-STATE
        EXIT SECTION
    END-IF

    *> For now, just disconnect the player
    MOVE 0 TO PACKET-ID
    MOVE " {""text"":""Not implemented!""}" TO BUFFER
    MOVE FUNCTION CHAR(29 + 1) TO BUFFER(1:1)
    MOVE 30 TO BYTE-COUNT
    CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
    PERFORM HandleError
    MOVE 255 TO CLIENT-STATE

    *> TODO: send login success
    *> TODO: send join game
    *> TOOD: send inventory
    *> TODO: send chunks
    *> TODO: send position
    *> TODO: spawn player

    EXIT SECTION.

HandleError SECTION.
    IF ERRNO NOT = 0 THEN
        DISPLAY "Error: " ERRNO
        STOP RUN
    END-IF.

    EXIT SECTION.

END PROGRAM server.
