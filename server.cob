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
    *> State of the player (0 = handshake, 1 = status, 2 = login, 3 = configuration, 4 = play, 255 = disconnect)
    01 CLIENT-STATE     PIC 9(3) VALUE 0.
    *> Player data
    01 USERNAME         PIC X(16).
    01 USERNAME-LENGTH  PIC 9(3).
    01 CONFIG-FINISH    PIC 9(1) VALUE 0.
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
    MOVE SPACES TO USERNAME.
    MOVE 0 TO USERNAME-LENGTH.
    MOVE 0 TO CONFIG-FINISH.
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
            PERFORM HandleConfiguration
        WHEN CLIENT-STATE = 4
            PERFORM HandlePlay
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
            MOVE PACKET-LENGTH TO BYTE-COUNT
            CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
            PERFORM HandleError
    END-EVALUATE.

    EXIT SECTION.

HandleLogin SECTION.
    EVALUATE TRUE
        *> Login start
        WHEN PACKET-ID = 0
            *> Read username
            CALL "Read-String" USING HNDL ERRNO BYTE-COUNT USERNAME
            PERFORM HandleError
            MOVE BYTE-COUNT TO USERNAME-LENGTH
            DISPLAY "  Login with username: " USERNAME

            *> Read UUID (since we don't need it, we just skip it)
            MOVE 16 TO BYTE-COUNT
            CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
            PERFORM HandleError

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

            *> Send login success. This should result in a "login acknowledged" packet by the client.
            *> UUID of the player (value: 00000...01)
            MOVE 0 TO BYTE-COUNT
            PERFORM UNTIL BYTE-COUNT = 15
                ADD 1 TO BYTE-COUNT
                MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            END-PERFORM
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(2) TO BUFFER(BYTE-COUNT:1)
            *> Username (string prefixed with VarInt length)
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(USERNAME-LENGTH + 1) TO BUFFER(BYTE-COUNT:1)
            MOVE USERNAME(1:USERNAME-LENGTH) TO BUFFER(BYTE-COUNT + 1:USERNAME-LENGTH)
            ADD USERNAME-LENGTH TO BYTE-COUNT
            *> Number of properties
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> End of properties
            *> send packet
            MOVE 2 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleError

        *> Login acknowledge
        WHEN PACKET-ID = 3
            *> Must not happen before login start
            IF USERNAME-LENGTH = 0 THEN
                DISPLAY "  Unexpected login acknowledge"
                MOVE 255 TO CLIENT-STATE
                EXIT SECTION
            END-IF

            *> We don't expect any payload, but better safe than sorry
            MOVE PACKET-LENGTH TO BYTE-COUNT
            CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
            PERFORM HandleError

            *> Can move to configuration state
            DISPLAY "  Acknowledged login"
            ADD 1 TO CLIENT-STATE

        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
            MOVE PACKET-LENGTH TO BYTE-COUNT
            CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
            PERFORM HandleError
    END-EVALUATE.

    EXIT SECTION.

HandleConfiguration SECTION.
    EVALUATE TRUE
        *> Client information
        WHEN PACKET-ID = 0
            DISPLAY "  Received client information"

            *> Read payload
            MOVE PACKET-LENGTH TO BYTE-COUNT
            CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
            PERFORM HandleError

            *> Send finish configuration
            MOVE 2 TO PACKET-ID
            MOVE 0 TO BYTE-COUNT
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleError

            *> We now expect an acknowledge packet
            MOVE 1 TO CONFIG-FINISH

        *> Acknowledge finish configuration
        WHEN PACKET-ID = 2
            IF CONFIG-FINISH = 0 THEN
                DISPLAY "  Unexpected acknowledge finish configuration"
                MOVE 255 TO CLIENT-STATE
                EXIT SECTION
            END-IF

            *> Can move to play state
            DISPLAY "  Acknowledged finish configuration"
            ADD 1 TO CLIENT-STATE

            *> TODO: send join game
            *> TOOD: send inventory
            *> TODO: send chunks
            *> TODO: send position
            *> TODO: spawn player

        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
            MOVE PACKET-LENGTH TO BYTE-COUNT
            CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
            PERFORM HandleError
    END-EVALUATE.

    EXIT SECTION.

HandlePlay SECTION.
    *> Consume the packet
    MOVE PACKET-LENGTH TO BYTE-COUNT
    CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
    PERFORM HandleError.

    DISPLAY "  Play state not implemented"
    *> MOVE 255 TO CLIENT-STATE

    EXIT SECTION.

HandleError SECTION.
    IF ERRNO NOT = 0 THEN
        DISPLAY "Error: " ERRNO
        STOP RUN
    END-IF.

    EXIT SECTION.

END PROGRAM server.
