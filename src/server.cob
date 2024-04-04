IDENTIFICATION DIVISION.
PROGRAM-ID. Server.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
SELECT FD-REGISTRY-BLOB ASSIGN TO "blobs/registry_packet.txt"
    ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD FD-REGISTRY-BLOB.
    01 REGISTRY-BLOB-REC    PIC X(64).

WORKING-STORAGE SECTION.
    *> Socket variables (server socket handle, client socket handle, error number)
    01 LISTEN           PIC X(4).
    01 HNDL             PIC X(4).
    01 ERRNO            PIC 9(3) VALUE 0.
    *> State of the player (0 = handshake, 1 = status, 2 = login, 3 = configuration, 4 = play, 255 = disconnect)
    01 CLIENT-STATE     PIC 9(3) VALUE 0.
    *> Player data
    01 USERNAME         PIC X(16).
    01 USERNAME-LENGTH  PIC 9(5).
    01 CONFIG-FINISH    PIC 9(1) VALUE 0.
    01 KEEPALIVE-ID     PIC 9(10) VALUE 0.
    *> Packet reading: packet length (-1 if not yet known), packet buffer, read/decode position
    *> Note: Maximum packet length is 2^21-1 bytes - see: https://wiki.vg/Protocol#Packet_format
    01 PACKET-LENGTH    PIC S9(10).
    01 PACKET-BUFFER    PIC X(2100000).
    01 PACKET-POSITION  PIC 9(10).
    *> Incoming/outgoing packet data
    01 BYTE-COUNT       PIC 9(5).
    01 PACKET-ID        PIC S9(10).
    01 BUFFER           PIC X(64000).
    *> Temporary variables
    01 TEMP-BUFFER      PIC X(64000).
    01 TEMP-BYTE-COUNT  PIC 9(5).
    01 TEMP-INT32       PIC S9(10).
    01 TEMP-INT64       PIC S9(20).

LINKAGE SECTION.
    *> Configuration provided by main program
    01 SERVER-CONFIG.
        02 PORT                 PIC X(5).
        02 WHITELIST-ENABLE     PIC 9(1).
        02 WHITELIST-PLAYER     PIC X(16).
        02 MOTD                 PIC X(64).

PROCEDURE DIVISION USING SERVER-CONFIG.
StartServer.
    DISPLAY "Starting server...".
    CALL "Socket-Listen" USING PORT LISTEN ERRNO.
    PERFORM HandleError.

AcceptConnection.
    DISPLAY "Waiting for client..."
    CALL "Socket-Accept" USING LISTEN HNDL ERRNO
    PERFORM HandleError

    MOVE 0 TO CLIENT-STATE
    MOVE SPACES TO USERNAME
    MOVE 0 TO USERNAME-LENGTH
    MOVE 0 TO CONFIG-FINISH
    MOVE 0 TO KEEPALIVE-ID
    PERFORM ReceivePacket UNTIL CLIENT-STATE = 255

    DISPLAY "Disconnecting..."
    CALL "Socket-Close" USING HNDL ERRNO
    PERFORM HandleError

    GO TO AcceptConnection.

    STOP RUN.

ReceivePacket SECTION.
    *> Read packet length - read bytes one at a time until the VarInt becomes valid
    MOVE -1 TO PACKET-LENGTH
    MOVE 0 TO BYTE-COUNT        *> number of VarInt bytes read
    MOVE 1 TO TEMP-BYTE-COUNT   *> read one byte at a time
    PERFORM UNTIL PACKET-LENGTH >= 0 OR BYTE-COUNT > 5
        CALL "Read-Raw" USING HNDL TEMP-BYTE-COUNT ERRNO TEMP-BUFFER
        IF ERRNO = 2
            DISPLAY "Client lost connection"
            MOVE 255 TO CLIENT-STATE
            EXIT SECTION
        END-IF
        PERFORM HandleError
        ADD 1 TO BYTE-COUNT
        MOVE TEMP-BUFFER(1:1) TO BUFFER(BYTE-COUNT:1)
        *> This is the last VarInt byte if the most significant bit is not set.
        *> Note: ORD(...) returns the ASCII code of the character + 1, meaning we need to check for <= 128.
        IF FUNCTION ORD(BUFFER(BYTE-COUNT:1)) <= 128 THEN
            MOVE 1 TO PACKET-POSITION
            CALL "Decode-VarInt" USING BUFFER PACKET-POSITION PACKET-LENGTH
        END-IF
    END-PERFORM

    *> Validate packet length - note that it must be at least 1 due to the packet ID
    IF PACKET-LENGTH < 1 OR PACKET-LENGTH > 2097151 THEN
        DISPLAY "Invalid packet length: " PACKET-LENGTH
        MOVE 255 TO CLIENT-STATE
        EXIT SECTION
    END-IF

    *> Read the packet into the buffer - note that we may only read 64k at a time
    MOVE 1 TO PACKET-POSITION
    PERFORM UNTIL PACKET-POSITION > PACKET-LENGTH
        COMPUTE TEMP-INT32 = FUNCTION MIN(64000, PACKET-LENGTH - PACKET-POSITION + 1)
        MOVE TEMP-INT32 TO BYTE-COUNT
        CALL "Read-Raw" USING HNDL BYTE-COUNT ERRNO BUFFER
        PERFORM HandleError
        MOVE BUFFER(1:BYTE-COUNT) TO PACKET-BUFFER(PACKET-POSITION:BYTE-COUNT)
        ADD BYTE-COUNT TO PACKET-POSITION
    END-PERFORM

    *> Start decoding the packet by decoding the packet ID
    MOVE 1 TO PACKET-POSITION
    CALL "Decode-VarInt" USING PACKET-BUFFER PACKET-POSITION PACKET-ID

    DISPLAY "[state=" CLIENT-STATE "] Received packet: " PACKET-ID

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

    *> The final byte of the payload encodes the target state.
    COMPUTE CLIENT-STATE = FUNCTION ORD(PACKET-BUFFER(PACKET-LENGTH:1)) - 1
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
            CALL "SendPacket-Status" USING HNDL ERRNO MOTD
            PERFORM HandleError
        WHEN PACKET-ID = 1
            *> Ping request: respond with the same payload and close the connection
            DISPLAY "  Responding to ping request"
            COMPUTE BYTE-COUNT = 8
            MOVE PACKET-BUFFER(PACKET-POSITION:BYTE-COUNT) TO BUFFER(1:BYTE-COUNT)
            MOVE 1 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleError
            MOVE 255 TO CLIENT-STATE
        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandleLogin SECTION.
    EVALUATE TRUE
        *> Login start
        WHEN PACKET-ID = 0
            *> Decode username
            CALL "Decode-String" USING BY REFERENCE PACKET-BUFFER PACKET-POSITION USERNAME-LENGTH USERNAME
            DISPLAY "  Login with username: " USERNAME

            *> Skip the UUID (16 bytes)
            ADD 16 TO PACKET-POSITION

            IF WHITELIST-ENABLE > 0 AND USERNAME NOT = WHITELIST-PLAYER THEN
                DISPLAY "  Player not whitelisted: " USERNAME
                MOVE "Not whitelisted!" TO BUFFER
                MOVE 16 TO BYTE-COUNT
                CALL "SendPacket-LoginDisconnect" USING BY REFERENCE HNDL ERRNO BUFFER BYTE-COUNT
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

            *> Can move to configuration state
            DISPLAY "  Acknowledged login"
            ADD 1 TO CLIENT-STATE

        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandleConfiguration SECTION.
    EVALUATE TRUE
        *> Client information
        WHEN PACKET-ID = 0
            *> Note: payload is ignored for now
            DISPLAY "  Received client information"

            *> Send registry data
            OPEN INPUT FD-REGISTRY-BLOB
            MOVE 64 TO TEMP-BYTE-COUNT
            PERFORM UNTIL TEMP-BYTE-COUNT = 0
                MOVE SPACES TO TEMP-BUFFER(1:64)
                READ FD-REGISTRY-BLOB INTO TEMP-BUFFER
                    AT END
                        MOVE 0 TO TEMP-BYTE-COUNT
                    NOT AT END
                        CALL "DecodeHexString" USING TEMP-BUFFER TEMP-BYTE-COUNT BUFFER BYTE-COUNT
                        CALL "Write-Raw" USING BY REFERENCE HNDL BYTE-COUNT BUFFER ERRNO
                        PERFORM HandleError
                END-READ
            END-PERFORM
            CLOSE FD-REGISTRY-BLOB

            *> Send feature flags
            MOVE 0 TO BYTE-COUNT
            *> count=1
            MOVE 1 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> feature flag="minecraft:vanilla"
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(17 + 1) TO BUFFER(BYTE-COUNT:1)
            MOVE "minecraft:vanilla" TO BUFFER(BYTE-COUNT + 1:17)
            ADD 17 TO BYTE-COUNT
            *> send packet
            MOVE 8 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
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

            *> send "Login (play)"
            MOVE 0 TO BYTE-COUNT
            *> entity ID=0x00000001 (suffix of UUID)
            PERFORM 4 TIMES
                ADD 1 TO BYTE-COUNT
                MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            END-PERFORM
            MOVE FUNCTION CHAR(2) TO BUFFER(BYTE-COUNT:1)
            *> is hardcore=false
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> dimension count=1
            MOVE 1 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> dimension name array=["minecraft:overworld"]
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(19 + 1) TO BUFFER(BYTE-COUNT:1)
            MOVE "minecraft:overworld" TO BUFFER(BYTE-COUNT + 1:19)
            ADD 19 TO BYTE-COUNT
            *> max players=1
            MOVE 10 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> view distance=10
            MOVE 10 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> simulation distance=10
            MOVE 1 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> reduced debug info=false
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> enable respawn screen=true
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> do limited crafting=false
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> dimension type="minecraft:overworld"
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(19 + 1) TO BUFFER(BYTE-COUNT:1)
            MOVE "minecraft:overworld" TO BUFFER(BYTE-COUNT + 1:19)
            ADD 19 TO BYTE-COUNT
            *> dimension name="minecraft:overworld"
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(19 + 1) TO BUFFER(BYTE-COUNT:1)
            MOVE "minecraft:overworld" TO BUFFER(BYTE-COUNT + 1:19)
            ADD 19 TO BYTE-COUNT
            *> hashed seed=0 (8-byte long)
            PERFORM 8 TIMES
                ADD 1 TO BYTE-COUNT
                MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            END-PERFORM
            *> gamemode=1 (creative)
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(2) TO BUFFER(BYTE-COUNT:1)
            *> previous gamemode=-1
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(255 + 1) TO BUFFER(BYTE-COUNT:1)
            *> is debug=false
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> is flat=false
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> has death location=false
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> portal cooldown=0
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> send packet
            MOVE 41 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleError

            *> send game event "start waiting for level chunks"
            MOVE "06200d00000000" TO BUFFER
            MOVE 14 TO BYTE-COUNT
            CALL "DecodeHexString" USING BUFFER BYTE-COUNT TEMP-BUFFER TEMP-BYTE-COUNT
            CALL "Write-Raw" USING BY REFERENCE HNDL TEMP-BYTE-COUNT TEMP-BUFFER ERRNO
            PERFORM HandleError

            *> set ticking state
            MOVE "066e41a0000000" TO BUFFER
            MOVE 14 TO BYTE-COUNT
            CALL "DecodeHexString" USING BUFFER BYTE-COUNT TEMP-BUFFER TEMP-BYTE-COUNT
            CALL "Write-Raw" USING BY REFERENCE HNDL TEMP-BYTE-COUNT TEMP-BUFFER ERRNO
            PERFORM HandleError

            *> tick
            MOVE "026f00" TO BUFFER
            MOVE 6 TO BYTE-COUNT
            CALL "DecodeHexString" USING BUFFER BYTE-COUNT TEMP-BUFFER TEMP-BYTE-COUNT
            CALL "Write-Raw" USING BY REFERENCE HNDL TEMP-BYTE-COUNT TEMP-BUFFER ERRNO
            PERFORM HandleError

            *> send inventory ("Set Container Content" with window ID=0)
            MOVE 0 TO BYTE-COUNT
            *> window ID=0
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> state ID=0
            MOVE 0 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> count=46 (https://wiki.vg/Inventory#Player_Inventory)
            MOVE 46 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> slot data (all empty)
            PERFORM UNTIL TEMP-INT32 = 0
                ADD 1 TO BYTE-COUNT
                MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
                SUBTRACT 1 FROM TEMP-INT32
            END-PERFORM
            *> carried item (empty)
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> send packet
            MOVE 19 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleError

            *> send "Set Center Chunk"
            MOVE 0 TO BYTE-COUNT
            *> chunk X=0
            MOVE 0 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> chunk Z=0
            MOVE 0 TO TEMP-INT32
            CALL "Encode-VarInt" USING TEMP-INT32 TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> send packet
            MOVE 82 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleError

            *> TODO: send chunk data ("Chunk Data and Update Light")

            *> send position ("Synchronize Player Position")
            MOVE 0 TO BYTE-COUNT
            *> X=0
            MOVE FUNCTION CHAR(64 + 1) TO BUFFER(BYTE-COUNT + 1:1)
            MOVE FUNCTION CHAR(111 + 1) TO BUFFER(BYTE-COUNT + 2:1)
            MOVE FUNCTION CHAR(224 + 1) TO BUFFER(BYTE-COUNT + 3:1)
            ADD 3 TO BYTE-COUNT
            PERFORM 5 TIMES
                ADD 1 TO BYTE-COUNT
                MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            END-PERFORM
            *> Y=255 (IEEE 754 double-precision floating-point)
            MOVE FUNCTION CHAR(64 + 1) TO BUFFER(BYTE-COUNT + 1:1)
            MOVE FUNCTION CHAR(111 + 1) TO BUFFER(BYTE-COUNT + 2:1)
            MOVE FUNCTION CHAR(224 + 1) TO BUFFER(BYTE-COUNT + 3:1)
            ADD 3 TO BYTE-COUNT
            PERFORM 5 TIMES
                ADD 1 TO BYTE-COUNT
                MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            END-PERFORM
            *> Z=0
            PERFORM 8 TIMES
                ADD 1 TO BYTE-COUNT
                MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            END-PERFORM
            *> yaw=pitch=0
            PERFORM UNTIL BYTE-COUNT = 32
                ADD 1 TO BYTE-COUNT
                MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            END-PERFORM
            *> flags=0
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> teleport ID=0
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> send packet
            MOVE 62 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleError

            *> TODO: receive "Confirm Teleportation"

        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandlePlay SECTION.
    *> TODO: implement packets

    EVALUATE TRUE
        *> Set player position
        WHEN PACKET-ID = 23
            CONTINUE
        *> Set player position and rotation
        WHEN PACKET-ID = 24
            CONTINUE
        *> Set player rotation
        WHEN PACKET-ID = 25
            CONTINUE
        *> Set player on ground
        WHEN PACKET-ID = 26
            CONTINUE
    END-EVALUATE

    *> Send keep-alive (TODO: move this out of packet handling!)
    *> but not in reaction to a keep-alive response packet, for obvious reasons
    *> Note: We abuse the fact that the client sends movement packets at least once per second, even standing still.
    IF PACKET-ID NOT = 21 THEN
        ADD 1 TO KEEPALIVE-ID
        MOVE 0 TO BYTE-COUNT
        MOVE KEEPALIVE-ID TO TEMP-INT64
        CALL "Encode-Long" USING TEMP-INT64 TEMP-BUFFER TEMP-BYTE-COUNT
        MOVE TEMP-BUFFER(1:TEMP-BYTE-COUNT) TO BUFFER(1:TEMP-BYTE-COUNT)
        ADD TEMP-BYTE-COUNT TO BYTE-COUNT
        MOVE 36 TO PACKET-ID
        CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
        PERFORM HandleError
    END-IF

    EXIT SECTION.

HandleError SECTION.
    IF ERRNO NOT = 0 THEN
        DISPLAY "Error: " ERRNO
        STOP RUN
    END-IF.

    EXIT SECTION.

END PROGRAM Server.
