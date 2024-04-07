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
    01 ERRNO            PIC 9(3)                VALUE 0.
    *> State of the player (0 = handshake, 1 = status, 2 = login, 3 = configuration, 4 = play, -1 = disconnect)
    01 CLIENT-STATE     BINARY-CHAR             VALUE 0.
    *> Player data
    01 USERNAME         PIC X(16).
    01 USERNAME-LENGTH  BINARY-LONG.
    01 CONFIG-FINISH    BINARY-CHAR             VALUE 0.
    01 PLAYER-X         FLOAT-LONG              VALUE 0.
    01 PLAYER-Y         FLOAT-LONG              VALUE 64.
    01 PLAYER-Z         FLOAT-LONG              VALUE 0.
    01 PLAYER-YAW       FLOAT-SHORT             VALUE 0.
    01 PLAYER-PITCH     FLOAT-SHORT             VALUE 0.
    *> Last keepalive ID sent and received
    01 KEEPALIVE-SENT   BINARY-LONG-LONG        VALUE 0.
    01 KEEPALIVE-RECV   BINARY-LONG-LONG        VALUE 0.
    01 KEEPALIVE-DELTA  BINARY-LONG-LONG.
    *> Packet reading: packet length (-1 if not yet known), packet buffer, read/decode position, timeout
    *> Note: Maximum packet length is 2^21-1 bytes - see: https://wiki.vg/Protocol#Packet_format
    01 PACKET-LENGTH    BINARY-LONG.
    01 PACKET-BUFFER    PIC X(2100000).
    01 PACKET-POSITION  BINARY-LONG UNSIGNED.
    01 TIMEOUT-MS       BINARY-SHORT UNSIGNED.
    *> Incoming/outgoing packet data
    01 PACKET-ID        BINARY-LONG.
    01 BUFFER           PIC X(64000).
    01 BYTE-COUNT       BINARY-LONG UNSIGNED.
    *> Temporary variables
    01 TEMP-BUFFER      PIC X(64000).
    01 TEMP-BYTE-COUNT  BINARY-LONG UNSIGNED.
    01 TEMP-INT32       BINARY-LONG.
    01 TEMP-DOUBLE      FLOAT-LONG.
    01 TEMP-POSITION.
        02 TEMP-POSITION-X  BINARY-LONG.
        02 TEMP-POSITION-Y  BINARY-LONG.
        02 TEMP-POSITION-Z  BINARY-LONG.
    *> Time measurement
    01 CURRENT-TIME     BINARY-LONG-LONG.
    01 TICK-ENDTIME     BINARY-LONG-LONG.
    *> Variables for working with chunks
    01 CHUNK-X          BINARY-LONG.
    01 CHUNK-Z          BINARY-LONG.
    01 CHUNK-INDEX      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX      BINARY-LONG UNSIGNED.
    *> World storage (7x7 chunks, each 16x384x16 blocks)
    01 WORLD-CHUNKS.
        02 WORLD-CHUNKS-COUNT-X BINARY-LONG VALUE 7.
        02 WORLD-CHUNKS-COUNT-Z BINARY-LONG VALUE 7.
        02 WORLD-CHUNK OCCURS 49 TIMES.
            03 WORLD-CHUNK-X BINARY-LONG.
            03 WORLD-CHUNK-Z BINARY-LONG.
            *> block IDs (16x384x16) - X increases fastest, then Z, then Y
            03 WORLD-CHUNK-BLOCKS.
                04 WORLD-BLOCK OCCURS 98304 TIMES.
                    05 WORLD-BLOCK-ID BINARY-CHAR UNSIGNED VALUE 0.

LINKAGE SECTION.
    *> Configuration provided by main program
    01 SERVER-CONFIG.
        02 PORT                 PIC X(5).
        02 WHITELIST-ENABLE     BINARY-CHAR.
        02 WHITELIST-PLAYER     PIC X(16).
        02 MOTD                 PIC X(64).

PROCEDURE DIVISION USING SERVER-CONFIG.
GenerateWorld.
    DISPLAY "Generating world..."
    PERFORM VARYING CHUNK-Z FROM -3 BY 1 UNTIL CHUNK-Z > 3
        PERFORM VARYING CHUNK-X FROM -3 BY 1 UNTIL CHUNK-X > 3
            COMPUTE CHUNK-INDEX = (CHUNK-Z + 3) * 7 + CHUNK-X + 3 + 1
            MOVE CHUNK-X TO WORLD-CHUNK-X(CHUNK-INDEX)
            MOVE CHUNK-Z TO WORLD-CHUNK-Z(CHUNK-INDEX)

            *> turn all blocks with Y < 63 (i.e., the bottom 128 blocks) into stone
            PERFORM VARYING TEMP-POSITION-Y FROM 0 BY 1 UNTIL TEMP-POSITION-Y >= 128
                PERFORM VARYING TEMP-POSITION-Z FROM 0 BY 1 UNTIL TEMP-POSITION-Z >= 16
                    PERFORM VARYING TEMP-POSITION-X FROM 0 BY 1 UNTIL TEMP-POSITION-X >= 16
                        COMPUTE BLOCK-INDEX = (TEMP-POSITION-Y * 16 + TEMP-POSITION-Z) * 16 + TEMP-POSITION-X + 1
                        MOVE 1 TO WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX)
                    END-PERFORM
                END-PERFORM
            END-PERFORM

            *> turn all blocks with Y = 63 (i.e., the top 16 blocks) into grass
            *> Note: grass has ID 9 with the 1.20.4 registry and no data packs/mods, but this may change
            *> TODO: find a more permanent solution to get a specific block ID
            MOVE 127 TO TEMP-POSITION-Y
            PERFORM VARYING TEMP-POSITION-Z FROM 0 BY 1 UNTIL TEMP-POSITION-Z >= 16
                PERFORM VARYING TEMP-POSITION-X FROM 0 BY 1 UNTIL TEMP-POSITION-X >= 16
                    COMPUTE BLOCK-INDEX = (TEMP-POSITION-Y * 16 + TEMP-POSITION-Z) * 16 + TEMP-POSITION-X + 1
                    MOVE 9 TO WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX)
                END-PERFORM
            END-PERFORM
        END-PERFORM
    END-PERFORM.

StartServer.
    DISPLAY "Starting server..."
    CALL "Util-IgnoreSIGPIPE"
    CALL "Socket-Listen" USING PORT LISTEN ERRNO
    PERFORM HandleServerError
    .

AcceptConnection.
    DISPLAY "Waiting for client..."
    CALL "Socket-Accept" USING LISTEN HNDL ERRNO
    PERFORM HandleServerError

    MOVE 0 TO CLIENT-STATE
    MOVE SPACES TO USERNAME
    MOVE 0 TO USERNAME-LENGTH
    MOVE 0 TO CONFIG-FINISH

    MOVE 0 TO KEEPALIVE-SENT
    MOVE 0 TO KEEPALIVE-RECV

    MOVE -1 TO PACKET-LENGTH
    MOVE 1 TO PACKET-POSITION

    *> Loop until the client disconnects - each iteration is one game tick (1/20th of a second).
    PERFORM UNTIL CLIENT-STATE < 0
        CALL "Util-SystemTimeMillis" USING CURRENT-TIME
        COMPUTE TICK-ENDTIME = CURRENT-TIME + (1000 / 20)

        *> Here is where we would do the game loop.

        PERFORM KeepAlive

        *> The remaining time of this tick can be used for receiving packets.
        PERFORM UNTIL CURRENT-TIME >= TICK-ENDTIME OR CLIENT-STATE < 0
            PERFORM ReceivePacket
            CALL "Util-SystemTimeMillis" USING CURRENT-TIME
        END-PERFORM
    END-PERFORM

    DISPLAY "Client disconnected."
    CALL "Socket-Close" USING HNDL ERRNO
    PERFORM HandleServerError

    GO TO AcceptConnection.

    STOP RUN.

KeepAlive SECTION.
    *> Give the client some time for keepalive when the connection is established
    IF KEEPALIVE-RECV = 0
        MOVE CURRENT-TIME TO KEEPALIVE-RECV
    END-IF

    *> If the client has not responded to keepalive within 15 seconds, disconnect
    COMPUTE KEEPALIVE-DELTA = CURRENT-TIME - KEEPALIVE-RECV
    IF KEEPALIVE-DELTA >= 15000
        DISPLAY "Client timed out"
        MOVE -1 TO CLIENT-STATE
        EXIT SECTION
    END-IF

    *> Send keepalive packet every second, but only in play state
    COMPUTE KEEPALIVE-DELTA = CURRENT-TIME - KEEPALIVE-SENT
    IF CLIENT-STATE = 4 AND KEEPALIVE-DELTA >= 1000
        MOVE CURRENT-TIME TO KEEPALIVE-SENT
        CALL "SendPacket-KeepAlive" USING HNDL ERRNO KEEPALIVE-SENT
        PERFORM HandleClientError
    END-IF

    EXIT SECTION.

ReceivePacket SECTION.
    *> If the packet length is not yet known, try to read more bytes one by one until the VarInt is valid
    IF PACKET-LENGTH < 0 THEN
        MOVE 1 TO BYTE-COUNT
        MOVE 1 TO TIMEOUT-MS
        CALL "Socket-Read" USING HNDL ERRNO BYTE-COUNT BUFFER TIMEOUT-MS
        PERFORM HandleClientError

        *> Check if anything was read. If not, just try again later.
        IF BYTE-COUNT = 0 THEN
            EXIT SECTION
        END-IF

        MOVE BUFFER(1:1) TO PACKET-BUFFER(PACKET-POSITION:1)
        ADD 1 TO PACKET-POSITION

        *> This is the last VarInt byte if the most significant bit is not set.
        *> Note: ORD(...) returns the ASCII code of the character + 1, meaning we need to check for <= 128.
        IF FUNCTION ORD(BUFFER(1:1)) <= 128 THEN
            MOVE 1 TO PACKET-POSITION
            CALL "Decode-VarInt" USING PACKET-BUFFER PACKET-POSITION PACKET-LENGTH
        END-IF

        *> Validate packet length - note that it must be at least 1 due to the packet ID
        IF PACKET-LENGTH < 1 OR PACKET-LENGTH > 2097151 THEN
            DISPLAY "Invalid packet length: " PACKET-LENGTH
            MOVE -1 TO CLIENT-STATE
            EXIT SECTION
        END-IF

        *> The length is now known. The remainder of the packet can be read in further invocations.
        MOVE 1 TO PACKET-POSITION
        EXIT SECTION
    END-IF

    *> Read more bytes if necessary
    IF PACKET-POSITION <= PACKET-LENGTH THEN
        COMPUTE BYTE-COUNT = PACKET-LENGTH - PACKET-POSITION + 1
        COMPUTE BYTE-COUNT = FUNCTION MIN(BYTE-COUNT, 64000)
        MOVE 1 TO TIMEOUT-MS
        CALL "Socket-Read" USING HNDL ERRNO BYTE-COUNT BUFFER TIMEOUT-MS
        PERFORM HandleClientError
        MOVE BUFFER(1:BYTE-COUNT) TO PACKET-BUFFER(PACKET-POSITION:BYTE-COUNT)
        ADD BYTE-COUNT TO PACKET-POSITION
        EXIT SECTION
    END-IF

    *> If we're at this point, everything is read and we can start processing the packet.

    *> Start decoding the packet by decoding the packet ID
    MOVE 1 TO PACKET-POSITION
    CALL "Decode-VarInt" USING PACKET-BUFFER PACKET-POSITION PACKET-ID

    DISPLAY "[state=" CLIENT-STATE "] Received packet: " PACKET-ID

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
            MOVE -1 TO CLIENT-STATE
    END-EVALUATE

    *> Reset packet position and length for the next packet
    MOVE -1 TO PACKET-LENGTH
    MOVE 1 TO PACKET-POSITION

    EXIT SECTION.

HandleHandshake SECTION.
    IF PACKET-ID NOT = 0 THEN
        DISPLAY "  Unexpected packet ID: " PACKET-ID
        MOVE -1 TO CLIENT-STATE
        EXIT SECTION
    END-IF

    *> The final byte of the payload encodes the target state.
    COMPUTE CLIENT-STATE = FUNCTION ORD(PACKET-BUFFER(PACKET-LENGTH:1)) - 1
    IF CLIENT-STATE NOT = 1 AND CLIENT-STATE NOT = 2 THEN
        DISPLAY "  Invalid target state: " CLIENT-STATE
        MOVE -1 TO CLIENT-STATE
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
            PERFORM HandleClientError
        WHEN PACKET-ID = 1
            *> Ping request: respond with the same payload and close the connection
            DISPLAY "  Responding to ping request"
            COMPUTE BYTE-COUNT = 8
            MOVE PACKET-BUFFER(PACKET-POSITION:BYTE-COUNT) TO BUFFER(1:BYTE-COUNT)
            MOVE 1 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleClientError
            MOVE -1 TO CLIENT-STATE
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
                PERFORM HandleClientError
                MOVE -1 TO CLIENT-STATE
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
            PERFORM HandleClientError

        *> Login acknowledge
        WHEN PACKET-ID = 3
            *> Must not happen before login start
            IF USERNAME-LENGTH = 0 THEN
                DISPLAY "  Unexpected login acknowledge"
                MOVE -1 TO CLIENT-STATE
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
                        CALL "Socket-Write" USING BY REFERENCE HNDL ERRNO BYTE-COUNT BUFFER
                        PERFORM HandleClientError
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
            PERFORM HandleClientError

            *> Send finish configuration
            MOVE 2 TO PACKET-ID
            MOVE 0 TO BYTE-COUNT
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleClientError

            *> We now expect an acknowledge packet
            MOVE 1 TO CONFIG-FINISH

        *> Acknowledge finish configuration
        WHEN PACKET-ID = 2
            IF CONFIG-FINISH = 0 THEN
                DISPLAY "  Unexpected acknowledge finish configuration"
                MOVE -1 TO CLIENT-STATE
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
            MOVE 10 TO TEMP-INT32
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
            PERFORM HandleClientError

            *> send game event "start waiting for level chunks"
            MOVE "06200d00000000" TO BUFFER
            MOVE 14 TO BYTE-COUNT
            CALL "DecodeHexString" USING BUFFER BYTE-COUNT TEMP-BUFFER TEMP-BYTE-COUNT
            CALL "Socket-Write" USING BY REFERENCE HNDL ERRNO TEMP-BYTE-COUNT TEMP-BUFFER
            PERFORM HandleClientError

            *> set ticking state
            MOVE "066e41a0000000" TO BUFFER
            MOVE 14 TO BYTE-COUNT
            CALL "DecodeHexString" USING BUFFER BYTE-COUNT TEMP-BUFFER TEMP-BYTE-COUNT
            CALL "Socket-Write" USING BY REFERENCE HNDL ERRNO TEMP-BYTE-COUNT TEMP-BUFFER
            PERFORM HandleClientError

            *> tick
            MOVE "026f00" TO BUFFER
            MOVE 6 TO BYTE-COUNT
            CALL "DecodeHexString" USING BUFFER BYTE-COUNT TEMP-BUFFER TEMP-BYTE-COUNT
            CALL "Socket-Write" USING BY REFERENCE HNDL ERRNO TEMP-BYTE-COUNT TEMP-BUFFER
            PERFORM HandleClientError

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
            PERFORM HandleClientError

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
            PERFORM HandleClientError

            *> send chunk data ("Chunk Data and Update Light") for all chunks
            *> TODO: only send chunks around the player
            COMPUTE TEMP-INT32 = WORLD-CHUNKS-COUNT-X * WORLD-CHUNKS-COUNT-Z
            PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > TEMP-INT32
                CALL "SendPacket-ChunkData" USING HNDL ERRNO WORLD-CHUNK(CHUNK-INDEX)
                PERFORM HandleClientError
            END-PERFORM

            *> send position ("Synchronize Player Position")
            MOVE 0 TO BYTE-COUNT
            *> X
            CALL "Encode-Double" USING PLAYER-X TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> Y
            CALL "Encode-Double" USING PLAYER-Y TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> Z
            CALL "Encode-Double" USING PLAYER-Z TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> yaw
            CALL "Encode-Float" USING PLAYER-YAW TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> pitch
            CALL "Encode-Float" USING PLAYER-PITCH TEMP-BUFFER TEMP-BYTE-COUNT
            MOVE TEMP-BUFFER TO BUFFER(BYTE-COUNT + 1:TEMP-BYTE-COUNT)
            ADD TEMP-BYTE-COUNT TO BYTE-COUNT
            *> flags=0
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> teleport ID=0
            ADD 1 TO BYTE-COUNT
            MOVE FUNCTION CHAR(1) TO BUFFER(BYTE-COUNT:1)
            *> send packet
            MOVE 62 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE HNDL PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleClientError

            *> TODO: receive "Confirm Teleportation"

        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandlePlay SECTION.
    *> TODO: implement packets

    EVALUATE TRUE
        *> KeepAlive response
        WHEN PACKET-ID = 21
            CALL "Decode-Long" USING PACKET-BUFFER PACKET-POSITION KEEPALIVE-RECV
        *> Set player position
        WHEN PACKET-ID = 23
            CALL "Decode-Double" USING PACKET-BUFFER PACKET-POSITION PLAYER-X
            CALL "Decode-Double" USING PACKET-BUFFER PACKET-POSITION PLAYER-Y
            CALL "Decode-Double" USING PACKET-BUFFER PACKET-POSITION PLAYER-Z
            *> TODO: "on ground" flag
        *> Set player position and rotation
        WHEN PACKET-ID = 24
            CALL "Decode-Double" USING PACKET-BUFFER PACKET-POSITION PLAYER-X
            CALL "Decode-Double" USING PACKET-BUFFER PACKET-POSITION PLAYER-Y
            CALL "Decode-Double" USING PACKET-BUFFER PACKET-POSITION PLAYER-Z
            CALL "Decode-Float" USING PACKET-BUFFER PACKET-POSITION PLAYER-YAW
            CALL "Decode-Float" USING PACKET-BUFFER PACKET-POSITION PLAYER-PITCH
            *> TODO: "on ground" flag
        *> Set player rotation
        WHEN PACKET-ID = 25
            CALL "Decode-Float" USING PACKET-BUFFER PACKET-POSITION PLAYER-YAW
            CALL "Decode-Float" USING PACKET-BUFFER PACKET-POSITION PLAYER-PITCH
            *> TODO: "on ground" flag
        *> Set player on ground
        WHEN PACKET-ID = 26
            *> TODO
            CONTINUE
    END-EVALUATE

    EXIT SECTION.

HandleServerError SECTION.
    IF ERRNO NOT = 0 THEN
        DISPLAY "Server socket error: " ERRNO
        STOP RUN
    END-IF.

    EXIT SECTION.

HandleClientError SECTION.
    IF ERRNO NOT = 0 THEN
        DISPLAY "Client socket error: " ERRNO
        MOVE -1 TO CLIENT-STATE
    END-IF.

    EXIT SECTION.

END PROGRAM Server.
