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
    *> Socket variables (server socket handle, error number from last operation)
    01 LISTEN           PIC X(4).
    01 ERRNO            PIC 9(3)                VALUE 0.
    *> Connected clients. Note: For now, only one can be in the login/configuration/play state.
    01 CLIENTS OCCURS 10 TIMES.
        03 CLIENT-PRESENT   BINARY-CHAR             VALUE 0.
        03 CLIENT-HNDL      PIC X(4)                VALUE X"00000000".
        *> State of the player (0 = handshake, 1 = status, 2 = login, 3 = configuration, 4 = play, -1 = disconnect)
        03 CLIENT-STATE     BINARY-CHAR             VALUE -1.
        *> Last keepalive ID sent and received
        03 KEEPALIVE-SENT   BINARY-LONG-LONG        VALUE 0.
        03 KEEPALIVE-RECV   BINARY-LONG-LONG        VALUE 0.
        *> Packet reading: expected packet length (-1 if not yet known), packet buffer, amount of received bytes
        *> Note: Maximum packet length is 2^21-1 bytes - see: https://wiki.vg/Protocol#Packet_format
        03 PACKET-LENGTH    BINARY-LONG.
        03 PACKET-BUFFER    PIC X(2100000).
        03 PACKET-BUFFERLEN BINARY-LONG.
    *> The client handle of the connection that is currently being processed, and the index in the CLIENTS array
    01 TEMP-HNDL        PIC X(4).
    01 CLIENT-ID        BINARY-LONG UNSIGNED.
    *> Player data
    01 PLAYER-CLIENT    BINARY-LONG UNSIGNED    VALUE 0.
    01 USERNAME         PIC X(16).
    01 USERNAME-LENGTH  BINARY-LONG.
    01 CONFIG-FINISH    BINARY-CHAR             VALUE 0.
    01 PLAYER-POSITION.
        02 PLAYER-X         FLOAT-LONG              VALUE 0.
        02 PLAYER-Y         FLOAT-LONG              VALUE 64.
        02 PLAYER-Z         FLOAT-LONG              VALUE 0.
    01 PLAYER-ROTATION.
        02 PLAYER-YAW       FLOAT-SHORT             VALUE 0.
        02 PLAYER-PITCH     FLOAT-SHORT             VALUE 0.
    01 PLAYER-INVENTORY.
        02 PLAYER-INVENTORY-SLOT OCCURS 46 TIMES.
            *> If no item is present, the count is 0 and the ID is -1
            03 PLAYER-INVENTORY-SLOT-ID         BINARY-LONG             VALUE 0.
            03 PLAYER-INVENTORY-SLOT-COUNT      BINARY-CHAR UNSIGNED    VALUE 0.
            03 PLAYER-INVENTORY-SLOT-NBT-LENGTH BINARY-SHORT UNSIGNED   VALUE 0.
            03 PLAYER-INVENTORY-SLOT-NBT-DATA   PIC X(1024).
    01 PLAYER-HOTBAR    BINARY-CHAR UNSIGNED    VALUE 0.
    *> Incoming/outgoing packet data
    01 PACKET-ID        BINARY-LONG.
    01 PACKET-POSITION  BINARY-LONG UNSIGNED.
    01 BUFFER           PIC X(64000).
    01 BYTE-COUNT       BINARY-LONG UNSIGNED.
    *> Temporary variables
    01 TEMP-BUFFER      PIC X(64000).
    01 TEMP-BYTE-COUNT  BINARY-LONG UNSIGNED.
    01 TEMP-INT8        BINARY-LONG.
    01 TEMP-INT16       BINARY-LONG.
    01 TEMP-INT32       BINARY-LONG.
    01 TEMP-INT64       BINARY-LONG-LONG.
    01 TEMP-DOUBLE      FLOAT-LONG.
    01 TEMP-POSITION.
        02 TEMP-POSITION-X  BINARY-LONG.
        02 TEMP-POSITION-Y  BINARY-LONG.
        02 TEMP-POSITION-Z  BINARY-LONG.
    *> Time measurement
    01 CURRENT-TIME     BINARY-LONG-LONG.
    01 TICK-ENDTIME     BINARY-LONG-LONG.
    01 TIMEOUT-MS       BINARY-SHORT UNSIGNED.
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

ServerLoop.
    MOVE SPACES TO USERNAME
    MOVE 0 TO USERNAME-LENGTH
    MOVE 0 TO CONFIG-FINISH

    *> Loop forever - each iteration is one game tick (1/20th of a second).
    PERFORM UNTIL EXIT
        CALL "Util-SystemTimeMillis" USING CURRENT-TIME
        COMPUTE TICK-ENDTIME = CURRENT-TIME + (1000 / 20)

        *> Update the game state
        PERFORM GameLoop

        *> Handle keep-alive and disconnections for connected clients
        PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > 10
            IF CLIENT-PRESENT(CLIENT-ID) = 1
                PERFORM KeepAlive
            END-IF
        END-PERFORM

        *> The remaining time of this tick can be used for accepting connections and receiving packets.
        PERFORM UNTIL CURRENT-TIME >= TICK-ENDTIME
            PERFORM NetworkRead
            CALL "Util-SystemTimeMillis" USING CURRENT-TIME
        END-PERFORM

        MOVE X"00000000" TO TEMP-HNDL
        MOVE 0 TO CLIENT-ID
    END-PERFORM
    .

GameLoop SECTION.
    *> For now, nothing to do here.
    EXIT SECTION.

NetworkRead SECTION.
    MOVE 1 TO TIMEOUT-MS
    CALL "Socket-Poll" USING LISTEN ERRNO TEMP-HNDL TIMEOUT-MS
    IF ERRNO = 5
        *> Timeout, nothing to do
        EXIT SECTION
    END-IF
    PERFORM HandleServerError

    *> Find an existing client to which the handle belongs
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > 10
        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-HNDL(CLIENT-ID) = TEMP-HNDL
            PERFORM ReceivePacket
            EXIT SECTION
        END-IF
    END-PERFORM

    *> If no existing client was found, find a free slot for a new client
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > 10
        IF CLIENT-PRESENT(CLIENT-ID) = 0
            PERFORM InsertClient
            PERFORM ReceivePacket
            EXIT SECTION
        END-IF
    END-PERFORM

    *> If no free slot was found, close the connection
    DISPLAY "No free slot for new client"
    CALL "Socket-Close" USING TEMP-HNDL ERRNO

    EXIT SECTION.

InsertClient SECTION.
    DISPLAY "New client connected: " CLIENT-ID

    MOVE 1 TO CLIENT-PRESENT(CLIENT-ID)
    MOVE TEMP-HNDL TO CLIENT-HNDL(CLIENT-ID)
    MOVE 0 TO CLIENT-STATE(CLIENT-ID)

    MOVE 0 TO KEEPALIVE-SENT(CLIENT-ID)
    MOVE 0 TO KEEPALIVE-RECV(CLIENT-ID)

    MOVE -1 TO PACKET-LENGTH(CLIENT-ID)
    MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)

    EXIT SECTION.

RemoveClient SECTION.
    DISPLAY "Client " CLIENT-ID " disconnected"

    CALL "Socket-Close" USING CLIENT-HNDL(CLIENT-ID) ERRNO
    PERFORM HandleServerError

    MOVE 0 TO CLIENT-PRESENT(CLIENT-ID)
    MOVE X"00000000" TO CLIENT-HNDL(CLIENT-ID)
    MOVE -1 TO CLIENT-STATE(CLIENT-ID)

    IF PLAYER-CLIENT = CLIENT-ID
        MOVE 0 TO PLAYER-CLIENT
    END-IF

    EXIT SECTION.

KeepAlive SECTION.
    *> Give the client some time for keepalive when the connection is established
    IF KEEPALIVE-RECV(CLIENT-ID) = 0
        MOVE CURRENT-TIME TO KEEPALIVE-RECV(CLIENT-ID)
    END-IF

    *> If the client has not responded to keepalive within 15 seconds, disconnect
    COMPUTE TEMP-INT64 = CURRENT-TIME - KEEPALIVE-RECV(CLIENT-ID)
    IF TEMP-INT64 >= 15000
        DISPLAY "Client " CLIENT-ID " timed out"
        MOVE -1 TO CLIENT-STATE(CLIENT-ID)
    END-IF

    *> Send keepalive packet every second, but only in play state
    COMPUTE TEMP-INT64 = CURRENT-TIME - KEEPALIVE-SENT(CLIENT-ID)
    IF CLIENT-STATE(CLIENT-ID) = 4 AND TEMP-INT64 >= 1000
        MOVE CURRENT-TIME TO KEEPALIVE-SENT(CLIENT-ID)
        CALL "SendPacket-KeepAlive" USING CLIENT-HNDL(CLIENT-ID) ERRNO KEEPALIVE-SENT(CLIENT-ID)
        PERFORM HandleClientError
    END-IF

    *> If the client should be disconnected, do so
    IF CLIENT-STATE(CLIENT-ID) < 0
        PERFORM RemoveClient
    END-IF

    EXIT SECTION.

ReceivePacket SECTION.
    *> Ignore any attempts to receive data for clients that are not in a valid state
    IF CLIENT-STATE(CLIENT-ID) < 0
        EXIT SECTION
    END-IF

    *> If the packet length is not yet known, try to read more bytes one by one until the VarInt is valid
    IF PACKET-LENGTH(CLIENT-ID) < 0 THEN
        MOVE 1 TO BYTE-COUNT
        MOVE 1 TO TIMEOUT-MS
        CALL "Socket-Read" USING CLIENT-HNDL(CLIENT-ID) ERRNO BYTE-COUNT BUFFER TIMEOUT-MS
        PERFORM HandleClientError

        *> Check if anything was read. If not, just try again later.
        IF BYTE-COUNT = 0 THEN
            EXIT SECTION
        END-IF

        ADD 1 TO PACKET-BUFFERLEN(CLIENT-ID)
        MOVE BUFFER(1:1) TO PACKET-BUFFER(CLIENT-ID)(PACKET-BUFFERLEN(CLIENT-ID):1)

        *> This is the last VarInt byte if the most significant bit is not set.
        *> Note: ORD(...) returns the ASCII code of the character + 1, meaning we need to check for <= 128.
        IF FUNCTION ORD(BUFFER(1:1)) <= 128 THEN
            MOVE 1 TO PACKET-POSITION
            CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PACKET-LENGTH(CLIENT-ID)
        END-IF

        *> Validate packet length - note that it must be at least 1 due to the packet ID
        IF PACKET-LENGTH(CLIENT-ID) < 1 OR PACKET-LENGTH(CLIENT-ID) > 2097151 THEN
            DISPLAY "Invalid packet length: " PACKET-LENGTH(CLIENT-ID)
            MOVE -1 TO CLIENT-STATE(CLIENT-ID)
            EXIT SECTION
        END-IF

        *> The expected packet data length is now known and can be read in later invocations.
        *> We don't read it now to avoid allotting too much time to a single client.
        MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)
        EXIT SECTION
    END-IF

    *> Read more bytes if necessary
    IF PACKET-BUFFERLEN(CLIENT-ID) < PACKET-LENGTH(CLIENT-ID) THEN
        COMPUTE BYTE-COUNT = PACKET-LENGTH(CLIENT-ID) - PACKET-BUFFERLEN(CLIENT-ID)
        COMPUTE BYTE-COUNT = FUNCTION MIN(BYTE-COUNT, 64000)
        MOVE 1 TO TIMEOUT-MS
        CALL "Socket-Read" USING CLIENT-HNDL(CLIENT-ID) ERRNO BYTE-COUNT BUFFER TIMEOUT-MS
        PERFORM HandleClientError
        MOVE BUFFER(1:BYTE-COUNT) TO PACKET-BUFFER(CLIENT-ID)(PACKET-BUFFERLEN(CLIENT-ID) + 1:BYTE-COUNT)
        ADD BYTE-COUNT TO PACKET-BUFFERLEN(CLIENT-ID)
    END-IF

    *> Check if we can start processing the packet now.
    IF PACKET-BUFFERLEN(CLIENT-ID) < PACKET-LENGTH(CLIENT-ID) THEN
        EXIT SECTION
    END-IF

    *> Start decoding the packet by decoding the packet ID
    MOVE 1 TO PACKET-POSITION
    CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PACKET-ID

    DISPLAY "[client=" CLIENT-ID " state=" CLIENT-STATE(CLIENT-ID) "] Received packet: " PACKET-ID

    EVALUATE CLIENT-STATE(CLIENT-ID)
        WHEN 0
            PERFORM HandleHandshake
        WHEN 1
            PERFORM HandleStatus
        WHEN 2
            PERFORM HandleLogin
        WHEN 3
            PERFORM HandleConfiguration
        WHEN 4
            PERFORM HandlePlay
        WHEN OTHER
            DISPLAY "  Invalid state: " CLIENT-STATE(CLIENT-ID)
            MOVE -1 TO CLIENT-STATE(CLIENT-ID)
    END-EVALUATE

    *> Reset length for the next packet
    MOVE -1 TO PACKET-LENGTH(CLIENT-ID)
    MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)

    EXIT SECTION.

HandleHandshake SECTION.
    IF PACKET-ID NOT = 0 THEN
        DISPLAY "  Unexpected packet ID: " PACKET-ID
        MOVE -1 TO CLIENT-STATE(CLIENT-ID)
        EXIT SECTION
    END-IF

    *> The final byte of the payload encodes the target state.
    COMPUTE CLIENT-STATE(CLIENT-ID) = FUNCTION ORD(PACKET-BUFFER(CLIENT-ID)(PACKET-LENGTH(CLIENT-ID):1)) - 1
    IF CLIENT-STATE(CLIENT-ID) NOT = 1 AND CLIENT-STATE(CLIENT-ID) NOT = 2 THEN
        DISPLAY "  Invalid target state: " CLIENT-STATE(CLIENT-ID)
        MOVE -1 TO CLIENT-STATE(CLIENT-ID)
    ELSE
        DISPLAY "  Target state: " CLIENT-STATE(CLIENT-ID)
    END-IF

    EXIT SECTION.

HandleStatus SECTION.
    EVALUATE PACKET-ID
        WHEN 0
            *> Status request
            DISPLAY "  Responding to status request"
            *> count the number of current players
            MOVE 0 TO TEMP-INT32
            PERFORM VARYING TEMP-INT16 FROM 1 BY 1 UNTIL TEMP-INT16 > 10
                IF CLIENT-PRESENT(TEMP-INT16) = 1 AND CLIENT-STATE(TEMP-INT16) >= 2
                    ADD 1 TO TEMP-INT32
                END-IF
            END-PERFORM
            CALL "SendPacket-Status" USING CLIENT-HNDL(CLIENT-ID) ERRNO MOTD TEMP-INT32
            PERFORM HandleClientError
        WHEN 1
            *> Ping request: respond with the same payload and close the connection
            DISPLAY "  Responding to ping request"
            COMPUTE BYTE-COUNT = 8
            MOVE PACKET-BUFFER(CLIENT-ID)(PACKET-POSITION:BYTE-COUNT) TO BUFFER(1:BYTE-COUNT)
            MOVE 1 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleClientError
            MOVE -1 TO CLIENT-STATE(CLIENT-ID)
        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandleLogin SECTION.
    EVALUATE PACKET-ID
        *> Login start
        WHEN 0
            *> Prevent multiple concurrent logins
            IF PLAYER-CLIENT > 0 THEN
                MOVE "Server is full" TO BUFFER
                MOVE 14 TO BYTE-COUNT
                CALL "SendPacket-LoginDisconnect" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) ERRNO BUFFER BYTE-COUNT
                PERFORM HandleClientError
                MOVE -1 TO CLIENT-STATE(CLIENT-ID)
                EXIT SECTION
            END-IF

            *> Decode username
            CALL "Decode-String" USING BY REFERENCE PACKET-BUFFER(CLIENT-ID) PACKET-POSITION USERNAME-LENGTH USERNAME
            DISPLAY "  Login with username: " USERNAME

            *> Skip the UUID (16 bytes)
            ADD 16 TO PACKET-POSITION

            IF WHITELIST-ENABLE > 0 AND USERNAME NOT = WHITELIST-PLAYER THEN
                DISPLAY "  Player not whitelisted: " USERNAME
                MOVE "Not whitelisted!" TO BUFFER
                MOVE 16 TO BYTE-COUNT
                CALL "SendPacket-LoginDisconnect" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) ERRNO BUFFER BYTE-COUNT
                PERFORM HandleClientError
                MOVE -1 TO CLIENT-STATE(CLIENT-ID)
                EXIT SECTION
            END-IF

            MOVE CLIENT-ID TO PLAYER-CLIENT

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
            CALL "SendPacket" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleClientError

        *> Login acknowledge
        WHEN 3
            *> Must not happen before login start
            IF USERNAME-LENGTH = 0 THEN
                DISPLAY "  Unexpected login acknowledge"
                MOVE -1 TO CLIENT-STATE(CLIENT-ID)
                EXIT SECTION
            END-IF

            *> Can move to configuration state
            DISPLAY "  Acknowledged login"
            ADD 1 TO CLIENT-STATE(CLIENT-ID)

        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandleConfiguration SECTION.
    EVALUATE PACKET-ID
        *> Client information
        WHEN 0
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
                        CALL "Socket-Write" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) ERRNO BYTE-COUNT BUFFER
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
            CALL "SendPacket" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleClientError

            *> Send finish configuration
            MOVE 2 TO PACKET-ID
            MOVE 0 TO BYTE-COUNT
            CALL "SendPacket" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleClientError

            *> We now expect an acknowledge packet
            MOVE 1 TO CONFIG-FINISH

        *> Acknowledge finish configuration
        WHEN 2
            IF CONFIG-FINISH = 0 THEN
                DISPLAY "  Unexpected acknowledge finish configuration"
                MOVE -1 TO CLIENT-STATE(CLIENT-ID)
                EXIT SECTION
            END-IF

            *> Can move to play state
            DISPLAY "  Acknowledged finish configuration"
            ADD 1 TO CLIENT-STATE(CLIENT-ID)

            *> send "Login (play)"
            CALL "SendPacket-LoginPlay" USING CLIENT-HNDL(CLIENT-ID) ERRNO
            PERFORM HandleClientError

            *> send game event "start waiting for level chunks"
            MOVE "06200d00000000" TO BUFFER
            MOVE 14 TO BYTE-COUNT
            CALL "DecodeHexString" USING BUFFER BYTE-COUNT TEMP-BUFFER TEMP-BYTE-COUNT
            CALL "Socket-Write" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-BYTE-COUNT TEMP-BUFFER
            PERFORM HandleClientError

            *> set ticking state
            MOVE "066e41a0000000" TO BUFFER
            MOVE 14 TO BYTE-COUNT
            CALL "DecodeHexString" USING BUFFER BYTE-COUNT TEMP-BUFFER TEMP-BYTE-COUNT
            CALL "Socket-Write" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-BYTE-COUNT TEMP-BUFFER
            PERFORM HandleClientError

            *> tick
            MOVE "026f00" TO BUFFER
            MOVE 6 TO BYTE-COUNT
            CALL "DecodeHexString" USING BUFFER BYTE-COUNT TEMP-BUFFER TEMP-BYTE-COUNT
            CALL "Socket-Write" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-BYTE-COUNT TEMP-BUFFER
            PERFORM HandleClientError

            *> send inventory
            CALL "SendPacket-SetContainerContent" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-INVENTORY
            PERFORM HandleClientError

            *> send selected hotbar slot
            MOVE FUNCTION CHAR(PLAYER-HOTBAR + 1) TO BUFFER(1:1)
            MOVE 1 TO BYTE-COUNT
            MOVE 81 TO PACKET-ID
            CALL "SendPacket" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) PACKET-ID BUFFER BYTE-COUNT ERRNO

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
            CALL "SendPacket" USING BY REFERENCE CLIENT-HNDL(CLIENT-ID) PACKET-ID BUFFER BYTE-COUNT ERRNO
            PERFORM HandleClientError

            *> send chunk data ("Chunk Data and Update Light") for all chunks
            *> TODO: only send chunks around the player
            COMPUTE TEMP-INT32 = WORLD-CHUNKS-COUNT-X * WORLD-CHUNKS-COUNT-Z
            PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > TEMP-INT32
                CALL "SendPacket-ChunkData" USING CLIENT-HNDL(CLIENT-ID) ERRNO WORLD-CHUNK(CHUNK-INDEX)
                PERFORM HandleClientError
            END-PERFORM

            *> send position ("Synchronize Player Position")
            CALL "SendPacket-SetPlayerPosition" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-POSITION PLAYER-ROTATION
            PERFORM HandleClientError

            *> TODO: receive "Confirm Teleportation"

        WHEN OTHER
            DISPLAY "  Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandlePlay SECTION.
    EVALUATE PACKET-ID
        *> KeepAlive response
        WHEN 21
            CALL "Decode-Long" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION KEEPALIVE-RECV(CLIENT-ID)
        *> Set player position
        WHEN 23
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-X
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-Y
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-Z
            *> TODO: "on ground" flag
        *> Set player position and rotation
        WHEN 24
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-X
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-Y
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-Z
            CALL "Decode-Float" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-YAW
            CALL "Decode-Float" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-PITCH
            *> TODO: "on ground" flag
        *> Set player rotation
        WHEN 25
            CALL "Decode-Float" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-YAW
            CALL "Decode-Float" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-PITCH
            *> TODO: "on ground" flag
        *> Set player on ground
        WHEN 26
            *> TODO
            CONTINUE
        *> Player action
        WHEN 33
            *> Status (= the action), block position, face, sequence number.
            *> For now we only care about status and position.
            CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
            CALL "Decode-Position" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-POSITION
            EVALUATE TRUE
                *> started digging
                WHEN TEMP-INT32 = 0
                    DIVIDE TEMP-POSITION-X BY 16 GIVING CHUNK-X ROUNDED MODE IS TOWARD-LESSER
                    DIVIDE TEMP-POSITION-Z BY 16 GIVING CHUNK-Z ROUNDED MODE IS TOWARD-LESSER
                    COMPUTE CHUNK-INDEX = (CHUNK-Z + 3) * 7 + CHUNK-X + 3 + 1
                    COMPUTE TEMP-POSITION-X = FUNCTION MOD(TEMP-POSITION-X, 16)
                    COMPUTE TEMP-POSITION-Z = FUNCTION MOD(TEMP-POSITION-Z, 16)
                    COMPUTE TEMP-POSITION-Y = TEMP-POSITION-Y + 64
                    COMPUTE BLOCK-INDEX = (TEMP-POSITION-Y * 16 + TEMP-POSITION-Z) * 16 + TEMP-POSITION-X + 1
                    *> ensure the position is not outside the world
                    IF CHUNK-X >= -3 AND CHUNK-X <= 3 AND CHUNK-Z >= -3 AND CHUNK-Z <= 3 AND TEMP-POSITION-Y >= 0 AND TEMP-POSITION-Y < 384
                        MOVE 0 TO WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX)
                    END-IF
                    *> TODO: acknowledge the action
            END-EVALUATE
        *> Set held item
        WHEN 44
            CALL "Decode-Short" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT16
            IF TEMP-INT8 >= 0 AND TEMP-INT8 <= 8
                MOVE TEMP-INT16 TO PLAYER-HOTBAR
            END-IF
        *> Set creative mode slot
        WHEN 47
            *> slot ID
            CALL "Decode-Short" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT16
            *> TODO: spawn item entity when slot ID is -1
            *> slot description (present (boolean) [, item ID (VarInt), count (byte), NBT data])
            CALL "Decode-Byte" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT8
            IF TEMP-INT16 >= 0 AND TEMP-INT16 < 46
                IF TEMP-INT8 = 0
                    MOVE -1 TO PLAYER-INVENTORY-SLOT-ID(TEMP-INT16 + 1)
                    MOVE 0 TO PLAYER-INVENTORY-SLOT-COUNT(TEMP-INT16 + 1)
                ELSE
                    CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
                    MOVE TEMP-INT32 TO PLAYER-INVENTORY-SLOT-ID(TEMP-INT16 + 1)
                    CALL "Decode-Byte" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT8
                    MOVE TEMP-INT8 TO PLAYER-INVENTORY-SLOT-COUNT(TEMP-INT16 + 1)
                    *> remainder is NBT
                    COMPUTE BYTE-COUNT = PACKET-LENGTH(CLIENT-ID) - PACKET-POSITION + 1
                    IF BYTE-COUNT <= 1024
                        MOVE BYTE-COUNT TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(TEMP-INT16 + 1)
                        MOVE PACKET-BUFFER(CLIENT-ID)(PACKET-POSITION:BYTE-COUNT) TO PLAYER-INVENTORY-SLOT-NBT-DATA(TEMP-INT16 + 1)(1:BYTE-COUNT)
                    ELSE
                        MOVE 0 TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(TEMP-INT16 + 1)
                        DISPLAY "  Item NBT data too long: " BYTE-COUNT
                    END-IF
                END-IF
            END-IF
        *> Swing arm
        WHEN 51
            *> TODO
            CONTINUE
        *> Use item on block
        WHEN 53
            *> hand enum: 0=main hand, 1=off hand
            CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
            IF TEMP-INT32 = 0
                *> compute the inventory slot
                COMPUTE TEMP-INT8 = 36 + PLAYER-HOTBAR
            ELSE
                MOVE 45 TO TEMP-INT8
            END-IF
            *> block position
            CALL "Decode-Position" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-POSITION
            *>  face enum (0-5): -Y, +Y, -Z, +Z, -X, +X
            CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
            *> TODO: cursor position, inside block, sequence
            *> compute the location of the block to be affected
            EVALUATE TEMP-INT32
                WHEN 0
                    COMPUTE TEMP-POSITION-Y = TEMP-POSITION-Y - 1
                WHEN 1
                    COMPUTE TEMP-POSITION-Y = TEMP-POSITION-Y + 1
                WHEN 2
                    COMPUTE TEMP-POSITION-Z = TEMP-POSITION-Z - 1
                WHEN 3
                    COMPUTE TEMP-POSITION-Z = TEMP-POSITION-Z + 1
                WHEN 4
                    COMPUTE TEMP-POSITION-X = TEMP-POSITION-X - 1
                WHEN 5
                    COMPUTE TEMP-POSITION-X = TEMP-POSITION-X + 1
            END-EVALUATE
            *> find the chunk and block index
            DIVIDE TEMP-POSITION-X BY 16 GIVING CHUNK-X ROUNDED MODE IS TOWARD-LESSER
            DIVIDE TEMP-POSITION-Z BY 16 GIVING CHUNK-Z ROUNDED MODE IS TOWARD-LESSER
            COMPUTE CHUNK-INDEX = (CHUNK-Z + 3) * 7 + CHUNK-X + 3 + 1
            COMPUTE TEMP-POSITION-X = FUNCTION MOD(TEMP-POSITION-X, 16)
            COMPUTE TEMP-POSITION-Z = FUNCTION MOD(TEMP-POSITION-Z, 16)
            COMPUTE TEMP-POSITION-Y = TEMP-POSITION-Y + 64
            COMPUTE BLOCK-INDEX = (TEMP-POSITION-Y * 16 + TEMP-POSITION-Z) * 16 + TEMP-POSITION-X + 1
            *> ensure the position is not outside the world
            IF CHUNK-X >= -3 AND CHUNK-X <= 3 AND CHUNK-Z >= -3 AND CHUNK-Z <= 3 AND TEMP-POSITION-Y >= 0 AND TEMP-POSITION-Y < 384
                *> determine the block to place
                *> TODO: support more than stone and grass ;)
                IF PLAYER-INVENTORY-SLOT-ID(TEMP-INT8 + 1) = 1
                    MOVE 1 TO WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX)
                ELSE IF PLAYER-INVENTORY-SLOT-ID(TEMP-INT8 + 1) = 27
                    MOVE 9 TO WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX)
                END-IF
            END-IF
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
        DISPLAY "Client " CLIENT-ID " socket error: " ERRNO
        MOVE -1 TO CLIENT-STATE(CLIENT-ID)
    END-IF.

    EXIT SECTION.

END PROGRAM Server.
