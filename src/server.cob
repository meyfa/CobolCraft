IDENTIFICATION DIVISION.
PROGRAM-ID. Server.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT FD-REGISTRIES-FILE ASSIGN TO "data/generated/reports/registries.json"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT FD-BLOCKS-FILE ASSIGN TO "data/generated/reports/blocks.json"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
    FD FD-REGISTRIES-FILE.
        01 REGISTRIES-LINE PIC X(1024).
    FD FD-BLOCKS-FILE.
        01 BLOCKS-LINE PIC X(1024).

WORKING-STORAGE SECTION.
    *> Constants
    01 C-MINECRAFT-ITEM             PIC X(50) VALUE "minecraft:item".
    01 C-MINECRAFT-STONE            PIC X(50) VALUE "minecraft:stone".
    01 C-MINECRAFT-GRASS_BLOCK      PIC X(50) VALUE "minecraft:grass_block".
    *> A large buffer to hold JSON data before parsing.
    01 DATA-BUFFER          PIC X(10000000).
    01 DATA-BUFFER-LEN      BINARY-LONG UNSIGNED    VALUE 0.
    *> Console input
    01 CONSOLE-INPUT        PIC X(255).
    *> Socket variables (server socket handle, error number from last operation)
    01 LISTEN               PIC X(4).
    01 ERRNO                PIC 9(3)                VALUE 0.
    *> Connected clients
    01 MAX-CLIENTS          BINARY-LONG UNSIGNED    VALUE 10.
    01 CLIENTS OCCURS 10 TIMES.
        03 CLIENT-PRESENT       BINARY-CHAR             VALUE 0.
        03 CLIENT-HNDL          PIC X(4)                VALUE X"00000000".
        *> State of the player (0 = handshake, 1 = status, 2 = login, 3 = configuration, 4 = play, -1 = disconnected)
        03 CLIENT-STATE         BINARY-CHAR             VALUE -1.
        03 CONFIG-FINISH        BINARY-CHAR             VALUE 0.
        *> The index of the associated player, or 0 if login has not been started
        03 CLIENT-PLAYER        BINARY-CHAR             VALUE 0.
        *> Last keepalive ID sent and received
        03 KEEPALIVE-SENT       BINARY-LONG-LONG        VALUE 0.
        03 KEEPALIVE-RECV       BINARY-LONG-LONG        VALUE 0.
        *> Last teleport ID sent and received. Until the client acknowledges the teleport, any movement packets it sends
        *> are ignored.
        03 TELEPORT-SENT        BINARY-LONG-LONG        VALUE 0.
        03 TELEPORT-RECV        BINARY-LONG-LONG        VALUE 0.
        *> Packet reading: expected packet length (-1 if not yet known), packet buffer, amount of received bytes
        *> Note: Maximum packet length is 2^21-1 bytes - see: https://wiki.vg/Protocol#Packet_format
        03 PACKET-LENGTH        BINARY-LONG.
        03 PACKET-BUFFER        PIC X(2100000).
        03 PACKET-BUFFERLEN     BINARY-LONG.
    *> The client handle of the connection that is currently being processed, and the index in the CLIENTS array
    01 TEMP-HNDL            PIC X(4).
    01 CLIENT-ID            BINARY-LONG UNSIGNED.
    *> Player data. Once a new player is connected, their data is stored here. When they disconnect, the client is
    *> set to 0, but the player data remains to be reclaimed if the same player connects again.
    *> TODO: add some way of offloading player data to disk
    01 MAX-PLAYERS          BINARY-LONG UNSIGNED    VALUE 10.
    01 PLAYERS OCCURS 10 TIMES.
        02 PLAYER-CLIENT        BINARY-LONG UNSIGNED    VALUE 0.
        02 PLAYER-UUID          PIC X(16).
        02 USERNAME             PIC X(16).
        02 USERNAME-LENGTH      BINARY-LONG.
        02 PLAYER-POSITION.
            03 PLAYER-X             FLOAT-LONG              VALUE 0.
            03 PLAYER-Y             FLOAT-LONG              VALUE 64.
            03 PLAYER-Z             FLOAT-LONG              VALUE 0.
        02 PLAYER-ROTATION.
            03 PLAYER-YAW           FLOAT-SHORT             VALUE 0.
            03 PLAYER-PITCH         FLOAT-SHORT             VALUE 0.
        02 PLAYER-INVENTORY.
            03 PLAYER-INVENTORY-SLOT OCCURS 46 TIMES.
                *> If no item is present, the count is 0 and the ID is -1
                04 PLAYER-INVENTORY-SLOT-ID         BINARY-LONG             VALUE 0.
                04 PLAYER-INVENTORY-SLOT-COUNT      BINARY-CHAR UNSIGNED    VALUE 0.
                04 PLAYER-INVENTORY-SLOT-NBT-LENGTH BINARY-SHORT UNSIGNED   VALUE 0.
                04 PLAYER-INVENTORY-SLOT-NBT-DATA   PIC X(1024).
        02 PLAYER-HOTBAR    BINARY-CHAR UNSIGNED    VALUE 0.
    *> Incoming/outgoing packet data
    01 PACKET-ID            BINARY-LONG.
    01 PACKET-POSITION      BINARY-LONG UNSIGNED.
    01 BUFFER               PIC X(64000).
    01 BYTE-COUNT           BINARY-LONG UNSIGNED.
    *> Temporary variables
    01 TEMP-INT8            BINARY-CHAR.
    01 TEMP-INT16           BINARY-SHORT.
    01 TEMP-INT32           BINARY-LONG.
    01 TEMP-INT64           BINARY-LONG-LONG.
    01 TEMP-FLOAT           FLOAT-SHORT.
    01 TEMP-UUID            PIC X(16).
    01 TEMP-POSITION.
        02 TEMP-POSITION-X      BINARY-LONG.
        02 TEMP-POSITION-Y      BINARY-LONG.
        02 TEMP-POSITION-Z      BINARY-LONG.
    01 TEMP-USERNAME        PIC X(16).
    01 TEMP-USERNAME-LEN    BINARY-LONG.
    01 TEMP-REGISTRY        PIC X(100)              VALUE SPACES.
    *> Time measurement
    01 CURRENT-TIME         BINARY-LONG-LONG.
    01 TICK-ENDTIME         BINARY-LONG-LONG.
    01 TIMEOUT-MS           BINARY-SHORT UNSIGNED.
    *> Variables for working with chunks
    01 CHUNK-X              BINARY-LONG.
    01 CHUNK-Z              BINARY-LONG.
    01 CHUNK-INDEX          BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX          BINARY-LONG UNSIGNED.
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
                    05 WORLD-BLOCK-ID BINARY-LONG UNSIGNED VALUE 0.
    *> Age of the world in ticks. This modulo 24000 is the current time of day.
    01 WORLD-AGE            BINARY-LONG-LONG.

LINKAGE SECTION.
    *> Configuration provided by main program
    01 SERVER-CONFIG.
        02 PORT                 PIC X(5).
        02 WHITELIST-ENABLE     BINARY-CHAR.
        02 WHITELIST-PLAYER     PIC X(16).
        02 MOTD                 PIC X(64).

PROCEDURE DIVISION USING SERVER-CONFIG.
LoadRegistries.
    DISPLAY "Loading registries"
    *> read the entire registries.json file into memory
    MOVE 0 TO DATA-BUFFER-LEN
    OPEN INPUT FD-REGISTRIES-FILE
    MOVE 1024 TO BYTE-COUNT
    PERFORM UNTIL BYTE-COUNT = 0
        READ FD-REGISTRIES-FILE
            AT END
                MOVE 0 TO BYTE-COUNT
            NOT AT END
                IF DATA-BUFFER-LEN > 0
                    MOVE " " TO DATA-BUFFER(DATA-BUFFER-LEN + 1:1)
                    ADD 1 TO DATA-BUFFER-LEN
                END-IF
                MOVE FUNCTION STORED-CHAR-LENGTH(REGISTRIES-LINE) TO TEMP-INT32
                MOVE REGISTRIES-LINE(1:TEMP-INT32) TO DATA-BUFFER(DATA-BUFFER-LEN + 1:TEMP-INT32)
                ADD TEMP-INT32 TO DATA-BUFFER-LEN
        END-READ
    END-PERFORM
    CLOSE FD-REGISTRIES-FILE
    *> parse the JSON data
    CALL "Registries-Parse" USING DATA-BUFFER DATA-BUFFER-LEN TEMP-INT8
    IF TEMP-INT8 NOT = 0
        DISPLAY "Failed to parse registries"
        STOP RUN
    END-IF
    .

LoadBlocks.
    DISPLAY "Loading blocks"
    *> read the entire blocks.json file into memory
    MOVE 0 TO DATA-BUFFER-LEN
    OPEN INPUT FD-BLOCKS-FILE
    MOVE 1024 TO BYTE-COUNT
    PERFORM UNTIL BYTE-COUNT = 0
        READ FD-BLOCKS-FILE
            AT END
                MOVE 0 TO BYTE-COUNT
            NOT AT END
                IF DATA-BUFFER-LEN > 0
                    MOVE " " TO DATA-BUFFER(DATA-BUFFER-LEN + 1:1)
                    ADD 1 TO DATA-BUFFER-LEN
                END-IF
                MOVE FUNCTION STORED-CHAR-LENGTH(BLOCKS-LINE) TO TEMP-INT32
                MOVE BLOCKS-LINE(1:TEMP-INT32) TO DATA-BUFFER(DATA-BUFFER-LEN + 1:TEMP-INT32)
                ADD TEMP-INT32 TO DATA-BUFFER-LEN
        END-READ
    END-PERFORM
    CLOSE FD-BLOCKS-FILE
    *> parse the JSON data
    CALL "Blocks-Parse" USING DATA-BUFFER DATA-BUFFER-LEN TEMP-INT8
    IF TEMP-INT8 NOT = 0
        DISPLAY "Failed to parse blocks"
        STOP RUN
    END-IF
    .

GenerateWorld.
    DISPLAY "Generating world"
    PERFORM VARYING CHUNK-Z FROM -3 BY 1 UNTIL CHUNK-Z > 3
        PERFORM VARYING CHUNK-X FROM -3 BY 1 UNTIL CHUNK-X > 3
            COMPUTE CHUNK-INDEX = (CHUNK-Z + 3) * 7 + CHUNK-X + 3 + 1
            MOVE CHUNK-X TO WORLD-CHUNK-X(CHUNK-INDEX)
            MOVE CHUNK-Z TO WORLD-CHUNK-Z(CHUNK-INDEX)

            *> turn all blocks with Y < 63 (i.e., the bottom 128 blocks) into stone
            CALL "Blocks-Get-DefaultStateId" USING C-MINECRAFT-STONE TEMP-INT32
            PERFORM VARYING TEMP-POSITION-Y FROM 0 BY 1 UNTIL TEMP-POSITION-Y >= 128
                PERFORM VARYING TEMP-POSITION-Z FROM 0 BY 1 UNTIL TEMP-POSITION-Z >= 16
                    PERFORM VARYING TEMP-POSITION-X FROM 0 BY 1 UNTIL TEMP-POSITION-X >= 16
                        COMPUTE BLOCK-INDEX = (TEMP-POSITION-Y * 16 + TEMP-POSITION-Z) * 16 + TEMP-POSITION-X + 1
                        MOVE TEMP-INT32 TO WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX)
                    END-PERFORM
                END-PERFORM
            END-PERFORM

            *> turn all blocks with Y = 63 (i.e., the top 16 blocks) into grass
            CALL "Blocks-Get-DefaultStateId" USING C-MINECRAFT-GRASS_BLOCK TEMP-INT32
            MOVE 127 TO TEMP-POSITION-Y
            PERFORM VARYING TEMP-POSITION-Z FROM 0 BY 1 UNTIL TEMP-POSITION-Z >= 16
                PERFORM VARYING TEMP-POSITION-X FROM 0 BY 1 UNTIL TEMP-POSITION-X >= 16
                    COMPUTE BLOCK-INDEX = (TEMP-POSITION-Y * 16 + TEMP-POSITION-Z) * 16 + TEMP-POSITION-X + 1
                    MOVE TEMP-INT32 TO WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX)
                END-PERFORM
            END-PERFORM
        END-PERFORM
    END-PERFORM.

StartServer.
    DISPLAY "Starting server"
    CALL "Util-IgnoreSIGPIPE"
    CALL "Socket-Listen" USING PORT LISTEN ERRNO
    PERFORM HandleServerError
    DISPLAY "Done! For help, type ""help"""
    .

ServerLoop.
    CALL "Util-SetConsoleNonBlocking"

    *> Loop forever - each iteration is one game tick (1/20th of a second).
    PERFORM UNTIL EXIT
        CALL "Util-SystemTimeMillis" USING CURRENT-TIME
        COMPUTE TICK-ENDTIME = CURRENT-TIME + (1000 / 20)

        *> Update the game state
        PERFORM GameLoop

        *> Handle keep-alive and disconnections for connected clients
        PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
            IF CLIENT-PRESENT(CLIENT-ID) = 1
                PERFORM KeepAlive
            END-IF
        END-PERFORM

        *> Send world time every second
        IF FUNCTION MOD(WORLD-AGE, 20) = 0
            COMPUTE TEMP-INT64 = FUNCTION MOD(WORLD-AGE, 24000)
            PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = 4
                    CALL "SendPacket-UpdateTime" USING CLIENT-HNDL(CLIENT-ID) ERRNO WORLD-AGE TEMP-INT64
                    PERFORM HandleClientError
                END-IF
            END-PERFORM
        END-IF

        *> broadcast player positions to all clients in play state, as well as their equipment
        *> TODO: only send this if the player has moved/rotated, or if the equipment has changed
        *> TODO: use more efficient packet types when possible
        PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
            IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = 4
                *> Note: Since sending the packet can fail, we need to stop the loop if the client is disconnected.
                PERFORM VARYING TEMP-INT16 FROM 1 BY 1 UNTIL TEMP-INT16 > MAX-CLIENTS OR CLIENT-PRESENT(CLIENT-ID) = 0
                    IF CLIENT-PRESENT(TEMP-INT16) = 1 AND CLIENT-STATE(TEMP-INT16) = 4 AND TEMP-INT16 NOT = CLIENT-ID
                        MOVE CLIENT-PLAYER(TEMP-INT16) TO TEMP-INT32
                        CALL "SendPacket-TeleportEntity" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32 PLAYER-POSITION(TEMP-INT32) PLAYER-ROTATION(TEMP-INT32)
                        IF ERRNO = 0
                            CALL "SendPacket-SetHeadRotation" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32 PLAYER-YAW(TEMP-INT32)
                        END-IF
                        IF ERRNO = 0
                            COMPUTE TEMP-INT8 = 36 + PLAYER-HOTBAR(TEMP-INT32) + 1
                            CALL "SendPacket-SetEquipment" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32 PLAYER-INVENTORY-SLOT(TEMP-INT32, TEMP-INT8)
                        END-IF
                        PERFORM HandleClientError
                    END-IF
                END-PERFORM
            END-IF
        END-PERFORM

        *> Read console command
        PERFORM ConsoleInput

        *> The remaining time of this tick can be used for accepting connections and receiving packets.
        PERFORM UNTIL CURRENT-TIME >= TICK-ENDTIME
            PERFORM NetworkRead
            CALL "Util-SystemTimeMillis" USING CURRENT-TIME
        END-PERFORM

        MOVE X"00000000" TO TEMP-HNDL
        MOVE 0 TO CLIENT-ID
    END-PERFORM
    .

StopServer.
    DISPLAY "Stopping server"
    CALL "Socket-Close" USING LISTEN ERRNO
    PERFORM HandleServerError

    STOP RUN.

GameLoop SECTION.
    *> Update the world age
    ADD 1 TO WORLD-AGE

    EXIT SECTION.

ConsoleInput SECTION.
    *> Read from the console (configured as non-blocking). Note that this will only return full lines.
    MOVE LENGTH OF CONSOLE-INPUT TO BYTE-COUNT
    CALL "Util-ReadConsole" USING CONSOLE-INPUT BYTE-COUNT
    IF BYTE-COUNT = 0
        EXIT SECTION
    END-IF

    *> Ignore leading forward slash
    IF CONSOLE-INPUT(1:1) = "/"
        COMPUTE BYTE-COUNT = BYTE-COUNT - 1
        MOVE CONSOLE-INPUT(2:BYTE-COUNT) TO BUFFER(1:BYTE-COUNT)
        MOVE BUFFER(1:BYTE-COUNT) TO CONSOLE-INPUT
    END-IF

    *> Ignore trailing whitespace
    PERFORM VARYING TEMP-INT32 FROM BYTE-COUNT BY -1 UNTIL TEMP-INT32 = 0
        IF CONSOLE-INPUT(TEMP-INT32:1) NOT = " " AND CONSOLE-INPUT(TEMP-INT32:1) NOT = X"0A"
            EXIT PERFORM
        END-IF
    END-PERFORM
    MOVE TEMP-INT32 TO BYTE-COUNT

    *> Handle the command
    EVALUATE CONSOLE-INPUT(1:BYTE-COUNT)
        WHEN "help"
            DISPLAY "Available commands:"
            DISPLAY "  /help - show this help"
            DISPLAY "  /stop - stop the server"
        WHEN "stop"
            PERFORM StopServer
        WHEN OTHER
            DISPLAY "Unknown command: " CONSOLE-INPUT(1:BYTE-COUNT)
    END-EVALUATE

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
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-HNDL(CLIENT-ID) = TEMP-HNDL
            PERFORM ReceivePacket
            EXIT SECTION
        END-IF
    END-PERFORM

    *> If no existing client was found, find a free slot for a new client
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 0
            PERFORM InsertClient
            PERFORM ReceivePacket
            EXIT SECTION
        END-IF
    END-PERFORM

    *> If no free slot was found, close the connection
    DISPLAY "Cannot accept new connection: no free slots"
    CALL "Socket-Close" USING TEMP-HNDL ERRNO

    EXIT SECTION.

InsertClient SECTION.
    MOVE 1 TO CLIENT-PRESENT(CLIENT-ID)
    MOVE TEMP-HNDL TO CLIENT-HNDL(CLIENT-ID)
    MOVE 0 TO CLIENT-STATE(CLIENT-ID)
    MOVE 0 TO CLIENT-PLAYER(CLIENT-ID)

    MOVE 0 TO KEEPALIVE-SENT(CLIENT-ID)
    MOVE 0 TO KEEPALIVE-RECV(CLIENT-ID)

    MOVE 0 TO TELEPORT-SENT(CLIENT-ID)
    MOVE 0 TO TELEPORT-RECV(CLIENT-ID)

    MOVE -1 TO PACKET-LENGTH(CLIENT-ID)
    MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)

    EXIT SECTION.

DisconnectClient SECTION.
    *> Reset this early to avoid infinite loops in case of errors when sending packets to other clients
    MOVE 0 TO CLIENT-PRESENT(CLIENT-ID)

    CALL "Socket-Close" USING CLIENT-HNDL(CLIENT-ID) ERRNO
    PERFORM HandleServerError

    *> If the client was playing, send a leave message to all other clients, and remove the player from their world
    IF CLIENT-STATE(CLIENT-ID) = 4
        *> send "<username> left the game" to all clients in play state, except the current client
        MOVE 0 TO BYTE-COUNT
        MOVE USERNAME(CLIENT-PLAYER(CLIENT-ID)) TO BUFFER
        ADD USERNAME-LENGTH(CLIENT-PLAYER(CLIENT-ID)) TO BYTE-COUNT
        MOVE " left the game" TO BUFFER(BYTE-COUNT + 1:14)
        ADD 14 TO BYTE-COUNT
        DISPLAY BUFFER(1:BYTE-COUNT)
        PERFORM BroadcastMessageExceptCurrent

        *> remove the player from the player list, and despawn the player entity
        MOVE CLIENT-ID TO TEMP-INT16
        MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
        PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
            IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = 4 AND CLIENT-ID NOT = TEMP-INT16
                CALL "SendPacket-RemovePlayer" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-UUID(TEMP-INT32)
                IF ERRNO = 0
                    CALL "SendPacket-RemoveEntity" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32
                END-IF
                PERFORM HandleClientError
            END-IF
        END-PERFORM
        MOVE TEMP-INT16 TO CLIENT-ID
    END-IF

    MOVE X"00000000" TO CLIENT-HNDL(CLIENT-ID)
    MOVE -1 TO CLIENT-STATE(CLIENT-ID)
    MOVE 0 TO CONFIG-FINISH(CLIENT-ID)

    *> If there is an associated player, remove the association
    IF CLIENT-PLAYER(CLIENT-ID) > 0
        MOVE 0 TO PLAYER-CLIENT(CLIENT-PLAYER(CLIENT-ID))
        MOVE 0 TO CLIENT-PLAYER(CLIENT-ID)
    END-IF

    EXIT SECTION.

BroadcastMessageExceptCurrent SECTION.
    *> send BUFFER(1:BYTE-COUNT) to all clients in play state, except the current client
    MOVE CLIENT-ID TO TEMP-INT16
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-ID NOT = TEMP-INT16 AND CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = 4
            CALL "SendPacket-SystemChat" USING CLIENT-HNDL(CLIENT-ID) ERRNO BUFFER BYTE-COUNT
            PERFORM HandleClientError
        END-IF
    END-PERFORM
    MOVE TEMP-INT16 TO CLIENT-ID
    EXIT SECTION.

KeepAlive SECTION.
    *> Give the client some time for keepalive when the connection is established
    IF KEEPALIVE-RECV(CLIENT-ID) = 0
        MOVE CURRENT-TIME TO KEEPALIVE-RECV(CLIENT-ID)
    END-IF

    *> If the client has not responded to keepalive within 15 seconds, disconnect
    COMPUTE TEMP-INT64 = CURRENT-TIME - KEEPALIVE-RECV(CLIENT-ID)
    IF TEMP-INT64 >= 15000
        DISPLAY "[client=" CLIENT-ID "] Timeout"
        PERFORM DisconnectClient
        EXIT SECTION
    END-IF

    *> Send keepalive packet every second, but only in play state
    COMPUTE TEMP-INT64 = CURRENT-TIME - KEEPALIVE-SENT(CLIENT-ID)
    IF CLIENT-STATE(CLIENT-ID) = 4 AND TEMP-INT64 >= 1000
        MOVE CURRENT-TIME TO KEEPALIVE-SENT(CLIENT-ID)
        CALL "SendPacket-KeepAlive" USING CLIENT-HNDL(CLIENT-ID) ERRNO KEEPALIVE-SENT(CLIENT-ID)
        PERFORM HandleClientError
    END-IF

    EXIT SECTION.

ReceivePacket SECTION.
    *> Ignore any attempts to receive data for clients that are not in a valid state
    IF CLIENT-STATE(CLIENT-ID) < 0
        EXIT SECTION
    END-IF

    *> If the packet length is not yet known, try to read more bytes one by one until the VarInt is valid
    IF PACKET-LENGTH(CLIENT-ID) < 0
        MOVE 1 TO BYTE-COUNT
        MOVE 1 TO TIMEOUT-MS
        CALL "Socket-Read" USING CLIENT-HNDL(CLIENT-ID) ERRNO BYTE-COUNT BUFFER TIMEOUT-MS
        PERFORM HandleClientError

        *> If nothing was read, we can try again later (unless there was an error).
        IF BYTE-COUNT = 0 OR ERRNO NOT = 0
            EXIT SECTION
        END-IF

        ADD 1 TO PACKET-BUFFERLEN(CLIENT-ID)
        MOVE BUFFER(1:1) TO PACKET-BUFFER(CLIENT-ID)(PACKET-BUFFERLEN(CLIENT-ID):1)

        *> This is the last VarInt byte if the most significant bit is not set.
        *> Note: ORD(...) returns the ASCII code of the character + 1, meaning we need to check for <= 128.
        IF FUNCTION ORD(BUFFER(1:1)) <= 128
            MOVE 1 TO PACKET-POSITION
            CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PACKET-LENGTH(CLIENT-ID)
        END-IF

        *> Validate packet length - note that it must be at least 1 due to the packet ID
        IF PACKET-LENGTH(CLIENT-ID) < 1 OR PACKET-LENGTH(CLIENT-ID) > 2097151
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Received invalid packet length: " PACKET-LENGTH(CLIENT-ID)
            PERFORM DisconnectClient
            EXIT SECTION
        END-IF

        *> The expected packet data length is now known and can be read in later invocations.
        *> We don't read it now to avoid allotting too much time to a single client.
        MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)
        EXIT SECTION
    END-IF

    *> Read more bytes if necessary
    IF PACKET-BUFFERLEN(CLIENT-ID) < PACKET-LENGTH(CLIENT-ID)
        COMPUTE BYTE-COUNT = PACKET-LENGTH(CLIENT-ID) - PACKET-BUFFERLEN(CLIENT-ID)
        COMPUTE BYTE-COUNT = FUNCTION MIN(BYTE-COUNT, 64000)
        MOVE 1 TO TIMEOUT-MS
        CALL "Socket-Read" USING CLIENT-HNDL(CLIENT-ID) ERRNO BYTE-COUNT BUFFER TIMEOUT-MS
        IF ERRNO NOT = 0
            PERFORM HandleClientError
            EXIT SECTION
        END-IF
        MOVE BUFFER(1:BYTE-COUNT) TO PACKET-BUFFER(CLIENT-ID)(PACKET-BUFFERLEN(CLIENT-ID) + 1:BYTE-COUNT)
        ADD BYTE-COUNT TO PACKET-BUFFERLEN(CLIENT-ID)
    END-IF

    *> Check if we can start processing the packet now.
    IF PACKET-BUFFERLEN(CLIENT-ID) < PACKET-LENGTH(CLIENT-ID)
        EXIT SECTION
    END-IF

    *> Start decoding the packet by decoding the packet ID
    MOVE 1 TO PACKET-POSITION
    CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PACKET-ID

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
            DISPLAY "Invalid client state: " CLIENT-STATE(CLIENT-ID)
            PERFORM DisconnectClient
            EXIT SECTION
    END-EVALUATE

    *> Reset length for the next packet
    MOVE -1 TO PACKET-LENGTH(CLIENT-ID)
    MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)

    EXIT SECTION.

HandleHandshake SECTION.
    IF PACKET-ID NOT = H'00'
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet ID: " PACKET-ID
        PERFORM DisconnectClient
        EXIT SECTION
    END-IF

    *> The final byte of the payload encodes the target state.
    COMPUTE CLIENT-STATE(CLIENT-ID) = FUNCTION ORD(PACKET-BUFFER(CLIENT-ID)(PACKET-LENGTH(CLIENT-ID):1)) - 1
    IF CLIENT-STATE(CLIENT-ID) NOT = 1 AND CLIENT-STATE(CLIENT-ID) NOT = 2
        DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Client requested invalid target state: " CLIENT-STATE(CLIENT-ID)
        PERFORM DisconnectClient
        EXIT SECTION
    END-IF

    EXIT SECTION.

HandleStatus SECTION.
    EVALUATE PACKET-ID
        WHEN H'00'
            *> Status request
            *> count the number of current players
            MOVE 0 TO TEMP-INT32
            PERFORM VARYING TEMP-INT16 FROM 1 BY 1 UNTIL TEMP-INT16 > MAX-CLIENTS
                IF CLIENT-PRESENT(TEMP-INT16) = 1 AND CLIENT-PLAYER(TEMP-INT16) > 0
                    ADD 1 TO TEMP-INT32
                END-IF
            END-PERFORM
            CALL "SendPacket-Status" USING CLIENT-HNDL(CLIENT-ID) ERRNO MOTD MAX-PLAYERS TEMP-INT32
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF
        WHEN H'01'
            *> Ping request: respond with the same payload and close the connection
            CALL "Decode-Long" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT64
            CALL "SendPacket-PingResponse" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT64
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF
            PERFORM DisconnectClient
        WHEN OTHER
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandleLogin SECTION.
    EVALUATE PACKET-ID
        *> Login start
        WHEN H'00'
            *> Decode username and UUID
            CALL "Decode-String" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-USERNAME-LEN TEMP-USERNAME
            MOVE PACKET-BUFFER(CLIENT-ID)(PACKET-POSITION:16) TO TEMP-UUID
            ADD 16 TO PACKET-POSITION

            *> For testing, we want to allow the same UUID to connect multiple times with different usernames.
            *> Since this is an offline server, we can simply generate our own UUID to achieve this.
            *> For lack of a better implementation, we will simply use the bytes of the username as the UUID.
            MOVE X"00000000000000000000000000000000" TO TEMP-UUID
            MOVE TEMP-USERNAME(1:TEMP-USERNAME-LEN) TO TEMP-UUID(1:TEMP-USERNAME-LEN)

            *> Check username against the whitelist
            IF WHITELIST-ENABLE > 0 AND TEMP-USERNAME(1:TEMP-USERNAME-LEN) NOT = WHITELIST-PLAYER
                MOVE "You are not white-listed on this server!" TO BUFFER
                MOVE 40 TO BYTE-COUNT
                DISPLAY "Disconnecting " TEMP-USERNAME(1:TEMP-USERNAME-LEN) ": " BUFFER(1:BYTE-COUNT)
                CALL "SendPacket-LoginDisconnect" USING CLIENT-HNDL(CLIENT-ID) ERRNO BUFFER BYTE-COUNT
                IF ERRNO NOT = 0
                    PERFORM HandleClientError
                    EXIT SECTION
                END-IF
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF

            *> Try to find an existing player for the UUID, or find a free slot to add a new player.
            PERFORM VARYING TEMP-INT8 FROM 1 BY 1 UNTIL TEMP-INT8 > MAX-PLAYERS
                *> If the player is already connected, disconnect the existing client to replace it
                IF PLAYER-CLIENT(TEMP-INT8) > 0 AND PLAYER-UUID(TEMP-INT8) = TEMP-UUID
                    *> TODO: fix fragile save/restore of temporary variables
                    MOVE CLIENT-ID TO TEMP-INT64
                    MOVE PLAYER-CLIENT(TEMP-INT8) TO CLIENT-ID
                    MOVE "You logged in from another location" TO BUFFER
                    MOVE 35 TO BYTE-COUNT
                    DISPLAY "Disconnecting " USERNAME(TEMP-INT8)(1:USERNAME-LENGTH(TEMP-INT8)) ": " BUFFER(1:BYTE-COUNT)
                    *> TODO: Send the message to the existing client
                    PERFORM DisconnectClient
                    MOVE TEMP-INT64 TO CLIENT-ID
                END-IF
                *> Once we find an unused slot with either the same UUID or no player, we can stop searching.
                IF PLAYER-CLIENT(TEMP-INT8) = 0 AND (PLAYER-UUID(TEMP-INT8) = TEMP-UUID OR USERNAME-LENGTH(TEMP-INT8) = 0)
                    *> associate the player with the client
                    MOVE CLIENT-ID TO PLAYER-CLIENT(TEMP-INT8)
                    MOVE TEMP-INT8 TO CLIENT-PLAYER(CLIENT-ID)
                    *> store the username and UUID on the player
                    MOVE TEMP-USERNAME(1:TEMP-USERNAME-LEN) TO USERNAME(TEMP-INT8)
                    MOVE TEMP-USERNAME-LEN TO USERNAME-LENGTH(TEMP-INT8)
                    MOVE TEMP-UUID TO PLAYER-UUID(TEMP-INT8)
                    EXIT PERFORM
                END-IF
            END-PERFORM

            *> If no player slot was found, the server is full
            IF CLIENT-PLAYER(CLIENT-ID) = 0
                MOVE "The server is full" TO BUFFER
                MOVE 18 TO BYTE-COUNT
                DISPLAY "Disconnecting " TEMP-USERNAME(1:TEMP-USERNAME-LEN) ": " BUFFER(1:BYTE-COUNT)
                CALL "SendPacket-LoginDisconnect" USING CLIENT-HNDL(CLIENT-ID) ERRNO BUFFER BYTE-COUNT
                IF ERRNO NOT = 0
                    PERFORM HandleClientError
                    EXIT SECTION
                END-IF
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF

            *> Send login success. This should result in a "login acknowledged" packet by the client.
            CALL "SendPacket-LoginSuccess" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-UUID(CLIENT-PLAYER(CLIENT-ID)) USERNAME(CLIENT-PLAYER(CLIENT-ID)) USERNAME-LENGTH(CLIENT-PLAYER(CLIENT-ID))
            PERFORM HandleClientError

        *> Login acknowledge
        WHEN H'03'
            *> Must not happen before login start
            IF CLIENT-PLAYER(CLIENT-ID) = 0
                DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Client sent unexpected login acknowledge"
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF

            *> Can move to configuration state
            ADD 1 TO CLIENT-STATE(CLIENT-ID)

        WHEN OTHER
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet ID: " PACKET-ID
    END-EVALUATE

    EXIT SECTION.

HandleConfiguration SECTION.
    EVALUATE PACKET-ID
        *> Client information
        WHEN H'00'
            *> Note: payload of this packet is ignored for now

            *> Send registry data
            CALL "SendPacket-Registry" USING CLIENT-HNDL(CLIENT-ID) ERRNO
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> Send feature flags
            CALL "SendPacket-FeatureFlags" USING CLIENT-HNDL(CLIENT-ID) ERRNO
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> Send finish configuration
            CALL "SendPacket-FinishConfiguration" USING CLIENT-HNDL(CLIENT-ID) ERRNO
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> We now expect an acknowledge packet
            MOVE 1 TO CONFIG-FINISH(CLIENT-ID)

        *> Serverbound plugin message
        WHEN H'01'
            *> Not implemented
            CONTINUE

        *> Acknowledge finish configuration
        WHEN H'02'
            IF CONFIG-FINISH(CLIENT-ID) = 0
                DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Client sent unexpected acknowledge finish configuration"
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF

            *> Can move to play state
            ADD 1 TO CLIENT-STATE(CLIENT-ID)

            *> send "Login (play)" with player index as entity ID
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            CALL "SendPacket-LoginPlay" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> send world time
            COMPUTE TEMP-INT64 = FUNCTION MOD(WORLD-AGE, 24000)
            CALL "SendPacket-UpdateTime" USING CLIENT-HNDL(CLIENT-ID) ERRNO WORLD-AGE TEMP-INT64
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> send game event "start waiting for level chunks"
            MOVE 13 TO TEMP-INT8
            MOVE 0 TO TEMP-FLOAT
            CALL "SendPacket-GameEvent" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT8 TEMP-FLOAT
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> Note: The official server sends a lot of additional packets in this phase, but they seem to be optional.
            *> For example: set ticking state (rate=20, frozen=false); step tick (steps=0 [sic.])
            *> We will skip these for now.

            *> send inventory
            CALL "SendPacket-SetContainerContent" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-INVENTORY(CLIENT-PLAYER(CLIENT-ID))
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> send selected hotbar slot
            CALL "SendPacket-SetHeldItem" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-HOTBAR(CLIENT-PLAYER(CLIENT-ID))
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> send "Set Center Chunk"
            MOVE 0 TO CHUNK-X
            MOVE 0 TO CHUNK-Z
            CALL "SendPacket-SetCenterChunk" USING CLIENT-HNDL(CLIENT-ID) ERRNO CHUNK-X CHUNK-Z
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> send chunk data ("Chunk Data and Update Light") for all chunks
            *> TODO: only send chunks around the player
            COMPUTE TEMP-INT32 = WORLD-CHUNKS-COUNT-X * WORLD-CHUNKS-COUNT-Z
            PERFORM VARYING CHUNK-INDEX FROM 1 BY 1 UNTIL CHUNK-INDEX > TEMP-INT32
                CALL "SendPacket-ChunkData" USING CLIENT-HNDL(CLIENT-ID) ERRNO WORLD-CHUNK(CHUNK-INDEX)
                IF ERRNO NOT = 0
                    PERFORM HandleClientError
                    EXIT SECTION
                END-IF
            END-PERFORM

            *> send the player list (including the new player) to the new player, and spawn player entities
            PERFORM VARYING TEMP-INT16 FROM 1 BY 1 UNTIL TEMP-INT16 > MAX-CLIENTS
                IF CLIENT-PRESENT(TEMP-INT16) = 1 AND CLIENT-STATE(TEMP-INT16) = 4
                    MOVE CLIENT-PLAYER(TEMP-INT16) TO TEMP-INT32
                    CALL "SendPacket-AddPlayer" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-UUID(TEMP-INT32) USERNAME(TEMP-INT32) USERNAME-LENGTH(TEMP-INT32)
                    IF ERRNO = 0 AND TEMP-INT16 NOT = CLIENT-ID
                        CALL "SendPacket-SpawnEntity" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32 PLAYER-UUID(TEMP-INT32) PLAYER-POSITION(TEMP-INT32) PLAYER-ROTATION(TEMP-INT32)
                    END-IF
                    IF ERRNO NOT = 0
                        PERFORM HandleClientError
                        EXIT SECTION
                    END-IF
                END-IF
            END-PERFORM

            *> Send position ("Synchronize Player Position"). The client must confirm the teleportation.
            ADD 1 TO TELEPORT-SENT(CLIENT-ID)
            CALL "SendPacket-SetPlayerPosition" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-POSITION(CLIENT-PLAYER(CLIENT-ID)) PLAYER-ROTATION(CLIENT-PLAYER(CLIENT-ID)) TELEPORT-SENT(CLIENT-ID)
            IF ERRNO NOT = 0
                PERFORM HandleClientError
                EXIT SECTION
            END-IF

            *> send the new player to all other players
            MOVE CLIENT-ID TO TEMP-INT16
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = 4 AND CLIENT-ID NOT = TEMP-INT16
                    CALL "SendPacket-AddPlayer" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-UUID(TEMP-INT32) USERNAME(TEMP-INT32) USERNAME-LENGTH(TEMP-INT32)
                    PERFORM HandleClientError
                    *> spawn a player entity
                    IF ERRNO = 0
                        CALL "SendPacket-SpawnEntity" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32 PLAYER-UUID(TEMP-INT32) PLAYER-POSITION(TEMP-INT32) PLAYER-ROTATION(TEMP-INT32)
                        PERFORM HandleClientError
                    END-IF
                END-IF
            END-PERFORM
            MOVE TEMP-INT16 TO CLIENT-ID

            *> send "<username> joined the game" to all clients in play state, except the current client
            MOVE 0 TO BYTE-COUNT
            MOVE USERNAME(CLIENT-PLAYER(CLIENT-ID)) TO BUFFER
            ADD USERNAME-LENGTH(CLIENT-PLAYER(CLIENT-ID)) TO BYTE-COUNT
            MOVE " joined the game" TO BUFFER(BYTE-COUNT + 1:16)
            ADD 16 TO BYTE-COUNT
            DISPLAY BUFFER(1:BYTE-COUNT)
            PERFORM BroadcastMessageExceptCurrent

        WHEN OTHER
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandlePlay SECTION.
    EVALUATE PACKET-ID
        *> Confirm teleportation
        WHEN H'00'
            CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
            IF TEMP-INT32 > TELEPORT-RECV(CLIENT-ID) AND TEMP-INT32 <= TELEPORT-SENT(CLIENT-ID)
                MOVE TEMP-INT32 TO TELEPORT-RECV(CLIENT-ID)
            END-IF
        *> Chat message
        WHEN H'05'
            CALL "Decode-String" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION BYTE-COUNT BUFFER
            *> Message may not be longer than 256 characters
            IF BYTE-COUNT > 256
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF
            *> display the message in the server console
            DISPLAY "<" USERNAME(CLIENT-PLAYER(CLIENT-ID))(1:USERNAME-LENGTH(CLIENT-PLAYER(CLIENT-ID))) "> " BUFFER(1:BYTE-COUNT)
            *> send the message to all clients in play state
            MOVE CLIENT-ID TO TEMP-INT16
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = 4
                    CALL "SendPacket-PlayerChat" USING CLIENT-HNDL(CLIENT-ID) ERRNO PLAYER-UUID(TEMP-INT32) USERNAME(TEMP-INT32) BUFFER BYTE-COUNT
                    PERFORM HandleClientError
                END-IF
            END-PERFORM
            MOVE TEMP-INT16 TO CLIENT-ID
        *> KeepAlive response
        WHEN H'15'
            CALL "Decode-Long" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION KEEPALIVE-RECV(CLIENT-ID)
        *> Set player position
        WHEN H'17'
            *> Ignore movement packets until the client acknowledges the last sent teleport packet
            IF TELEPORT-RECV(CLIENT-ID) NOT = TELEPORT-SENT(CLIENT-ID)
                EXIT SECTION
            END-IF
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-X(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-Y(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-Z(CLIENT-PLAYER(CLIENT-ID))
            *> TODO: "on ground" flag
        *> Set player position and rotation
        WHEN H'18'
            IF TELEPORT-RECV(CLIENT-ID) NOT = TELEPORT-SENT(CLIENT-ID)
                EXIT SECTION
            END-IF
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-X(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-Y(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Double" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-Z(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Float" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-YAW(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Float" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-PITCH(CLIENT-PLAYER(CLIENT-ID))
            *> TODO: "on ground" flag
        *> Set player rotation
        WHEN H'19'
            IF TELEPORT-RECV(CLIENT-ID) NOT = TELEPORT-SENT(CLIENT-ID)
                EXIT SECTION
            END-IF
            CALL "Decode-Float" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-YAW(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Float" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION PLAYER-PITCH(CLIENT-PLAYER(CLIENT-ID))
            *> TODO: "on ground" flag
        *> Set player on ground
        WHEN H'1A'
            IF TELEPORT-RECV(CLIENT-ID) NOT = TELEPORT-SENT(CLIENT-ID)
                EXIT SECTION
            END-IF
            *> TODO: "on ground" flag
        *> Player action
        WHEN H'21'
            *> Status (= the action), block position, face, sequence number.
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
                    *> ignore face
                    CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
                    *> ensure the position is not outside the world
                    IF CHUNK-X >= -3 AND CHUNK-X <= 3 AND CHUNK-Z >= -3 AND CHUNK-Z <= 3 AND TEMP-POSITION-Y >= 0 AND TEMP-POSITION-Y < 384
                        *> acknowledge the action
                        CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
                        CALL "SendPacket-AckBlockChange" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32
                        IF ERRNO NOT = 0
                            PERFORM HandleClientError
                            EXIT SECTION
                        END-IF
                        *> update the block
                        MOVE 0 TO WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX)
                    END-IF
                    *> send the block update to all players
                    COMPUTE TEMP-POSITION-X = CHUNK-X * 16 + TEMP-POSITION-X
                    COMPUTE TEMP-POSITION-Z = CHUNK-Z * 16 + TEMP-POSITION-Z
                    COMPUTE TEMP-POSITION-Y = TEMP-POSITION-Y - 64
                    MOVE WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX) TO TEMP-INT32
                    *> store the current client ID while we interact with other clients
                    MOVE CLIENT-ID TO TEMP-INT16
                    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = 4
                            CALL "SendPacket-BlockUpdate" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-POSITION TEMP-INT32
                            PERFORM HandleClientError
                        END-IF
                    END-PERFORM
                    MOVE TEMP-INT16 TO CLIENT-ID
            END-EVALUATE
        *> Set held item
        WHEN H'2C'
            CALL "Decode-Short" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT16
            IF TEMP-INT16 >= 0 AND TEMP-INT16 <= 8
                MOVE TEMP-INT16 TO PLAYER-HOTBAR(CLIENT-PLAYER(CLIENT-ID))
            END-IF
        *> Set creative mode slot
        WHEN H'2F'
            *> slot ID
            CALL "Decode-Short" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT16
            *> TODO: spawn item entity when slot ID is -1
            *> slot description (present (boolean) [, item ID (VarInt), count (byte), NBT data])
            CALL "Decode-Byte" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT8
            IF TEMP-INT16 >= 0 AND TEMP-INT16 < 46
                IF TEMP-INT8 = 0
                    MOVE -1 TO PLAYER-INVENTORY-SLOT-ID(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                    MOVE 0 TO PLAYER-INVENTORY-SLOT-COUNT(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                ELSE
                    CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
                    MOVE TEMP-INT32 TO PLAYER-INVENTORY-SLOT-ID(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                    CALL "Decode-Byte" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT8
                    MOVE TEMP-INT8 TO PLAYER-INVENTORY-SLOT-COUNT(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                    *> remainder is NBT
                    COMPUTE BYTE-COUNT = PACKET-LENGTH(CLIENT-ID) - PACKET-POSITION + 1
                    IF BYTE-COUNT <= 1024
                        MOVE BYTE-COUNT TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                        MOVE PACKET-BUFFER(CLIENT-ID)(PACKET-POSITION:BYTE-COUNT) TO PLAYER-INVENTORY-SLOT-NBT-DATA(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)(1:BYTE-COUNT)
                    ELSE
                        MOVE 0 TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                        DISPLAY "Item NBT data too long: " BYTE-COUNT
                    END-IF
                END-IF
            END-IF
        *> Swing arm
        WHEN H'33'
            *> hand enum: 0=main hand, 1=off hand
            CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
            IF TEMP-INT32 = 1
                MOVE 3 TO TEMP-INT8
            ELSE
                MOVE 0 TO TEMP-INT8
            END-IF
            *> send an animation packet to each of the other players
            MOVE CLIENT-ID TO TEMP-INT16
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = 4 AND CLIENT-ID NOT = TEMP-INT16
                    CALL "SendPacket-EntityAnimation" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32 TEMP-INT8
                    PERFORM HandleClientError
                END-IF
            END-PERFORM
            MOVE TEMP-INT16 TO CLIENT-ID
        *> Use item on block
        WHEN H'35'
            *> hand enum: 0=main hand, 1=off hand
            CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
            IF TEMP-INT32 = 0
                *> compute the inventory slot
                COMPUTE TEMP-INT8 = 36 + PLAYER-HOTBAR(CLIENT-PLAYER(CLIENT-ID))
            ELSE
                MOVE 45 TO TEMP-INT8
            END-IF
            *> block position
            CALL "Decode-Position" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-POSITION
            *>  face enum (0-5): -Y, +Y, -Z, +Z, -X, +X
            CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
            *> TODO: cursor position, inside block
            ADD 13 TO PACKET-POSITION
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
                *> acknowledge the action
                CALL "Decode-VarInt" USING PACKET-BUFFER(CLIENT-ID) PACKET-POSITION TEMP-INT32
                CALL "SendPacket-AckBlockChange" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-INT32
                IF ERRNO NOT = 0
                    PERFORM HandleClientError
                    EXIT SECTION
                END-IF
                *> Determine the block to place by converting the item ID to a registry string, then looking for an
                *> identically named block in the blocks list.
                *> For example: item 27 -> "minecraft:grass_block" -> block state 9.
                MOVE PLAYER-INVENTORY-SLOT-ID(CLIENT-PLAYER(CLIENT-ID), TEMP-INT8 + 1) TO TEMP-INT32
                CALL "Registries-Get-EntryName" USING C-MINECRAFT-ITEM TEMP-INT32 TEMP-REGISTRY
                CALL "Blocks-Get-DefaultStateId" USING TEMP-REGISTRY TEMP-INT32
                *> Ensure the block was previously air
                IF TEMP-INT32 > 0 AND WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX) = 0
                    MOVE TEMP-INT32 TO WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX)
                END-IF
                *> send the block update to all players
                COMPUTE TEMP-POSITION-X = CHUNK-X * 16 + TEMP-POSITION-X
                COMPUTE TEMP-POSITION-Z = CHUNK-Z * 16 + TEMP-POSITION-Z
                COMPUTE TEMP-POSITION-Y = TEMP-POSITION-Y - 64
                MOVE WORLD-BLOCK-ID(CHUNK-INDEX, BLOCK-INDEX) TO TEMP-INT32
                *> store the current client ID while we interact with other clients
                MOVE CLIENT-ID TO TEMP-INT16
                PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                    IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = 4
                        CALL "SendPacket-BlockUpdate" USING CLIENT-HNDL(CLIENT-ID) ERRNO TEMP-POSITION TEMP-INT32
                        PERFORM HandleClientError
                    END-IF
                END-PERFORM
                MOVE TEMP-INT16 TO CLIENT-ID
            END-IF
    END-EVALUATE

    EXIT SECTION.

HandleServerError SECTION.
    IF ERRNO NOT = 0
        DISPLAY "Server socket error: " ERRNO
        STOP RUN
    END-IF.

    EXIT SECTION.

HandleClientError SECTION.
    IF ERRNO NOT = 0
        DISPLAY "[client=" CLIENT-ID ", state=" CLIENT-STATE(CLIENT-ID) "] Socket error: " ERRNO
        PERFORM DisconnectClient
    END-IF.

    EXIT SECTION.

END PROGRAM Server.
