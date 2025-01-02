*> --- Server ---
*> This is the entrypoint for starting a CobolCraft server.
IDENTIFICATION DIVISION.
PROGRAM-ID. Server.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> File names
    01 FILE-REGISTRIES              PIC X(255)              VALUE "data/generated/reports/registries.json".
    01 FILE-BLOCKS                  PIC X(255)              VALUE "data/generated/reports/blocks.json".
    01 FILE-DATAPACK-ROOT           PIC X(255)              VALUE "data/generated/data/".
    *> Constants
    COPY DD-CLIENT-STATES.
    01 C-MINECRAFT-WORLDGEN-BIOME   PIC X(50)               VALUE "minecraft:worldgen/biome".
    01 C-MINECRAFT-CHAT_TYPE        PIC X(50)               VALUE "minecraft:chat_type".
    01 C-MINECRAFT-TRIM_PATTERN     PIC X(50)               VALUE "minecraft:trim_pattern".
    01 C-MINECRAFT-TRIM_MATERIAL    PIC X(50)               VALUE "minecraft:trim_material".
    01 C-MINECRAFT-WOLF_VARIANT     PIC X(50)               VALUE "minecraft:wolf_variant".
    01 C-MINECRAFT-PAINTING_VARIANT PIC X(50)               VALUE "minecraft:painting_variant".
    01 C-MINECRAFT-DIMENSION_TYPE   PIC X(50)               VALUE "minecraft:dimension_type".
    01 C-MINECRAFT-DAMAGE_TYPE      PIC X(50)               VALUE "minecraft:damage_type".
    01 C-MINECRAFT-BANNER_PATTERN   PIC X(50)               VALUE "minecraft:banner_pattern".
    01 C-MINECRAFT-ENCHANTMENT      PIC X(50)               VALUE "minecraft:enchantment".
    01 C-MINECRAFT-JUKEBOX_SONG     PIC X(50)               VALUE "minecraft:jukebox_song".
    01 C-MINECRAFT-ITEM             PIC X(50)               VALUE "minecraft:item".
    01 C-COLOR-YELLOW               PIC X(16)               VALUE "yellow".
    *> Configuration
    COPY DD-SERVER-PROPERTIES.
    *> The server sends (2 * VIEW-DISTANCE + 1) * (2 * VIEW-DISTANCE + 1) chunks around the player.
    *> TODO: Improve performance so this can be increased to a reasonable value.
    01 VIEW-DISTANCE                BINARY-LONG             VALUE 3.
    *> The amount of milliseconds between autosaves, and the last autosave timestamp.
    01 AUTOSAVE-INTERVAL            BINARY-LONG             VALUE 300000.
    01 LAST-AUTOSAVE                BINARY-LONG-LONG.
    *> A large buffer to hold JSON data before parsing.
    01 DATA-BUFFER                  PIC X(10000000).
    01 DATA-BUFFER-LEN              BINARY-LONG UNSIGNED.
    01 DATA-FAILURE                 BINARY-CHAR UNSIGNED.
    *> Socket variables (server socket handle, error number from last operation)
    01 SERVER-HNDL                  PIC X(4)                EXTERNAL.
    01 ERRNO                        PIC 9(3).
    *> Connected clients
    COPY DD-CLIENTS.
    *> The client handle of the connection that is currently being processed, and the index in the CLIENTS array
    01 TEMP-HNDL                    PIC X(4).
    01 CLIENT-ID                    BINARY-LONG UNSIGNED.
    *> TODO: remove need to access player data directly in this file
    COPY DD-PLAYERS.
    *> Incoming/outgoing packet data
    01 PACKET-ID                    BINARY-LONG.
    01 PACKET-POSITION              BINARY-LONG UNSIGNED.
    01 BUFFER                       PIC X(64000).
    01 BYTE-COUNT                   BINARY-LONG UNSIGNED.
    01 BYTE-COUNT-EXPECTED          BINARY-LONG UNSIGNED.
    *> Temporary variables
    01 TEMP-INT8                    BINARY-CHAR.
    01 TEMP-INT16                   BINARY-SHORT.
    01 TEMP-INT32                   BINARY-LONG.
    01 TEMP-INT64                   BINARY-LONG-LONG.
    01 TEMP-INT64-2                 BINARY-LONG-LONG.
    01 TEMP-FLOAT                   FLOAT-SHORT.
    01 TEMP-UUID                    PIC X(16).
    01 TEMP-POSITION.
        02 TEMP-POSITION-X          BINARY-LONG.
        02 TEMP-POSITION-Y          BINARY-LONG.
        02 TEMP-POSITION-Z          BINARY-LONG.
    01 TEMP-BLOCK-FACE              BINARY-LONG.
    01 TEMP-CURSOR.
        02 TEMP-CURSOR-X            FLOAT-SHORT.
        02 TEMP-CURSOR-Y            FLOAT-SHORT.
        02 TEMP-CURSOR-Z            FLOAT-SHORT.
    01 TEMP-PLAYER-NAME             PIC X(16).
    01 TEMP-PLAYER-NAME-LEN         BINARY-LONG UNSIGNED.
    01 TEMP-IDENTIFIER              PIC X(100).
    01 CALLBACK-PTR-ITEM            PROGRAM-POINTER.
    01 CALLBACK-PTR-BLOCK           PROGRAM-POINTER.
    01 SEQUENCE-ID                  BINARY-LONG.
    *> Time measurement
    01 CURRENT-TIME                 BINARY-LONG-LONG.
    01 TICK-ENDTIME                 BINARY-LONG-LONG.
    *> Variables for working with chunks
    01 CHUNK-X                      BINARY-LONG.
    01 CHUNK-Z                      BINARY-LONG.
    01 CHUNK-START-X                BINARY-LONG.
    01 CHUNK-END-X                  BINARY-LONG.
    01 CHUNK-START-Z                BINARY-LONG.
    01 CHUNK-END-Z                  BINARY-LONG.
    01 PREV-CENTER-CHUNK-X          BINARY-LONG.
    01 PREV-CENTER-CHUNK-Z          BINARY-LONG.
    *> Variables used for datapack loading
    01 VANILLA-DATAPACK-NAME        PIC X(50)               VALUE "minecraft".
    01 REGISTRY-COUNT               BINARY-LONG UNSIGNED.
    *> Variables used for item registration
    01 REGISTRY-INDEX               BINARY-LONG.
    01 REGISTRY-LENGTH              BINARY-LONG UNSIGNED.
    01 REGISTRY-ENTRY-INDEX         BINARY-LONG UNSIGNED.
    01 REGISTRY-ENTRY-NAME          PIC X(100).
    *> Variables used for block registration
    01 BLOCK-COUNT                  BINARY-LONG.
    01 BLOCK-INDEX                  BINARY-LONG.
    01 BLOCK-MINIMUM-STATE-ID       BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID       BINARY-LONG.
    01 BLOCK-STATE-ID               BINARY-LONG.
    *> TODO: remove need to access world data directly in this file
    COPY DD-WORLD.

PROCEDURE DIVISION.
LoadRegistries.
    DISPLAY "Loading registries"
    CALL "Files-ReadAll" USING FILE-REGISTRIES DATA-BUFFER DATA-BUFFER-LEN DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to read: " FUNCTION TRIM(FILE-REGISTRIES)
        STOP RUN RETURNING 1
    END-IF
    CALL "Registries-Parse" USING DATA-BUFFER DATA-BUFFER-LEN DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to parse registries"
        STOP RUN RETURNING 1
    END-IF

    *> Create additional registries not exported by the report generator.
    *> Set the flag to 1 to indicate that they need to be part of a Registry Data packet sent to the client.
    MOVE 1 TO TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-WORLDGEN-BIOME TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-CHAT_TYPE TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-TRIM_PATTERN TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-TRIM_MATERIAL TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-WOLF_VARIANT TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-PAINTING_VARIANT TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-DIMENSION_TYPE TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-DAMAGE_TYPE TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-BANNER_PATTERN TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-ENCHANTMENT TEMP-INT8
    CALL "Registries-Create" USING C-MINECRAFT-JUKEBOX_SONG TEMP-INT8
    .

LoadBlocks.
    DISPLAY "Loading blocks"
    CALL "Files-ReadAll" USING FILE-BLOCKS DATA-BUFFER DATA-BUFFER-LEN DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to read: " FUNCTION TRIM(FILE-BLOCKS)
        STOP RUN RETURNING 1
    END-IF
    CALL "Blocks-Parse" USING DATA-BUFFER DATA-BUFFER-LEN DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to parse blocks"
        STOP RUN RETURNING 1
    END-IF
    .

LoadDatapack.
    DISPLAY "Loading vanilla datapack"
    CALL "Datapack-Load" USING FILE-DATAPACK-ROOT VANILLA-DATAPACK-NAME DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to load datapack"
        STOP RUN RETURNING 1
    END-IF

    *> Sanity check: There shouldn't be any empty registries now.
    CALL "Registries-GetCount" USING REGISTRY-COUNT
    PERFORM VARYING REGISTRY-INDEX FROM 1 BY 1 UNTIL REGISTRY-INDEX > REGISTRY-COUNT
        CALL "Registries-GetRegistryLength" USING REGISTRY-INDEX REGISTRY-LENGTH
        IF REGISTRY-LENGTH <= 0
            CALL "Registries-Iterate-Name" USING REGISTRY-INDEX TEMP-IDENTIFIER
            DISPLAY "Error: Empty registry '" FUNCTION TRIM(TEMP-IDENTIFIER) "'!"
            STOP RUN RETURNING 1
        END-IF
    END-PERFORM
    .

RegisterItems.
    DISPLAY "Registering items"

    *> Register a generic block item for each item that has an identically-named block
    CALL "Registries-GetRegistryIndex" USING C-MINECRAFT-ITEM REGISTRY-INDEX
    IF REGISTRY-INDEX <= 0
        DISPLAY "Failed to find " FUNCTION TRIM(C-MINECRAFT-ITEM) " registry"
        STOP RUN RETURNING 1
    END-IF
    CALL "Registries-GetRegistryLength" USING REGISTRY-INDEX REGISTRY-LENGTH
    PERFORM VARYING REGISTRY-ENTRY-INDEX FROM 1 BY 1 UNTIL REGISTRY-ENTRY-INDEX > REGISTRY-LENGTH
        CALL "Registries-Iterate-EntryName" USING REGISTRY-INDEX REGISTRY-ENTRY-INDEX REGISTRY-ENTRY-NAME
        CALL "Blocks-Get-DefaultStateId" USING REGISTRY-ENTRY-NAME TEMP-INT32
        IF TEMP-INT32 > 0
            CALL "RegisterItem-Block" USING REGISTRY-ENTRY-NAME
        END-IF
    END-PERFORM

    *> Register items with special handling
    CALL "RegisterItem-Torch"
    CALL "RegisterItem-Slab"
    CALL "RegisterItem-Stairs"
    CALL "RegisterItem-RotatedPillar"
    CALL "RegisterItem-Button"
    CALL "RegisterItem-Door"
    CALL "RegisterItem-Trapdoor"
    CALL "RegisterItem-Bed"
    CALL "RegisterItem-Bucket"
    CALL "RegisterItem-WaterBucket"
    CALL "RegisterItem-LavaBucket"
    .

RegisterBlocks.
    DISPLAY "Registering blocks"

    *> Register generic handlers for every block state
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-StateIds" USING BLOCK-INDEX BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
        PERFORM VARYING BLOCK-STATE-ID FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL BLOCK-STATE-ID > BLOCK-MAXIMUM-STATE-ID
            CALL "RegisterBlock-Generic" USING BLOCK-STATE-ID
        END-PERFORM
    END-PERFORM

    *> Register blocks with special handling
    CALL "RegisterBlock-Air"
    CALL "RegisterBlock-TallGrass"
    CALL "RegisterBlock-Torch"
    CALL "RegisterBlock-Slab"
    CALL "RegisterBlock-Door"
    CALL "RegisterBlock-Trapdoor"
    CALL "RegisterBlock-Bed"
    CALL "RegisterBlock-Water"
    CALL "RegisterBlock-Lava"
    .

RegisterCommands.
    DISPLAY "Registering commands"

    CALL "RegisterCommand-GameMode"
    CALL "RegisterCommand-Help"
    CALL "RegisterCommand-Say"
    CALL "RegisterCommand-Save"
    CALL "RegisterCommand-Stop"
    CALL "RegisterCommand-Time"
    CALL "RegisterCommand-Whitelist"
    .

LoadProperties.
    DISPLAY "Loading server properties"
    CALL "ServerProperties-Read" USING DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to read server.properties"
        STOP RUN RETURNING 1
    END-IF

    *> Always write out server.properties to ensure that any missing properties are added
    CALL "ServerProperties-Write" USING DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to write server.properties"
        STOP RUN RETURNING 1
    END-IF

    DISPLAY "Loading whitelist"
    CALL "Whitelist-Read" USING DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to read whitelist"
        STOP RUN RETURNING 1
    END-IF
    .

GenerateWorld.
    DISPLAY "Loading world"
    *> prepare chunks
    CALL "World-Load" USING TEMP-INT8
    IF TEMP-INT8 NOT = 0
        DISPLAY "Failed to load world"
        STOP RUN RETURNING 1
    END-IF
    *> prepare player data
    CALL "Players-Init"
    *> don't autosave immediately
    CALL "SystemTimeMillis" USING LAST-AUTOSAVE
    .

StartServer.
    DISPLAY "Starting server"

    CALL "IgnoreSIGPIPE"
    CALL "SetConsoleNonBlocking" GIVING ERRNO
    IF ERRNO NOT = 0
        DISPLAY "Could not set console to non-blocking mode."
        STOP RUN RETURNING 1
    END-IF

    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        MOVE 0 TO CLIENT-PRESENT(CLIENT-ID)
        MOVE CLIENT-STATE-DISCONNECTED TO CLIENT-STATE(CLIENT-ID)
    END-PERFORM

    CALL "SocketListen" USING SP-PORT SERVER-HNDL GIVING ERRNO
    PERFORM HandleServerError

    DISPLAY "Done! For help, type ""help"""
    .

ServerLoop.
    *> Loop forever - each iteration is one game tick (1/20th of a second).
    PERFORM UNTIL EXIT
        CALL "SystemTimeMillis" USING CURRENT-TIME
        COMPUTE TICK-ENDTIME = CURRENT-TIME + (1000 / 20)

        COMPUTE TEMP-INT64 = CURRENT-TIME - LAST-AUTOSAVE
        IF TEMP-INT64 >= AUTOSAVE-INTERVAL
            CALL "Server-Save"
            MOVE CURRENT-TIME TO LAST-AUTOSAVE
        END-IF

        *> Update the game state
        PERFORM GameLoop

        *> Handle keep-alive and disconnections for connected clients
        PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
            EVALUATE TRUE
                WHEN CLIENT-PRESENT(CLIENT-ID) = 0
                    CONTINUE
                WHEN CLIENT-ERRNO-SEND(CLIENT-ID) NOT = 0
                    MOVE CLIENT-ERRNO-SEND(CLIENT-ID) TO ERRNO
                    PERFORM HandleClientError
                WHEN OTHER
                    PERFORM KeepAlive
            END-EVALUATE
        END-PERFORM

        *> Send world time every second
        CALL "World-GetAge" USING TEMP-INT64
        IF FUNCTION MOD(TEMP-INT64, 20) = 0
            CALL "World-GetTime" USING TEMP-INT64-2
            PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
                    CALL "SendPacket-UpdateTime" USING CLIENT-ID TEMP-INT64 TEMP-INT64-2
                END-IF
            END-PERFORM
        END-IF

        *> broadcast player positions to all clients in play state, as well as their equipment
        *> TODO: only send this if the player has moved/rotated, or if the equipment has changed
        *> TODO: use more efficient packet types when possible
        PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
            IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
                *> Note: Since sending the packet can fail, we need to stop the loop if the client is disconnected.
                PERFORM VARYING TEMP-INT16 FROM 1 BY 1 UNTIL TEMP-INT16 > MAX-CLIENTS OR CLIENT-PRESENT(CLIENT-ID) = 0
                    IF CLIENT-PRESENT(TEMP-INT16) = 1 AND CLIENT-STATE(TEMP-INT16) = CLIENT-STATE-PLAY AND TEMP-INT16 NOT = CLIENT-ID
                        MOVE CLIENT-PLAYER(TEMP-INT16) TO TEMP-INT32
                        CALL "SendPacket-EntityPositionSync" USING CLIENT-ID TEMP-INT32 PLAYER-POSITION(TEMP-INT32) PLAYER-ROTATION(TEMP-INT32)
                        CALL "SendPacket-SetHeadRotation" USING CLIENT-ID TEMP-INT32 PLAYER-YAW(TEMP-INT32)
                        *> index(byte), type(VarInt), value(VarInt); terminator is 0xFF
                        *> index of pose: 6, type of pose: 21
                        *> value of standing: 0, value of sneaking: 5
                        MOVE X"06" TO BUFFER(1:1)
                        MOVE X"15" TO BUFFER(2:1)
                        IF PLAYER-SNEAKING(TEMP-INT32) = 1
                            MOVE X"05" TO BUFFER(3:1)
                        ELSE
                            MOVE X"00" TO BUFFER(3:1)
                        END-IF
                        MOVE X"FF" TO BUFFER(4:1)
                        MOVE 4 TO BYTE-COUNT
                        CALL "SendPacket-SetEntityMetadata" USING CLIENT-ID TEMP-INT32 BYTE-COUNT BUFFER
                        COMPUTE TEMP-INT8 = 36 + PLAYER-HOTBAR(TEMP-INT32) + 1
                        CALL "SendPacket-SetEquipment" USING CLIENT-ID TEMP-INT32 PLAYER-INVENTORY-SLOT(TEMP-INT32, TEMP-INT8)
                    END-IF
                END-PERFORM
            END-IF
        END-PERFORM

        *> update chunks around players
        PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
            IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
                PERFORM SendChunks
            END-IF
        END-PERFORM
        CALL "World-UnloadChunks" USING VIEW-DISTANCE TEMP-INT8
        IF TEMP-INT8 NOT = 0
            DISPLAY "Failure unloading chunks"
            STOP RUN RETURNING 1
        END-IF

        *> Read console command
        PERFORM ConsoleInput

        *> The remaining time of this tick can be used for accepting connections and receiving packets.
        PERFORM UNTIL CURRENT-TIME >= TICK-ENDTIME
            PERFORM NetworkRead
            CALL "SystemTimeMillis" USING CURRENT-TIME
        END-PERFORM

        MOVE X"00000000" TO TEMP-HNDL
        MOVE 0 TO CLIENT-ID
    END-PERFORM
    .

GameLoop SECTION.
    *> Update the world age
    CALL "World-UpdateAge"

    EXIT SECTION.

ConsoleInput SECTION.
    *> Read from the console (configured as non-blocking). Note that this will only return full lines.
    MOVE LENGTH OF BUFFER TO BYTE-COUNT
    CALL "ReadConsole" USING BUFFER BYTE-COUNT
    IF BYTE-COUNT > 0
        *> client id = 0 means the console
        MOVE 0 TO TEMP-INT32
        CALL "HandleCommand" USING TEMP-INT32 BUFFER BYTE-COUNT
    END-IF
    EXIT SECTION.

NetworkRead SECTION.
    CALL "SocketPoll" USING SERVER-HNDL TEMP-HNDL GIVING ERRNO
    IF ERRNO NOT = 0
        PERFORM HandleServerError
        EXIT SECTION
    END-IF
    *> Without anything to do, sleep for a millisecond to avoid busy-waiting
    IF TEMP-HNDL = X"00000000"
        CALL "CBL_GC_NANOSLEEP" USING 1000000
        EXIT SECTION
    END-IF

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
    CALL "SocketClose" USING TEMP-HNDL GIVING ERRNO
    IF ERRNO NOT = 0
        PERFORM HandleServerError
    END-IF

    EXIT SECTION.

InsertClient SECTION.
    INITIALIZE CLIENT(CLIENT-ID)

    MOVE 1 TO CLIENT-PRESENT(CLIENT-ID)
    MOVE TEMP-HNDL TO CLIENT-HNDL(CLIENT-ID)
    MOVE CLIENT-STATE-HANDSHAKE TO CLIENT-STATE(CLIENT-ID)

    ALLOCATE RECEIVE-BUFFER-LENGTH CHARACTERS RETURNING PACKET-BUFFER(CLIENT-ID)

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
    IF CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY AND TEMP-INT64 >= 1000
        MOVE CURRENT-TIME TO KEEPALIVE-SENT(CLIENT-ID)
        CALL "SendPacket-KeepAlive" USING CLIENT-ID KEEPALIVE-SENT(CLIENT-ID)
    END-IF

    EXIT SECTION.

SendChunks SECTION.
    *> send up to 1 chunk to the client per tick
    PERFORM ProcessChunkQueue
    IF CLIENT-PRESENT(CLIENT-ID) = 0
        *> The client disconnected while processing the queue
        EXIT SECTION
    END-IF

    *> compute the new center chunk position
    MOVE CENTER-CHUNK-X(CLIENT-ID) TO PREV-CENTER-CHUNK-X
    MOVE CENTER-CHUNK-Z(CLIENT-ID) TO PREV-CENTER-CHUNK-Z
    DIVIDE PLAYER-X(CLIENT-PLAYER(CLIENT-ID)) BY 16 GIVING CENTER-CHUNK-X(CLIENT-ID) ROUNDED MODE IS TOWARD-LESSER
    DIVIDE PLAYER-Z(CLIENT-PLAYER(CLIENT-ID)) BY 16 GIVING CENTER-CHUNK-Z(CLIENT-ID) ROUNDED MODE IS TOWARD-LESSER

    IF CENTER-CHUNK-X(CLIENT-ID) NOT = PREV-CENTER-CHUNK-X OR CENTER-CHUNK-Z(CLIENT-ID) NOT = PREV-CENTER-CHUNK-Z
        *> send center chunk position
        CALL "SendPacket-SetCenterChunk" USING CLIENT-ID CENTER-CHUNK-X(CLIENT-ID) CENTER-CHUNK-Z(CLIENT-ID)

        *> compute chunk area that came into view
        *> TODO: make this code look better

        *> first: parallel to the X axis
        COMPUTE CHUNK-START-X = CENTER-CHUNK-X(CLIENT-ID) - VIEW-DISTANCE
        COMPUTE CHUNK-END-X = CENTER-CHUNK-X(CLIENT-ID) + VIEW-DISTANCE
        IF CENTER-CHUNK-Z(CLIENT-ID) < PREV-CENTER-CHUNK-Z
            COMPUTE CHUNK-START-Z = CENTER-CHUNK-Z(CLIENT-ID) - VIEW-DISTANCE
            COMPUTE CHUNK-END-Z = FUNCTION MIN(PREV-CENTER-CHUNK-Z - VIEW-DISTANCE, CENTER-CHUNK-Z(CLIENT-ID) + VIEW-DISTANCE)
        ELSE
            COMPUTE CHUNK-START-Z = FUNCTION MAX(PREV-CENTER-CHUNK-Z + VIEW-DISTANCE, CENTER-CHUNK-Z(CLIENT-ID) - VIEW-DISTANCE)
            COMPUTE CHUNK-END-Z = CENTER-CHUNK-Z(CLIENT-ID) + VIEW-DISTANCE
        END-IF
        PERFORM VARYING CHUNK-X FROM CHUNK-START-X BY 1 UNTIL CHUNK-X > CHUNK-END-X
            PERFORM VARYING CHUNK-Z FROM CHUNK-START-Z BY 1 UNTIL CHUNK-Z > CHUNK-END-Z
                PERFORM EnqueueChunk
            END-PERFORM
        END-PERFORM

        *> second: parallel to the Z axis
        COMPUTE CHUNK-START-Z = CENTER-CHUNK-Z(CLIENT-ID) - VIEW-DISTANCE
        COMPUTE CHUNK-END-Z = CENTER-CHUNK-Z(CLIENT-ID) + VIEW-DISTANCE
        IF CENTER-CHUNK-X(CLIENT-ID) < PREV-CENTER-CHUNK-X
            COMPUTE CHUNK-START-X = CENTER-CHUNK-X(CLIENT-ID) - VIEW-DISTANCE
            COMPUTE CHUNK-END-X = FUNCTION MIN(PREV-CENTER-CHUNK-X - VIEW-DISTANCE, CENTER-CHUNK-X(CLIENT-ID) + VIEW-DISTANCE)
        ELSE
            COMPUTE CHUNK-START-X = FUNCTION MAX(PREV-CENTER-CHUNK-X + VIEW-DISTANCE, CENTER-CHUNK-X(CLIENT-ID) - VIEW-DISTANCE)
            COMPUTE CHUNK-END-X = CENTER-CHUNK-X(CLIENT-ID) + VIEW-DISTANCE
        END-IF
        PERFORM VARYING CHUNK-X FROM CHUNK-START-X BY 1 UNTIL CHUNK-X > CHUNK-END-X
            PERFORM VARYING CHUNK-Z FROM CHUNK-START-Z BY 1 UNTIL CHUNK-Z > CHUNK-END-Z
                PERFORM EnqueueChunk
            END-PERFORM
        END-PERFORM
    END-IF

    EXIT SECTION.

EnqueueChunk SECTION.
    *> Overflow would occur if (end + 1) % length == begin
    COMPUTE TEMP-INT32 = CHUNK-QUEUE-END(CLIENT-ID) + 1
    COMPUTE TEMP-INT32 = FUNCTION MOD(TEMP-INT32, CHUNK-QUEUE-LENGTH)
    IF TEMP-INT32 = CHUNK-QUEUE-BEGIN(CLIENT-ID)
        DISPLAY "[client=" CLIENT-ID "] Chunk queue overflow!"
        EXIT SECTION
    END-IF

    *> Check for duplicates
    MOVE CHUNK-QUEUE-BEGIN(CLIENT-ID) TO TEMP-INT32
    PERFORM UNTIL TEMP-INT32 = CHUNK-QUEUE-END(CLIENT-ID)
        IF CHUNK-QUEUE-X(CLIENT-ID, TEMP-INT32 + 1) = CHUNK-X AND CHUNK-QUEUE-Z(CLIENT-ID, TEMP-INT32 + 1) = CHUNK-Z
            EXIT SECTION
        END-IF
        ADD 1 TO TEMP-INT32
        IF TEMP-INT32 >= CHUNK-QUEUE-LENGTH
            MOVE 0 TO TEMP-INT32
        END-IF
    END-PERFORM

    *> Insert the chunk at the current end position
    MOVE CHUNK-X TO CHUNK-QUEUE-X(CLIENT-ID, CHUNK-QUEUE-END(CLIENT-ID) + 1)
    MOVE CHUNK-Z TO CHUNK-QUEUE-Z(CLIENT-ID, CHUNK-QUEUE-END(CLIENT-ID) + 1)

    *> Move the end pointer one beyond the new item
    ADD 1 TO CHUNK-QUEUE-END(CLIENT-ID)
    IF CHUNK-QUEUE-END(CLIENT-ID) >= CHUNK-QUEUE-LENGTH
        MOVE 0 TO CHUNK-QUEUE-END(CLIENT-ID)
    END-IF

    EXIT SECTION.

ProcessChunkQueue SECTION.
    *> Determine the client's view area to avoid sending chunks outside of it
    COMPUTE CHUNK-START-X = CENTER-CHUNK-X(CLIENT-ID) - VIEW-DISTANCE
    COMPUTE CHUNK-END-X = CENTER-CHUNK-X(CLIENT-ID) + VIEW-DISTANCE
    COMPUTE CHUNK-START-Z = CENTER-CHUNK-Z(CLIENT-ID) - VIEW-DISTANCE
    COMPUTE CHUNK-END-Z = CENTER-CHUNK-Z(CLIENT-ID) + VIEW-DISTANCE

    *> Since end points one beyond the last item, the queue is empty once begin = end.
    PERFORM UNTIL CHUNK-QUEUE-BEGIN(CLIENT-ID) = CHUNK-QUEUE-END(CLIENT-ID)
        *> Dequeue the next chunk
        MOVE CHUNK-QUEUE-X(CLIENT-ID, CHUNK-QUEUE-BEGIN(CLIENT-ID) + 1) TO CHUNK-X
        MOVE CHUNK-QUEUE-Z(CLIENT-ID, CHUNK-QUEUE-BEGIN(CLIENT-ID) + 1) TO CHUNK-Z
        ADD 1 TO CHUNK-QUEUE-BEGIN(CLIENT-ID)
        IF CHUNK-QUEUE-BEGIN(CLIENT-ID) >= CHUNK-QUEUE-LENGTH
            MOVE 0 TO CHUNK-QUEUE-BEGIN(CLIENT-ID)
        END-IF
        *> Check if the chunk is within the client's view area
        IF CHUNK-X >= CHUNK-START-X AND CHUNK-X <= CHUNK-END-X AND CHUNK-Z >= CHUNK-START-Z AND CHUNK-Z <= CHUNK-END-Z
            CALL "World-EnsureChunk" USING CHUNK-X CHUNK-Z TEMP-INT32
            IF TEMP-INT32 > 0
                CALL "SendPacket-ChunkData" USING CLIENT-ID WORLD-CHUNK(TEMP-INT32)
            END-IF
            *> Stop once a chunk has been sent
            EXIT PERFORM
        END-IF
    END-PERFORM

    EXIT SECTION.

ReceivePacket SECTION.
    *> Ignore any attempts to receive data for clients that are not in a valid state
    IF CLIENT-STATE(CLIENT-ID) < 0
        EXIT SECTION
    END-IF

    *> Select the current client's buffer
    SET ADDRESS OF CLIENT-RECEIVE-BUFFER TO PACKET-BUFFER(CLIENT-ID)

    *> Receive the packet length. This is a VarInt, so it has to be done byte by byte (up to 5 bytes).
    PERFORM UNTIL PACKET-LENGTH(CLIENT-ID) > 0
        MOVE 1 TO BYTE-COUNT
        CALL "SocketRead" USING CLIENT-HNDL(CLIENT-ID) BYTE-COUNT BUFFER GIVING ERRNO
        IF ERRNO NOT = 0
            PERFORM HandleClientError
            EXIT SECTION
        END-IF
        IF BYTE-COUNT = 0
            *> No data read, try again later
            EXIT SECTION
        END-IF

        ADD 1 TO PACKET-BUFFERLEN(CLIENT-ID)
        MOVE BUFFER(1:1) TO CLIENT-RECEIVE-BUFFER(PACKET-BUFFERLEN(CLIENT-ID):1)

        *> Once the most significant bit is 0, the VarInt is complete.
        IF FUNCTION ORD(BUFFER) <= 128
            MOVE 1 TO PACKET-POSITION
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PACKET-LENGTH(CLIENT-ID)
            IF PACKET-LENGTH(CLIENT-ID) < 1 OR PACKET-LENGTH(CLIENT-ID) > 2097151
                DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Received invalid packet length: " PACKET-LENGTH(CLIENT-ID)
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF
            MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)
            EXIT PERFORM
        END-IF

        IF PACKET-BUFFERLEN(CLIENT-ID) >= 5
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Received invalid packet length."
            PERFORM DisconnectClient
            EXIT SECTION
        END-IF
    END-PERFORM

    *> Receive the packet ID and payload
    PERFORM UNTIL PACKET-BUFFERLEN(CLIENT-ID) >= PACKET-LENGTH(CLIENT-ID)
        *> The socket library can only read up to 64 kB at a time.
        MOVE FUNCTION MIN(PACKET-LENGTH(CLIENT-ID) - PACKET-BUFFERLEN(CLIENT-ID), 64000) TO BYTE-COUNT-EXPECTED BYTE-COUNT
        CALL "SocketRead" USING CLIENT-HNDL(CLIENT-ID) BYTE-COUNT CLIENT-RECEIVE-BUFFER(PACKET-BUFFERLEN(CLIENT-ID) + 1:) GIVING ERRNO
        IF ERRNO NOT = 0
            PERFORM HandleClientError
            EXIT SECTION
        END-IF
        ADD BYTE-COUNT TO PACKET-BUFFERLEN(CLIENT-ID)
        IF BYTE-COUNT < BYTE-COUNT-EXPECTED
            *> Not enough data read, try again later
            EXIT SECTION
        END-IF
    END-PERFORM

    *> Packet received, process it
    MOVE 1 TO PACKET-POSITION
    CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PACKET-ID

    EVALUATE CLIENT-STATE(CLIENT-ID)
        WHEN CLIENT-STATE-HANDSHAKE
            PERFORM HandleHandshake
        WHEN CLIENT-STATE-STATUS
            PERFORM HandleStatus
        WHEN CLIENT-STATE-LOGIN
            PERFORM HandleLogin
        WHEN CLIENT-STATE-CONFIGURATION
            PERFORM HandleConfiguration
        WHEN CLIENT-STATE-PLAY
            PERFORM HandlePlay
        WHEN OTHER
            DISPLAY "Invalid client state: " CLIENT-STATE(CLIENT-ID)
            PERFORM DisconnectClient
            EXIT SECTION
    END-EVALUATE

    *> Reset length for the next packet
    MOVE 0 TO PACKET-LENGTH(CLIENT-ID)
    MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)

    EXIT SECTION.

HandleHandshake SECTION.
    IF PACKET-ID NOT = H'00'
        DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet ID: " PACKET-ID
        PERFORM DisconnectClient
        EXIT SECTION
    END-IF

    *> The final byte of the payload encodes the target state.
    COMPUTE CLIENT-STATE(CLIENT-ID) = FUNCTION ORD(CLIENT-RECEIVE-BUFFER(PACKET-LENGTH(CLIENT-ID):1)) - 1
    IF CLIENT-STATE(CLIENT-ID) NOT = CLIENT-STATE-STATUS AND CLIENT-STATE(CLIENT-ID) NOT = CLIENT-STATE-LOGIN
        DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Client requested invalid target state: " CLIENT-STATE(CLIENT-ID)
        PERFORM DisconnectClient
        EXIT SECTION
    END-IF

    EXIT SECTION.

HandleStatus SECTION.
    EVALUATE PACKET-ID
        *> Status request
        WHEN H'00'
            *> count the number of current players
            MOVE 0 TO TEMP-INT32
            PERFORM VARYING TEMP-INT16 FROM 1 BY 1 UNTIL TEMP-INT16 > MAX-CLIENTS
                IF CLIENT-PRESENT(TEMP-INT16) = 1 AND CLIENT-PLAYER(TEMP-INT16) > 0
                    ADD 1 TO TEMP-INT32
                END-IF
            END-PERFORM
            CALL "SendPacket-Status" USING CLIENT-ID SP-MOTD MAX-PLAYERS TEMP-INT32

        *> Ping request: respond with the same payload and close the connection
        WHEN H'01'
            CALL "Decode-Long" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT64
            CALL "SendPacket-PingResponse" USING CLIENT-ID TEMP-INT64
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
            CALL "Decode-String" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-PLAYER-NAME-LEN TEMP-PLAYER-NAME
            MOVE CLIENT-RECEIVE-BUFFER(PACKET-POSITION:16) TO TEMP-UUID
            ADD 16 TO PACKET-POSITION

            *> For testing, we want to allow the same UUID to connect multiple times with different usernames.
            *> Since this is an offline server, we can simply generate our own UUID to achieve this.
            *> For lack of a better implementation, we will simply use the bytes of the username as the UUID.
            MOVE X"00000000000000000000000000000000" TO TEMP-UUID
            MOVE TEMP-PLAYER-NAME(1:TEMP-PLAYER-NAME-LEN) TO TEMP-UUID(1:TEMP-PLAYER-NAME-LEN)

            *> Check username against the whitelist
            IF SP-WHITELIST-ENABLE > 0
                CALL "Whitelist-Check" USING TEMP-UUID TEMP-PLAYER-NAME TEMP-PLAYER-NAME-LEN TEMP-INT8
                IF TEMP-INT8 = 0
                    MOVE "You are not white-listed on this server!" TO BUFFER
                    MOVE 40 TO BYTE-COUNT
                    DISPLAY "Disconnecting " TEMP-PLAYER-NAME(1:TEMP-PLAYER-NAME-LEN) ": " BUFFER(1:BYTE-COUNT)
                    CALL "SendPacket-Disconnect" USING CLIENT-ID CLIENT-STATE(CLIENT-ID) BUFFER BYTE-COUNT
                    PERFORM DisconnectClient
                    EXIT SECTION
                END-IF
            END-IF

            *> If the player is already connected, disconnect them first
            CALL "Players-FindConnectedByUUID" USING TEMP-UUID TEMP-INT8
            IF TEMP-INT8 > 0
                MOVE CLIENT-ID TO TEMP-INT64
                MOVE PLAYER-CLIENT(TEMP-INT8) TO CLIENT-ID
                MOVE "You logged in from another location" TO BUFFER
                MOVE 35 TO BYTE-COUNT
                DISPLAY "Disconnecting " PLAYER-NAME(TEMP-INT8)(1:PLAYER-NAME-LENGTH(TEMP-INT8)) ": " BUFFER(1:BYTE-COUNT)
                CALL "SendPacket-Disconnect" USING CLIENT-ID CLIENT-STATE(CLIENT-ID) BUFFER BYTE-COUNT
                PERFORM DisconnectClient
                MOVE TEMP-INT64 TO CLIENT-ID
            END-IF

            *> Try to find an existing player for the UUID, or find a free slot to add a new player.
            CALL "Players-Connect" USING CLIENT-ID TEMP-UUID TEMP-PLAYER-NAME TEMP-PLAYER-NAME-LEN TEMP-INT8
            MOVE TEMP-INT8 TO CLIENT-PLAYER(CLIENT-ID)

            *> If no player slot was found, the server is full
            IF CLIENT-PLAYER(CLIENT-ID) = 0
                MOVE "The server is full" TO BUFFER
                MOVE 18 TO BYTE-COUNT
                DISPLAY "Disconnecting " TEMP-PLAYER-NAME(1:TEMP-PLAYER-NAME-LEN) ": " BUFFER(1:BYTE-COUNT)
                CALL "SendPacket-Disconnect" USING CLIENT-ID CLIENT-STATE(CLIENT-ID) BUFFER BYTE-COUNT
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF

            *> Send login success. This should result in a "login acknowledged" packet by the client.
            CALL "SendPacket-LoginSuccess" USING CLIENT-ID PLAYER-UUID(CLIENT-PLAYER(CLIENT-ID)) PLAYER-NAME(CLIENT-PLAYER(CLIENT-ID)) PLAYER-NAME-LENGTH(CLIENT-PLAYER(CLIENT-ID))

        *> Login acknowledge
        WHEN H'03'
            *> Must not happen before login start
            IF CLIENT-PLAYER(CLIENT-ID) = 0
                DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Client sent unexpected login acknowledge"
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF

            *> Can move to configuration state
            MOVE CLIENT-STATE-CONFIGURATION TO CLIENT-STATE(CLIENT-ID)

        WHEN OTHER
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet ID: " PACKET-ID
    END-EVALUATE

    EXIT SECTION.

HandleConfiguration SECTION.
    EVALUATE PACKET-ID
        *> Client information
        WHEN H'00'
            *> Note: payload of this packet is ignored for now

            *> Send brand
            MOVE "minecraft:brand" TO TEMP-IDENTIFIER
            MOVE 10 TO TEMP-INT32 *> length of the brand string
            MOVE 1 TO BYTE-COUNT
            CALL "Encode-VarInt" USING TEMP-INT32 BUFFER BYTE-COUNT
            MOVE "CobolCraft" TO BUFFER(BYTE-COUNT:TEMP-INT32)
            COMPUTE BYTE-COUNT = BYTE-COUNT + TEMP-INT32 - 1
            CALL "SendPacket-PluginMessage" USING CLIENT-ID CLIENT-STATE(CLIENT-ID) TEMP-IDENTIFIER BYTE-COUNT BUFFER

            *> Send configuration packets
            CALL "SendPacket-FeatureFlags" USING CLIENT-ID
            CALL "SendPacket-KnownPacks" USING CLIENT-ID
            CALL "Registries-GetCount" USING REGISTRY-COUNT
            PERFORM VARYING REGISTRY-INDEX FROM 1 BY 1 UNTIL REGISTRY-INDEX > REGISTRY-COUNT
                CALL "Registries-Iterate-ReqPacket" USING REGISTRY-INDEX TEMP-INT8
                IF TEMP-INT8 = 1
                    CALL "SendPacket-Registry" USING CLIENT-ID REGISTRY-INDEX
                END-IF
            END-PERFORM
            CALL "SendPacket-UpdateTags" USING CLIENT-ID

            *> Send finish configuration
            CALL "SendPacket-FinishConfiguration" USING CLIENT-ID

            *> We now expect an acknowledge packet
            MOVE 1 TO CONFIG-FINISH(CLIENT-ID)

        *> Serverbound plugin message
        WHEN H'02'
            *> Not implemented
            CONTINUE

        *> Acknowledge finish configuration
        WHEN H'03'
            IF CONFIG-FINISH(CLIENT-ID) = 0
                DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Client sent unexpected acknowledge finish configuration"
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF

            *> Can move to play state
            MOVE CLIENT-STATE-PLAY TO CLIENT-STATE(CLIENT-ID)

            *> send "Login (play)" with player index as entity ID
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            CALL "SendPacket-LoginPlay" USING CLIENT-ID TEMP-INT32 VIEW-DISTANCE PLAYER-GAMEMODE(TEMP-INT32) MAX-PLAYERS

            *> send world time
            CALL "World-GetAge" USING TEMP-INT64
            CALL "World-GetTime" USING TEMP-INT64-2
            CALL "SendPacket-UpdateTime" USING CLIENT-ID TEMP-INT64 TEMP-INT64-2

            *> send game event "start waiting for level chunks"
            MOVE 13 TO TEMP-INT8
            MOVE 0 TO TEMP-FLOAT
            CALL "SendPacket-GameEvent" USING CLIENT-ID TEMP-INT8 TEMP-FLOAT

            *> Note: The official server sends a lot of additional packets in this phase, but they seem to be optional.
            *> For example: set ticking state (rate=20, frozen=false); step tick (steps=0 [sic.])
            *> We will skip these for now.

            *> send inventory
            CALL "SendPacket-SetContainerContent" USING CLIENT-ID PLAYER-INVENTORY(CLIENT-PLAYER(CLIENT-ID))

            *> send selected hotbar slot
            CALL "SendPacket-SetHeldItem" USING CLIENT-ID PLAYER-HOTBAR(CLIENT-PLAYER(CLIENT-ID))

            *> send "Set Center Chunk"
            DIVIDE PLAYER-X(CLIENT-PLAYER(CLIENT-ID)) BY 16 GIVING CENTER-CHUNK-X(CLIENT-ID) ROUNDED MODE IS TOWARD-LESSER
            DIVIDE PLAYER-Z(CLIENT-PLAYER(CLIENT-ID)) BY 16 GIVING CENTER-CHUNK-Z(CLIENT-ID) ROUNDED MODE IS TOWARD-LESSER
            CALL "SendPacket-SetCenterChunk" USING CLIENT-ID CENTER-CHUNK-X(CLIENT-ID) CENTER-CHUNK-Z(CLIENT-ID)

            *> enqueue 3x3 chunks around the player first
            COMPUTE CHUNK-START-X = CENTER-CHUNK-X(CLIENT-ID) - 1
            COMPUTE CHUNK-END-X = CENTER-CHUNK-X(CLIENT-ID) + 1
            COMPUTE CHUNK-START-Z = CENTER-CHUNK-Z(CLIENT-ID) - 1
            COMPUTE CHUNK-END-Z = CENTER-CHUNK-Z(CLIENT-ID) + 1
            PERFORM VARYING CHUNK-X FROM CHUNK-START-X BY 1 UNTIL CHUNK-X > CHUNK-END-X
                PERFORM VARYING CHUNK-Z FROM CHUNK-START-Z BY 1 UNTIL CHUNK-Z > CHUNK-END-Z
                    PERFORM EnqueueChunk
                END-PERFORM
            END-PERFORM

            *> now enqueue all chunks in the view distance
            COMPUTE CHUNK-START-X = CENTER-CHUNK-X(CLIENT-ID) - VIEW-DISTANCE
            COMPUTE CHUNK-END-X = CENTER-CHUNK-X(CLIENT-ID) + VIEW-DISTANCE
            COMPUTE CHUNK-START-Z = CENTER-CHUNK-Z(CLIENT-ID) - VIEW-DISTANCE
            COMPUTE CHUNK-END-Z = CENTER-CHUNK-Z(CLIENT-ID) + VIEW-DISTANCE
            PERFORM VARYING CHUNK-X FROM CHUNK-START-X BY 1 UNTIL CHUNK-X > CHUNK-END-X
                PERFORM VARYING CHUNK-Z FROM CHUNK-START-Z BY 1 UNTIL CHUNK-Z > CHUNK-END-Z
                    *> Note: EnqueueChunk will automatically skip duplicates
                    PERFORM EnqueueChunk
                END-PERFORM
            END-PERFORM

            *> send the 3x3 chunks around the player immediately, the rest in the next ticks
            PERFORM 9 TIMES
                PERFORM ProcessChunkQueue
            END-PERFORM

            *> send the player list (including the new player) to the new player, and spawn player entities
            PERFORM VARYING TEMP-INT16 FROM 1 BY 1 UNTIL TEMP-INT16 > MAX-CLIENTS
                IF CLIENT-PRESENT(TEMP-INT16) = 1 AND CLIENT-STATE(TEMP-INT16) = CLIENT-STATE-PLAY
                    MOVE CLIENT-PLAYER(TEMP-INT16) TO TEMP-INT32
                    CALL "SendPacket-AddPlayer" USING CLIENT-ID PLAYER-UUID(TEMP-INT32) PLAYER-NAME(TEMP-INT32) PLAYER-NAME-LENGTH(TEMP-INT32)
                    IF TEMP-INT16 NOT = CLIENT-ID
                        CALL "SendPacket-SpawnEntity" USING CLIENT-ID TEMP-INT32 PLAYER-UUID(TEMP-INT32) PLAYER-POSITION(TEMP-INT32) PLAYER-ROTATION(TEMP-INT32)
                    END-IF
                END-IF
            END-PERFORM

            *> Send abilities (flying, etc.)
            CALL "SendPacket-PlayerAbilities" USING CLIENT-ID PLAYER-GAMEMODE(CLIENT-PLAYER(CLIENT-ID)) PLAYER-FLYING(CLIENT-PLAYER(CLIENT-ID))

            *> Send position ("Synchronize Player Position"). The client must confirm the teleportation.
            ADD 1 TO TELEPORT-SENT(CLIENT-ID)
            CALL "SendPacket-SetPlayerPosition" USING CLIENT-ID PLAYER-POSITION(CLIENT-PLAYER(CLIENT-ID)) PLAYER-ROTATION(CLIENT-PLAYER(CLIENT-ID)) TELEPORT-SENT(CLIENT-ID)

            *> Send available commands list
            CALL "SendPacket-Commands" USING CLIENT-ID

            *> Set op permission level to 4 (full permissions) - mainly so the gamemode switcher works
            *> TODO: implement a proper permission system
            MOVE 28 TO TEMP-INT8
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            CALL "SendPacket-EntityEvent" USING CLIENT-ID TEMP-INT32 TEMP-INT8

            *> send the new player to all other players
            MOVE CLIENT-ID TO TEMP-INT16
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY AND CLIENT-ID NOT = TEMP-INT16
                    *> add the new player to the player list
                    CALL "SendPacket-AddPlayer" USING CLIENT-ID PLAYER-UUID(TEMP-INT32) PLAYER-NAME(TEMP-INT32) PLAYER-NAME-LENGTH(TEMP-INT32)
                    *> spawn a player entity
                    CALL "SendPacket-SpawnEntity" USING CLIENT-ID TEMP-INT32 PLAYER-UUID(TEMP-INT32) PLAYER-POSITION(TEMP-INT32) PLAYER-ROTATION(TEMP-INT32)
                END-IF
            END-PERFORM
            MOVE TEMP-INT16 TO CLIENT-ID

            *> send "<username> joined the game" to all clients in play state, except the current client
            MOVE 0 TO BYTE-COUNT
            MOVE PLAYER-NAME(CLIENT-PLAYER(CLIENT-ID)) TO BUFFER
            ADD PLAYER-NAME-LENGTH(CLIENT-PLAYER(CLIENT-ID)) TO BYTE-COUNT
            MOVE " joined the game" TO BUFFER(BYTE-COUNT + 1:16)
            ADD 16 TO BYTE-COUNT
            CALL "BroadcastChatMessageExcept" USING CLIENT-ID BUFFER BYTE-COUNT C-COLOR-YELLOW

        WHEN H'07'
            *> Serverbound known packs
            CONTINUE

        WHEN OTHER
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet ID: " PACKET-ID
    END-EVALUATE.

    EXIT SECTION.

HandlePlay SECTION.
    EVALUATE PACKET-ID
        *> Confirm teleportation
        WHEN H'00'
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT32
            IF TEMP-INT32 > TELEPORT-RECV(CLIENT-ID) AND TEMP-INT32 <= TELEPORT-SENT(CLIENT-ID)
                MOVE TEMP-INT32 TO TELEPORT-RECV(CLIENT-ID)
            END-IF

        *> Chat command
        WHEN H'05'
            CALL "Decode-String" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION BYTE-COUNT BUFFER
            *> Command may not be longer than 256 characters
            IF BYTE-COUNT > 256
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF
            *> Handle the command
            CALL "HandleCommand" USING CLIENT-ID BUFFER BYTE-COUNT

        *> Chat message
        WHEN H'07'
            CALL "Decode-String" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION BYTE-COUNT BUFFER
            *> Message may not be longer than 256 characters
            IF BYTE-COUNT > 256
                PERFORM DisconnectClient
                EXIT SECTION
            END-IF
            *> display the message in the server console
            DISPLAY "<" PLAYER-NAME(CLIENT-PLAYER(CLIENT-ID))(1:PLAYER-NAME-LENGTH(CLIENT-PLAYER(CLIENT-ID))) "> " BUFFER(1:BYTE-COUNT)
            *> send the message to all clients in play state
            MOVE CLIENT-ID TO TEMP-INT16
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
                    CALL "SendPacket-PlayerChat" USING CLIENT-ID PLAYER-UUID(TEMP-INT32) PLAYER-NAME(TEMP-INT32) BUFFER BYTE-COUNT
                END-IF
            END-PERFORM
            MOVE TEMP-INT16 TO CLIENT-ID

        *> KeepAlive response
        WHEN H'1A'
            CALL "Decode-Long" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION KEEPALIVE-RECV(CLIENT-ID)

        *> Set player position
        WHEN H'1C'
            *> Ignore movement packets until the client acknowledges the last sent teleport packet
            IF TELEPORT-RECV(CLIENT-ID) NOT = TELEPORT-SENT(CLIENT-ID)
                EXIT SECTION
            END-IF
            CALL "Decode-Double" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-X(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Double" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-Y(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Double" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-Z(CLIENT-PLAYER(CLIENT-ID))
            *> TODO: "on ground" flag

        *> Set player position and rotation
        WHEN H'1D'
            IF TELEPORT-RECV(CLIENT-ID) NOT = TELEPORT-SENT(CLIENT-ID)
                EXIT SECTION
            END-IF
            CALL "Decode-Double" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-X(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Double" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-Y(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Double" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-Z(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Float" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-YAW(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Float" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-PITCH(CLIENT-PLAYER(CLIENT-ID))
            *> TODO: "on ground" flag

        *> Set player rotation
        WHEN H'1E'
            IF TELEPORT-RECV(CLIENT-ID) NOT = TELEPORT-SENT(CLIENT-ID)
                EXIT SECTION
            END-IF
            CALL "Decode-Float" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-YAW(CLIENT-PLAYER(CLIENT-ID))
            CALL "Decode-Float" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PLAYER-PITCH(CLIENT-PLAYER(CLIENT-ID))
            *> TODO: "on ground" flag

        *> Set player on ground
        WHEN H'1F'
            IF TELEPORT-RECV(CLIENT-ID) NOT = TELEPORT-SENT(CLIENT-ID)
                EXIT SECTION
            END-IF
            *> TODO: "on ground" flag

        *> Pick item from block
        WHEN H'22'
            *> location
            CALL "Decode-Position" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-POSITION
            *> TODO: "include data" flag
            ADD 1 TO PACKET-POSITION

            CALL "World-CheckBounds" USING TEMP-POSITION TEMP-INT8
            IF TEMP-INT8 NOT = 0
                EXIT SECTION
            END-IF

            *> Find the block's item handler
            CALL "World-GetBlock" USING TEMP-POSITION TEMP-INT32
            CALL "GetCallback-BlockItem" USING TEMP-INT32 CALLBACK-PTR-BLOCK
            IF CALLBACK-PTR-BLOCK = NULL
                EXIT SECTION
            END-IF

            CALL CALLBACK-PTR-BLOCK USING TEMP-INT32 TEMP-IDENTIFIER
            IF TEMP-IDENTIFIER = SPACES
                EXIT SECTION
            END-IF

            CALL "Registries-Get-EntryId" USING C-MINECRAFT-ITEM TEMP-IDENTIFIER TEMP-INT32
            IF TEMP-INT32 > 0
                CALL "Inventory-PickItem" USING PLAYER-HOTBAR(CLIENT-PLAYER(CLIENT-ID)) PLAYER-INVENTORY(CLIENT-PLAYER(CLIENT-ID)) TEMP-INT32
                CALL "SendPacket-SetHeldItem" USING CLIENT-ID PLAYER-HOTBAR(CLIENT-PLAYER(CLIENT-ID))
                CALL "SendPacket-SetContainerContent" USING CLIENT-ID PLAYER-INVENTORY(CLIENT-PLAYER(CLIENT-ID))
            END-IF

        *> Player abilities
        WHEN H'26'
            CALL "Decode-Byte" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT8
            *> flying = bitmask & 0x02
            COMPUTE TEMP-INT8 = TEMP-INT8 / 2
            COMPUTE PLAYER-FLYING(CLIENT-PLAYER(CLIENT-ID)) = FUNCTION MOD(TEMP-INT8, 2)

        *> Player action
        WHEN H'27'
            *> Status (= the action), block position, face, sequence number.
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT32
            CALL "Decode-Position" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-POSITION
            EVALUATE TRUE
                *> started digging
                WHEN TEMP-INT32 = 0
                    *> face (ignored)
                    CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-BLOCK-FACE
                    *> sequence ID
                    CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION SEQUENCE-ID

                    *> Check the position validity
                    CALL "World-CheckBounds" USING TEMP-POSITION TEMP-INT8
                    IF TEMP-INT8 = 0
                        *> Call the block's destroy handler
                        CALL "World-GetBlock" USING TEMP-POSITION TEMP-INT32
                        CALL "GetCallback-BlockDestroy" USING TEMP-INT32 CALLBACK-PTR-BLOCK
                        IF CALLBACK-PTR-BLOCK NOT = NULL
                            CALL CALLBACK-PTR-BLOCK USING CLIENT-PLAYER(CLIENT-ID) TEMP-POSITION TEMP-BLOCK-FACE
                        END-IF
                    END-IF

                    *> Acknowledge the action
                    CALL "SendPacket-AckBlockChange" USING CLIENT-ID SEQUENCE-ID
            END-EVALUATE

        *> Player command
        WHEN H'28'
            *> entity ID (why does this exist? should always be the player's entity ID - skip it)
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT32
            *> action ID
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT32
            EVALUATE TEMP-INT32
                *> start sneaking
                WHEN 0
                    MOVE 1 TO PLAYER-SNEAKING(CLIENT-PLAYER(CLIENT-ID))
                *> stop sneaking
                WHEN 1
                    MOVE 0 TO PLAYER-SNEAKING(CLIENT-PLAYER(CLIENT-ID))
            END-EVALUATE

        *> Set held item
        WHEN H'33'
            CALL "Decode-Short" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT16
            IF TEMP-INT16 >= 0 AND TEMP-INT16 <= 8
                MOVE TEMP-INT16 TO PLAYER-HOTBAR(CLIENT-PLAYER(CLIENT-ID))
            END-IF

        *> Set creative mode slot
        WHEN H'36'
            *> slot ID
            CALL "Decode-Short" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT16
            *> TODO: spawn item entity when slot ID is -1
            *> slot description (count (byte) [, item ID (VarInt), NBT data])
            CALL "Decode-Byte" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT8
            IF TEMP-INT16 >= 0 AND TEMP-INT16 < 46
                MOVE TEMP-INT8 TO PLAYER-INVENTORY-SLOT-COUNT(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                IF TEMP-INT8 = 0
                    MOVE 0 TO PLAYER-INVENTORY-SLOT-ID(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                ELSE
                    CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT32
                    MOVE TEMP-INT32 TO PLAYER-INVENTORY-SLOT-ID(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                    *> remainder is NBT
                    COMPUTE BYTE-COUNT = PACKET-LENGTH(CLIENT-ID) - PACKET-POSITION + 1
                    IF BYTE-COUNT <= 1024
                        MOVE BYTE-COUNT TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                        MOVE CLIENT-RECEIVE-BUFFER(PACKET-POSITION:BYTE-COUNT) TO PLAYER-INVENTORY-SLOT-NBT-DATA(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)(1:BYTE-COUNT)
                    ELSE
                        MOVE 0 TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1)
                        DISPLAY "Item NBT data too long: " BYTE-COUNT
                    END-IF
                END-IF
            END-IF

        *> Swing arm
        WHEN H'3A'
            *> hand enum: 0=main hand, 1=off hand
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT32
            IF TEMP-INT32 = 1
                MOVE 3 TO TEMP-INT8
            ELSE
                MOVE 0 TO TEMP-INT8
            END-IF
            *> send an animation packet to each of the other players
            MOVE CLIENT-ID TO TEMP-INT16
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
                IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY AND CLIENT-ID NOT = TEMP-INT16
                    CALL "SendPacket-EntityAnimation" USING CLIENT-ID TEMP-INT32 TEMP-INT8
                END-IF
            END-PERFORM
            MOVE TEMP-INT16 TO CLIENT-ID

        *> Use item on block
        WHEN H'3C'
            *> hand enum: 0=main hand, 1=off hand
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT32
            IF TEMP-INT32 = 0
                *> compute the inventory slot
                COMPUTE TEMP-INT16 = 36 + PLAYER-HOTBAR(CLIENT-PLAYER(CLIENT-ID))
            ELSE
                MOVE 45 TO TEMP-INT16
            END-IF

            *> block position
            CALL "Decode-Position" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-POSITION

            *> face enum (0-5): -Y, +Y, -Z, +Z, -X, +X
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-BLOCK-FACE
            CALL "Decode-Float" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-CURSOR-X
            CALL "Decode-Float" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-CURSOR-Y
            CALL "Decode-Float" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-CURSOR-Z

            *> TODO: "inside block" flag
            ADD 1 TO PACKET-POSITION

            *> sequence ID
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION SEQUENCE-ID

            *> Determine the item in the inventory slot
            MOVE PLAYER-INVENTORY-SLOT-ID(CLIENT-PLAYER(CLIENT-ID), TEMP-INT16 + 1) TO TEMP-INT32
            CALL "Registries-Get-EntryName" USING C-MINECRAFT-ITEM TEMP-INT32 TEMP-IDENTIFIER
            CALL "GetCallback-ItemUse" USING TEMP-IDENTIFIER CALLBACK-PTR-ITEM

            *> Determine the current block's "interact" callback
            CALL "World-GetBlock" USING TEMP-POSITION TEMP-INT32
            CALL "GetCallback-BlockInteract" USING TEMP-INT32 CALLBACK-PTR-BLOCK

            *> If the player is sneaking, we should execute the item's "use" callback instead of the block's
            *> "interact" callback - unless the item has no "use" callback.
            EVALUATE TRUE
                WHEN CALLBACK-PTR-ITEM NOT = NULL AND (CALLBACK-PTR-BLOCK = NULL OR PLAYER-SNEAKING(CLIENT-PLAYER(CLIENT-ID)) NOT = 0)
                    CALL CALLBACK-PTR-ITEM USING CLIENT-PLAYER(CLIENT-ID) TEMP-IDENTIFIER TEMP-POSITION TEMP-BLOCK-FACE TEMP-CURSOR
                WHEN CALLBACK-PTR-BLOCK NOT = NULL
                    CALL CALLBACK-PTR-BLOCK USING CLIENT-PLAYER(CLIENT-ID) TEMP-IDENTIFIER TEMP-POSITION TEMP-BLOCK-FACE TEMP-CURSOR
            END-EVALUATE

            *> Acknowledge the action
            CALL "SendPacket-AckBlockChange" USING CLIENT-ID SEQUENCE-ID

        *> Use item
        WHEN H'3D'
            *> hand enum: 0=main hand, 1=off hand
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT32
            IF TEMP-INT32 = 0
                *> compute the inventory slot
                COMPUTE TEMP-INT16 = 36 + PLAYER-HOTBAR(CLIENT-PLAYER(CLIENT-ID))
            ELSE
                MOVE 45 TO TEMP-INT16
            END-IF

            *> sequence ID
            CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION SEQUENCE-ID

            *> TODO: Buckets send this packet when clicking a liquid directly - handle this case
            *> TODO food items
            *> TODO potions
            *> TODO splash potions
            *> TODO lingering potions
            *> TODO bows
            *> TODO shields
            *> TODO bottle o' enchanting
            *> TODO eggs, snowballs, ender pearls, etc.

            *> Acknowledge the action
            CALL "SendPacket-AckBlockChange" USING CLIENT-ID SEQUENCE-ID
    END-EVALUATE

    EXIT SECTION.

DisconnectClient SECTION.
    *> Disconnect the current client.
    CALL "Server-DisconnectClient" USING CLIENT-ID
    EXIT SECTION.

HandleServerError SECTION.
    IF ERRNO NOT = 0
        DISPLAY "Server socket error: " ERRNO
        STOP RUN RETURNING 1
    END-IF

    EXIT SECTION.

HandleClientError SECTION.
    IF ERRNO NOT = 0
        CALL "Server-ClientError" USING CLIENT-ID ERRNO
    END-IF

    EXIT SECTION.

END PROGRAM Server.

*> --- Server-DisconnectClient ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Server-DisconnectClient.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> constants
    01 C-COLOR-YELLOW           PIC X(16)                   VALUE "yellow".
    *> shared data
    COPY DD-CLIENT-STATES.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    COPY DD-SERVER-PROPERTIES.
    *> temporary data
    01 ERRNO                    PIC 9(3).
    01 OTHER-CLIENT-ID          BINARY-LONG UNSIGNED.
    01 PLAYER-ID                BINARY-LONG UNSIGNED.
    01 BUFFER                   PIC X(256).
    01 BYTE-COUNT               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT-ID             BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT-ID.
    IF CLIENT-PRESENT(LK-CLIENT-ID) = 0
        EXIT SECTION
    END-IF

    MOVE 0 TO CLIENT-PRESENT(LK-CLIENT-ID)

    CALL "SocketClose" USING CLIENT-HNDL(LK-CLIENT-ID) GIVING ERRNO
    IF ERRNO NOT = 0
        DISPLAY "Error closing client socket: " ERRNO
    END-IF

    *> If the client was playing, send a leave message to all other clients, and remove the player from their world
    IF CLIENT-STATE(LK-CLIENT-ID) = CLIENT-STATE-PLAY
        *> send "<username> left the game" to all clients in play state, except the current client
        MOVE 0 TO BYTE-COUNT
        MOVE PLAYER-NAME(CLIENT-PLAYER(LK-CLIENT-ID)) TO BUFFER
        ADD PLAYER-NAME-LENGTH(CLIENT-PLAYER(LK-CLIENT-ID)) TO BYTE-COUNT
        MOVE " left the game" TO BUFFER(BYTE-COUNT + 1:14)
        ADD 14 TO BYTE-COUNT
        CALL "BroadcastChatMessageExcept" USING LK-CLIENT-ID BUFFER BYTE-COUNT C-COLOR-YELLOW

        *> remove the player from the player list, and despawn the player entity
        MOVE CLIENT-PLAYER(LK-CLIENT-ID) TO PLAYER-ID
        PERFORM VARYING OTHER-CLIENT-ID FROM 1 BY 1 UNTIL OTHER-CLIENT-ID > MAX-CLIENTS
            IF CLIENT-PRESENT(OTHER-CLIENT-ID) = 1 AND CLIENT-STATE(OTHER-CLIENT-ID) = CLIENT-STATE-PLAY AND OTHER-CLIENT-ID NOT = LK-CLIENT-ID
                CALL "SendPacket-RemovePlayer" USING OTHER-CLIENT-ID PLAYER-UUID(PLAYER-ID)
                CALL "SendPacket-RemoveEntity" USING OTHER-CLIENT-ID PLAYER-ID
            END-IF
        END-PERFORM
    END-IF

    MOVE X"00000000" TO CLIENT-HNDL(LK-CLIENT-ID)
    MOVE CLIENT-STATE-DISCONNECTED TO CLIENT-STATE(LK-CLIENT-ID)
    MOVE 0 TO CONFIG-FINISH(LK-CLIENT-ID)

    FREE PACKET-BUFFER(LK-CLIENT-ID)

    *> If there is an associated player, remove the association
    IF CLIENT-PLAYER(LK-CLIENT-ID) > 0
        CALL "Players-Disconnect" USING CLIENT-PLAYER(LK-CLIENT-ID)
        MOVE 0 TO CLIENT-PLAYER(LK-CLIENT-ID)
    END-IF

    GOBACK.

END PROGRAM Server-DisconnectClient.

*> --- Server-ClientError ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Server-ClientError.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
LINKAGE SECTION.
    01 LK-CLIENT-ID             BINARY-LONG UNSIGNED.
    01 LK-ERRNO                 PIC 9(3).

PROCEDURE DIVISION USING LK-CLIENT-ID LK-ERRNO.
    DISPLAY "[client=" LK-CLIENT-ID  ", state=" CLIENT-STATE(LK-CLIENT-ID) "] Socket error: " LK-ERRNO
    CALL "Server-DisconnectClient" USING LK-CLIENT-ID

    GOBACK.

END PROGRAM Server-ClientError.

*> --- Server-Save ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Server-Save.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 SAVE-FAILURE             BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION.
    *> save chunks
    CALL "World-Save" USING SAVE-FAILURE
    IF SAVE-FAILURE NOT = 0
        DISPLAY "Failed to save world"
        STOP RUN RETURNING 1
    END-IF
    *> save player data
    CALL "Players-Save"
    GOBACK.

END PROGRAM Server-Save.

*> --- Server-Stop ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Server-Stop.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ERRNO                    PIC 9(3).
    01 CLIENT-ID                BINARY-LONG UNSIGNED.
    01 BUFFER                   PIC X(256).
    01 BYTE-COUNT               BINARY-LONG UNSIGNED.
    01 FAILURE                  BINARY-CHAR UNSIGNED.
    *> shared state with Server
    01 SERVER-HNDL              PIC X(4)                EXTERNAL.
    COPY DD-CLIENTS.
    COPY DD-SERVER-PROPERTIES.

PROCEDURE DIVISION.
    CALL "Server-Save"

    DISPLAY "Stopping server"

    *> Close all region files
    CALL "Region-CloseAll" USING FAILURE
    IF FAILURE NOT = 0
        DISPLAY "Error while closing region files"
    END-IF

    *> Send a message to all clients
    MOVE "Server closed" TO BUFFER
    MOVE 13 TO BYTE-COUNT
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 1
            CALL "SendPacket-Disconnect" USING CLIENT-ID CLIENT-STATE(CLIENT-ID) BUFFER BYTE-COUNT
        END-IF
    END-PERFORM

    CALL "SocketClose" USING SERVER-HNDL GIVING ERRNO
    IF ERRNO NOT = 0
        DISPLAY "Error closing server socket: " ERRNO
    END-IF

    STOP RUN.

END PROGRAM Server-Stop.
