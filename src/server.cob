*> --- Server ---
*> This is the entrypoint for starting a CobolCraft server.
IDENTIFICATION DIVISION.
PROGRAM-ID. Server.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> File names
    01 FILE-REGISTRIES              PIC X(255)              VALUE "data/generated/reports/registries.json".
    01 FILE-BLOCKS                  PIC X(255)              VALUE "data/generated/reports/blocks.json".
    01 FILE-PACKETS                 PIC X(255)              VALUE "data/generated/reports/packets.json".
    01 FILE-DATAPACK-ROOT           PIC X(255)              VALUE "data/generated/data/".
    *> Constants
    COPY DD-CLIENT-STATES.
    COPY DD-PACKET-DIRECTIONS.
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
    01 C-MINECRAFT-INSTRUMENT       PIC X(50)               VALUE "minecraft:instrument".
    01 C-COLOR-YELLOW               PIC X(16)               VALUE "yellow".
    *> Configuration
    COPY DD-SERVER-PROPERTIES.
    *> The amount of microseconds between autosaves, and the last autosave timestamp.
    01 AUTOSAVE-INTERVAL            BINARY-LONG-LONG        VALUE 300000000.
    01 LAST-AUTOSAVE                BINARY-LONG-LONG.
    *> Microsecond intervals
    01 GAME-TICK-INTERVAL           BINARY-LONG-LONG        VALUE 50000.    *> 1/20th of a second
    01 KEEPALIVE-SEND-INTERVAL      BINARY-LONG-LONG        VALUE 1000000.  *> 1s
    01 KEEPALIVE-RECV-TIMEOUT       BINARY-LONG-LONG        VALUE 15000000. *> 15s
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
    01 PACKET-STATE                 BINARY-CHAR.
    01 PACKET-ID                    BINARY-LONG.
    01 PACKET-NAME                  PIC X(128).
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
    01 TEMP-UUID                    PIC X(16).
    01 TEMP-PLAYER-NAME             PIC X(16).
    01 TEMP-IDENTIFIER              PIC X(100).
    *> Time measurement (microseconds)
    78 NANOSECOND-SCALE             VALUE 1000.
    01 CURRENT-TIME                 BINARY-LONG-LONG.
    01 TICK-STARTTIME               BINARY-LONG-LONG.
    01 TICK-ENDTIME                 BINARY-LONG-LONG.
    01 TICK-MAIN-DURATION           BINARY-LONG-LONG.
    01 TICK-SLEEP-DURATION          BINARY-LONG-LONG.
    *> Debug samples
    01 DEBUG-SUBSCRIPTION-VALIDITY  BINARY-LONG-LONG        VALUE 10000000. *> 10s
    01 DEBUG-SAMPLE.
        02 DEBUG-SAMPLE-FULL        BINARY-LONG-LONG.
        02 DEBUG-SAMPLE-MAIN        BINARY-LONG-LONG.
        02 DEBUG-SAMPLE-TASKS       BINARY-LONG-LONG.
        02 DEBUG-SAMPLE-IDLE        BINARY-LONG-LONG.
    *> Variables used for datapack loading
    01 VANILLA-DATAPACK-NAME        PIC X(50)               VALUE "minecraft".
    01 REGISTRY-COUNT               BINARY-LONG UNSIGNED.
    *> Variables used for item registration
    01 REGISTRY-INDEX               BINARY-LONG.
    01 REGISTRY-LENGTH              BINARY-LONG UNSIGNED.
    01 REGISTRY-ENTRY-INDEX         BINARY-LONG UNSIGNED.
    01 REGISTRY-ENTRY-NAME          PIC X(100).

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
    CALL "Registries-Create" USING C-MINECRAFT-INSTRUMENT TEMP-INT8
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

LoadPackets.
    DISPLAY "Loading packets"
    CALL "Files-ReadAll" USING FILE-PACKETS DATA-BUFFER DATA-BUFFER-LEN DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to read: " FUNCTION TRIM(FILE-PACKETS)
        STOP RUN RETURNING 1
    END-IF
    CALL "Packets-Parse" USING DATA-BUFFER DATA-BUFFER-LEN DATA-FAILURE
    IF DATA-FAILURE NOT = 0
        DISPLAY "Failed to parse packets"
        STOP RUN RETURNING 1
    END-IF
    .

RegisterPacketHandlers.
    DISPLAY "Registering packet handlers"

    CALL "InitializePacketHandlers"

    MOVE CLIENT-STATE-PLAY TO PACKET-STATE
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:accept_teleportation"         "RecvPacket-AcceptTeleport"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:chat_command"                 "RecvPacket-ChatCommand"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:chat"                         "RecvPacket-Chat"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:client_command"               "RecvPacket-ClientCommand"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:container_click"              "RecvPacket-ContainerClick"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:container_close"              "RecvPacket-ContainerClose"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:debug_sample_subscription"    "RecvPacket-DebugSubscription"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:keep_alive"                   "RecvPacket-KeepAlive"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:move_player_pos"              "RecvPacket-MovePlayerPos"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:move_player_pos_rot"          "RecvPacket-MovePlayerPosRot"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:move_player_rot"              "RecvPacket-MovePlayerRot"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:move_player_status_only"      "RecvPacket-MovePlayerStatus"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:pick_item_from_block"         "RecvPacket-PickBlock"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:player_abilities"             "RecvPacket-PlayerAbilities"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:player_action"                "RecvPacket-PlayerAction"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:player_command"               "RecvPacket-PlayerCommand"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:set_carried_item"             "RecvPacket-SetCarriedItem"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:set_creative_mode_slot"       "RecvPacket-SetCreativeSlot"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:swing"                        "RecvPacket-Swing"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:use_item_on"                  "RecvPacket-UseItemOn"
    CALL "RegisterPacketHandler" USING PACKET-STATE "minecraft:use_item"                     "RecvPacket-UseItem"
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
    CALL "RegisterBlock-Generic"

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
    CALL "RegisterBlock-CraftingTable"
    .

RegisterCommands.
    DISPLAY "Registering commands"

    CALL "Commands-Init"

    CALL "RegisterCommand-GameMode"
    CALL "RegisterCommand-Help"
    CALL "RegisterCommand-Kill"
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
    CALL "SystemTimeMicros" USING LAST-AUTOSAVE
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
        CALL "SystemTimeMicros" USING CURRENT-TIME
        MOVE CURRENT-TIME TO TICK-STARTTIME
        COMPUTE TICK-ENDTIME = TICK-STARTTIME + GAME-TICK-INTERVAL

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
                        IF PLAYER-BLOCK-BREAKING-STAGE(TEMP-INT32) >= 0
                            CALL "SendPacket-BlockDestruction" USING CLIENT-ID TEMP-INT32 PLAYER-BLOCK-BREAKING-POSITION(TEMP-INT32) PLAYER-BLOCK-BREAKING-STAGE(TEMP-INT32)
                        END-IF
                        *> index(byte), type(VarInt), value(VarInt)
                        *> index of pose: 6, type of pose: 21
                        *> value of standing: 0, value of sneaking: 5
                        MOVE X"06" TO BUFFER(1:1)
                        MOVE X"15" TO BUFFER(2:1)
                        IF PLAYER-SNEAKING(TEMP-INT32) = 1
                            MOVE X"05" TO BUFFER(3:1)
                        ELSE
                            MOVE X"00" TO BUFFER(3:1)
                        END-IF
                        *> index of health: 9, type of health: float(3)
                        MOVE X"09" TO BUFFER(4:1)
                        MOVE X"03" TO BUFFER(5:1)
                        MOVE 6 TO BYTE-COUNT
                        CALL "Encode-Float" USING PLAYER-HEALTH(TEMP-INT32) BUFFER BYTE-COUNT
                        *> terminator is 0xFF
                        MOVE X"FF" TO BUFFER(10:1)
                        MOVE 10 TO BYTE-COUNT
                        CALL "SendPacket-SetEntityMetadata" USING CLIENT-ID TEMP-INT32 BYTE-COUNT BUFFER
                        *> main hand item
                        MOVE 0 TO TEMP-INT8
                        COMPUTE TEMP-INT64 = 36 + PLAYER-HOTBAR(TEMP-INT32) + 1
                        CALL "SendPacket-SetEquipment" USING CLIENT-ID TEMP-INT32 TEMP-INT8 PLAYER-INVENTORY-SLOT(TEMP-INT32, TEMP-INT64)
                        *> offhand item
                        MOVE 1 TO TEMP-INT8
                        CALL "SendPacket-SetEquipment" USING CLIENT-ID TEMP-INT32 TEMP-INT8 PLAYER-INVENTORY-SLOT(TEMP-INT32, 45 + 1)
                        *> boots
                        MOVE 2 TO TEMP-INT8
                        CALL "SendPacket-SetEquipment" USING CLIENT-ID TEMP-INT32 TEMP-INT8 PLAYER-INVENTORY-SLOT(TEMP-INT32, 8 + 1)
                        *> leggings
                        MOVE 3 TO TEMP-INT8
                        CALL "SendPacket-SetEquipment" USING CLIENT-ID TEMP-INT32 TEMP-INT8 PLAYER-INVENTORY-SLOT(TEMP-INT32, 7 + 1)
                        *> chestplate
                        MOVE 4 TO TEMP-INT8
                        CALL "SendPacket-SetEquipment" USING CLIENT-ID TEMP-INT32 TEMP-INT8 PLAYER-INVENTORY-SLOT(TEMP-INT32, 6 + 1)
                        *> helmet
                        MOVE 5 TO TEMP-INT8
                        CALL "SendPacket-SetEquipment" USING CLIENT-ID TEMP-INT32 TEMP-INT8 PLAYER-INVENTORY-SLOT(TEMP-INT32, 5 + 1)
                    END-IF
                END-PERFORM
            END-IF
        END-PERFORM

        *> update chunks around players
        CALL "ProcessClientChunks"

        *> Send debug sample for the past tick
        PERFORM SendDebugSamples

        CALL "SystemTimeMicros" USING CURRENT-TIME
        COMPUTE TICK-MAIN-DURATION = CURRENT-TIME - TICK-STARTTIME

        *> Read console command
        PERFORM ConsoleInput

        *> The remaining time of this tick can be used for accepting connections and receiving packets.
        CALL "SystemTimeMicros" USING CURRENT-TIME
        MOVE 0 TO TICK-SLEEP-DURATION
        PERFORM UNTIL CURRENT-TIME >= TICK-ENDTIME
            PERFORM NetworkRead
            CALL "SystemTimeMicros" USING CURRENT-TIME
        END-PERFORM

        *> debug samples are measured in nanoseconds
        COMPUTE DEBUG-SAMPLE-FULL = (CURRENT-TIME - TICK-STARTTIME) * NANOSECOND-SCALE
        COMPUTE DEBUG-SAMPLE-MAIN = TICK-MAIN-DURATION * NANOSECOND-SCALE
        COMPUTE DEBUG-SAMPLE-IDLE = TICK-SLEEP-DURATION * NANOSECOND-SCALE
        COMPUTE DEBUG-SAMPLE-TASKS = DEBUG-SAMPLE-FULL - DEBUG-SAMPLE-MAIN - DEBUG-SAMPLE-IDLE

        MOVE X"00000000" TO TEMP-HNDL
        MOVE 0 TO CLIENT-ID
    END-PERFORM
    .

GameLoop.
    *> Update the world age
    CALL "World-UpdateAge"

    EXIT PARAGRAPH.

ConsoleInput.
    *> Read from the console (configured as non-blocking). Note that this will only return full lines.
    MOVE LENGTH OF BUFFER TO BYTE-COUNT
    CALL "ReadConsole" USING BUFFER BYTE-COUNT
    IF BYTE-COUNT > 0
        *> client id = 0 means the console
        MOVE 0 TO TEMP-INT32
        CALL "HandleCommand" USING TEMP-INT32 BUFFER BYTE-COUNT
    END-IF
    EXIT PARAGRAPH.

NetworkRead.
    CALL "SocketPoll" USING SERVER-HNDL TEMP-HNDL GIVING ERRNO
    IF ERRNO NOT = 0
        PERFORM HandleServerError
        EXIT PARAGRAPH
    END-IF
    *> Without anything to do, sleep up to 1ms to avoid busy-waiting
    IF TEMP-HNDL = X"00000000"
        MOVE FUNCTION MIN(1000, FUNCTION MAX(1, TICK-ENDTIME - CURRENT-TIME)) TO TEMP-INT64
        ADD TEMP-INT64 TO TICK-SLEEP-DURATION
        COMPUTE TEMP-INT64 = TEMP-INT64 * NANOSECOND-SCALE
        CALL "CBL_GC_NANOSLEEP" USING TEMP-INT64
        EXIT PARAGRAPH
    END-IF

    *> Find an existing client to which the handle belongs
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-HNDL(CLIENT-ID) = TEMP-HNDL
            PERFORM ReceivePacket
            EXIT PARAGRAPH
        END-IF
    END-PERFORM

    *> If no existing client was found, find a free slot for a new client
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 0
            PERFORM InsertClient
            PERFORM ReceivePacket
            EXIT PARAGRAPH
        END-IF
    END-PERFORM

    *> If no free slot was found, close the connection
    DISPLAY "Cannot accept new connection: no free slots"
    CALL "SocketClose" USING TEMP-HNDL GIVING ERRNO
    IF ERRNO NOT = 0
        PERFORM HandleServerError
    END-IF

    EXIT PARAGRAPH.

InsertClient.
    INITIALIZE CLIENT(CLIENT-ID)

    MOVE 1 TO CLIENT-PRESENT(CLIENT-ID)
    MOVE TEMP-HNDL TO CLIENT-HNDL(CLIENT-ID)
    MOVE CLIENT-STATE-HANDSHAKE TO CLIENT-STATE(CLIENT-ID)

    ALLOCATE RECEIVE-BUFFER-LENGTH CHARACTERS RETURNING PACKET-BUFFER(CLIENT-ID)

    EXIT PARAGRAPH.

KeepAlive.
    *> Give the client some time for keepalive when the connection is established
    IF KEEPALIVE-RECV(CLIENT-ID) = 0
        MOVE CURRENT-TIME TO KEEPALIVE-RECV(CLIENT-ID)
    END-IF

    *> If the client has not responded to keepalive within 15 seconds, disconnect
    COMPUTE TEMP-INT64 = CURRENT-TIME - KEEPALIVE-RECV(CLIENT-ID)
    IF TEMP-INT64 >= KEEPALIVE-RECV-TIMEOUT
        DISPLAY "[client=" CLIENT-ID "] Timeout"
        PERFORM DisconnectClient
        EXIT PARAGRAPH
    END-IF

    *> Send keepalive packet every second, but only in play state
    COMPUTE TEMP-INT64 = CURRENT-TIME - KEEPALIVE-SENT(CLIENT-ID)
    IF CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY AND TEMP-INT64 >= KEEPALIVE-SEND-INTERVAL
        MOVE CURRENT-TIME TO KEEPALIVE-SENT(CLIENT-ID)
        CALL "SendPacket-KeepAlive" USING CLIENT-ID KEEPALIVE-SENT(CLIENT-ID)
    END-IF

    EXIT PARAGRAPH.

SendDebugSamples.
    COMPUTE TEMP-INT64 = TICK-STARTTIME - DEBUG-SUBSCRIPTION-VALIDITY
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) NOT = 0 AND DEBUG-SUBSCRIBE-TIME(CLIENT-ID) > TEMP-INT64
            CALL "SendPacket-DebugSample" USING CLIENT-ID DEBUG-SAMPLE
        END-IF
    END-PERFORM
    EXIT PARAGRAPH.

ReceivePacket.
    *> Ignore any attempts to receive data for clients that are not in a valid state
    IF CLIENT-STATE(CLIENT-ID) < 0
        EXIT PARAGRAPH
    END-IF

    *> Select the current client's buffer
    SET ADDRESS OF CLIENT-RECEIVE-BUFFER TO PACKET-BUFFER(CLIENT-ID)

    *> Receive the packet length. This is a VarInt, so it has to be done byte by byte (up to 5 bytes).
    PERFORM UNTIL PACKET-LENGTH(CLIENT-ID) > 0
        MOVE 1 TO BYTE-COUNT
        CALL "SocketRead" USING CLIENT-HNDL(CLIENT-ID) BYTE-COUNT BUFFER GIVING ERRNO
        IF ERRNO NOT = 0
            PERFORM HandleClientError
            EXIT PARAGRAPH
        END-IF
        IF BYTE-COUNT = 0
            *> No data read, try again later
            EXIT PARAGRAPH
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
                EXIT PARAGRAPH
            END-IF
            MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)
            EXIT PERFORM
        END-IF

        IF PACKET-BUFFERLEN(CLIENT-ID) >= 5
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Received invalid packet length."
            PERFORM DisconnectClient
            EXIT PARAGRAPH
        END-IF
    END-PERFORM

    *> Receive the packet ID and payload
    PERFORM UNTIL PACKET-BUFFERLEN(CLIENT-ID) >= PACKET-LENGTH(CLIENT-ID)
        *> The socket library can only read up to 64 kB at a time.
        MOVE FUNCTION MIN(PACKET-LENGTH(CLIENT-ID) - PACKET-BUFFERLEN(CLIENT-ID), 64000) TO BYTE-COUNT-EXPECTED BYTE-COUNT
        CALL "SocketRead" USING CLIENT-HNDL(CLIENT-ID) BYTE-COUNT CLIENT-RECEIVE-BUFFER(PACKET-BUFFERLEN(CLIENT-ID) + 1:) GIVING ERRNO
        IF ERRNO NOT = 0
            PERFORM HandleClientError
            EXIT PARAGRAPH
        END-IF
        ADD BYTE-COUNT TO PACKET-BUFFERLEN(CLIENT-ID)
        IF BYTE-COUNT < BYTE-COUNT-EXPECTED
            *> Not enough data read, try again later
            EXIT PARAGRAPH
        END-IF
    END-PERFORM

    *> Packet received, process it
    MOVE 1 TO PACKET-POSITION
    CALL "Decode-VarInt" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION PACKET-ID

    EVALUATE CLIENT-STATE(CLIENT-ID)
        WHEN CLIENT-STATE-HANDSHAKE
            CALL "Packets-GetReference" USING CLIENT-STATE(CLIENT-ID) PACKET-DIRECTION-SERVERBOUND PACKET-ID PACKET-NAME
            PERFORM HandleHandshake
        WHEN CLIENT-STATE-STATUS
            CALL "Packets-GetReference" USING CLIENT-STATE(CLIENT-ID) PACKET-DIRECTION-SERVERBOUND PACKET-ID PACKET-NAME
            PERFORM HandleStatus
        WHEN CLIENT-STATE-LOGIN
            CALL "Packets-GetReference" USING CLIENT-STATE(CLIENT-ID) PACKET-DIRECTION-SERVERBOUND PACKET-ID PACKET-NAME
            PERFORM HandleLogin
        WHEN CLIENT-STATE-CONFIGURATION
            CALL "Packets-GetReference" USING CLIENT-STATE(CLIENT-ID) PACKET-DIRECTION-SERVERBOUND PACKET-ID PACKET-NAME
            PERFORM HandleConfiguration
        WHEN OTHER
            CALL "HandlePacket" USING CLIENT-ID CLIENT-STATE(CLIENT-ID) PACKET-ID CLIENT-RECEIVE-BUFFER PACKET-POSITION
    END-EVALUATE

    *> Reset length for the next packet
    MOVE 0 TO PACKET-LENGTH(CLIENT-ID)
    MOVE 0 TO PACKET-BUFFERLEN(CLIENT-ID)

    EXIT PARAGRAPH.

HandleHandshake.
    IF PACKET-NAME NOT = "minecraft:intention"
        DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet: " FUNCTION TRIM(PACKET-NAME)
        PERFORM DisconnectClient
        EXIT PARAGRAPH
    END-IF

    *> The final byte of the payload encodes the target state.
    COMPUTE CLIENT-STATE(CLIENT-ID) = FUNCTION ORD(CLIENT-RECEIVE-BUFFER(PACKET-LENGTH(CLIENT-ID):1)) - 1
    IF CLIENT-STATE(CLIENT-ID) NOT = CLIENT-STATE-STATUS AND CLIENT-STATE(CLIENT-ID) NOT = CLIENT-STATE-LOGIN
        DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Client requested invalid target state: " CLIENT-STATE(CLIENT-ID)
        PERFORM DisconnectClient
        EXIT PARAGRAPH
    END-IF

    EXIT PARAGRAPH.

HandleStatus.
    EVALUATE PACKET-NAME
        WHEN "minecraft:status_request"
            *> count the number of current players
            MOVE 0 TO TEMP-INT32
            PERFORM VARYING TEMP-INT16 FROM 1 BY 1 UNTIL TEMP-INT16 > MAX-CLIENTS
                IF CLIENT-PRESENT(TEMP-INT16) = 1 AND CLIENT-PLAYER(TEMP-INT16) > 0
                    ADD 1 TO TEMP-INT32
                END-IF
            END-PERFORM
            CALL "SendPacket-Status" USING CLIENT-ID SP-MOTD MAX-PLAYERS TEMP-INT32

        WHEN "minecraft:ping_request"
            *> respond with the same payload and close the connection
            CALL "Decode-Long" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT64
            CALL "SendPacket-PingResponse" USING CLIENT-ID TEMP-INT64
            PERFORM DisconnectClient

        WHEN OTHER
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet: " FUNCTION TRIM(PACKET-NAME)
    END-EVALUATE.

    EXIT PARAGRAPH.

HandleLogin.
    EVALUATE PACKET-NAME
        WHEN "minecraft:hello"
            *> Decode username
            MOVE SPACES TO TEMP-PLAYER-NAME
            CALL "Decode-String" USING CLIENT-RECEIVE-BUFFER PACKET-POSITION TEMP-INT32 TEMP-PLAYER-NAME

            *> Ignore UUID and generate our own
            ADD 16 TO PACKET-POSITION
            CALL "Players-NameToUUID" USING TEMP-PLAYER-NAME TEMP-UUID

            *> Check username against the whitelist
            IF SP-WHITELIST-ENABLE > 0
                CALL "Whitelist-Check" USING TEMP-UUID TEMP-PLAYER-NAME TEMP-INT8
                IF TEMP-INT8 = 0
                    MOVE "You are not white-listed on this server!" TO BUFFER
                    MOVE 40 TO BYTE-COUNT
                    DISPLAY "Disconnecting " FUNCTION TRIM(TEMP-PLAYER-NAME) ": " BUFFER(1:BYTE-COUNT)
                    CALL "SendPacket-Disconnect" USING CLIENT-ID CLIENT-STATE(CLIENT-ID) BUFFER BYTE-COUNT
                    PERFORM DisconnectClient
                    EXIT PARAGRAPH
                END-IF
            END-IF

            *> If the player is already connected, disconnect them first
            CALL "Players-FindConnectedByUUID" USING TEMP-UUID TEMP-INT8
            IF TEMP-INT8 > 0
                MOVE CLIENT-ID TO TEMP-INT64
                MOVE PLAYER-CLIENT(TEMP-INT8) TO CLIENT-ID
                MOVE "You logged in from another location" TO BUFFER
                MOVE 35 TO BYTE-COUNT
                DISPLAY "Disconnecting " FUNCTION TRIM(PLAYER-NAME(TEMP-INT8)) ": " BUFFER(1:BYTE-COUNT)
                CALL "SendPacket-Disconnect" USING CLIENT-ID CLIENT-STATE(CLIENT-ID) BUFFER BYTE-COUNT
                PERFORM DisconnectClient
                MOVE TEMP-INT64 TO CLIENT-ID
            END-IF

            *> Try to find an existing player for the UUID, or find a free slot to add a new player.
            CALL "Players-Connect" USING CLIENT-ID TEMP-UUID TEMP-PLAYER-NAME TEMP-INT8
            MOVE TEMP-INT8 TO CLIENT-PLAYER(CLIENT-ID)

            *> If no player slot was found, the server is full
            IF CLIENT-PLAYER(CLIENT-ID) = 0
                MOVE "The server is full" TO BUFFER
                MOVE 18 TO BYTE-COUNT
                DISPLAY "Disconnecting " FUNCTION TRIM(TEMP-PLAYER-NAME) ": " BUFFER(1:BYTE-COUNT)
                CALL "SendPacket-Disconnect" USING CLIENT-ID CLIENT-STATE(CLIENT-ID) BUFFER BYTE-COUNT
                PERFORM DisconnectClient
                EXIT PARAGRAPH
            END-IF

            *> Send login success. This should result in a "login acknowledged" packet by the client.
            CALL "SendPacket-LoginSuccess" USING CLIENT-ID PLAYER-UUID(CLIENT-PLAYER(CLIENT-ID)) PLAYER-NAME(CLIENT-PLAYER(CLIENT-ID))

        WHEN "minecraft:login_acknowledged"
            *> Must not happen before login start
            IF CLIENT-PLAYER(CLIENT-ID) = 0
                DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Client sent unexpected login acknowledge"
                PERFORM DisconnectClient
                EXIT PARAGRAPH
            END-IF

            *> Can move to configuration state
            MOVE CLIENT-STATE-CONFIGURATION TO CLIENT-STATE(CLIENT-ID)

        WHEN OTHER
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet: " FUNCTION TRIM(PACKET-NAME)
    END-EVALUATE

    EXIT PARAGRAPH.

HandleConfiguration.
    EVALUATE PACKET-NAME
        WHEN "minecraft:client_information"
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

        WHEN "minecraft:custom_payload"
            *> Not implemented
            CONTINUE

        WHEN "minecraft:finish_configuration"
            IF CONFIG-FINISH(CLIENT-ID) = 0
                DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Client sent unexpected acknowledge finish configuration"
                PERFORM DisconnectClient
                EXIT PARAGRAPH
            END-IF

            *> Can move to play state
            MOVE CLIENT-STATE-PLAY TO CLIENT-STATE(CLIENT-ID)

            *> send "Login (play)" with player index as entity ID
            MOVE CLIENT-PLAYER(CLIENT-ID) TO TEMP-INT32
            CALL "World-IsHardcore" USING TEMP-INT8
            CALL "SendPacket-LoginPlay" USING CLIENT-ID TEMP-INT32 VIEW-DISTANCE PLAYER-GAMEMODE(TEMP-INT32) TEMP-INT8 MAX-PLAYERS

            *> perform all actions to spawn the player, load initial chunks, etc.
            CALL "Server-SpawnPlayer" USING CLIENT-ID

            *> Send available commands list
            CALL "SendPacket-Commands" USING CLIENT-ID

            *> send join message to all other players
            INITIALIZE BUFFER
            STRING FUNCTION TRIM(PLAYER-NAME(CLIENT-PLAYER(CLIENT-ID))) " joined the game" INTO BUFFER
            MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BYTE-COUNT
            CALL "BroadcastChatMessageExcept" USING CLIENT-ID BUFFER BYTE-COUNT C-COLOR-YELLOW

        WHEN "minecraft:select_known_packs"
            *> Not implemented
            CONTINUE

        WHEN OTHER
            DISPLAY "[state=" CLIENT-STATE(CLIENT-ID) "] Unexpected packet: " FUNCTION TRIM(PACKET-NAME)
    END-EVALUATE.

    EXIT PARAGRAPH.

DisconnectClient.
    *> Disconnect the current client.
    CALL "Server-DisconnectClient" USING CLIENT-ID
    EXIT PARAGRAPH.

HandleServerError.
    IF ERRNO NOT = 0
        DISPLAY "Server socket error: " ERRNO
        STOP RUN RETURNING 1
    END-IF

    EXIT PARAGRAPH.

HandleClientError.
    IF ERRNO NOT = 0
        CALL "Server-ClientError" USING CLIENT-ID ERRNO
    END-IF

    EXIT PARAGRAPH.

END PROGRAM Server.

*> --- Server-SpawnPlayer ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Server-SpawnPlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    COPY DD-CLIENT-STATES.
    COPY DD-SERVER-PROPERTIES.
    *> constants
    01 GAME-EVENT-LEVEL-CHUNKS  BINARY-CHAR                 VALUE 13.
    01 FLOAT-ZERO               FLOAT-SHORT                 VALUE 0.0.
    01 ENTITY-EVENT-OP-4        BINARY-CHAR                 VALUE 28.
    *> variables
    01 PLAYER-ID                BINARY-LONG UNSIGNED.
    01 WORLD-AGE                BINARY-LONG-LONG.
    01 WORLD-TIME               BINARY-LONG-LONG.
    01 OTHER-CLIENT             BINARY-LONG UNSIGNED.
    01 OTHER-PLAYER-ID          BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    *> send world time
    CALL "World-GetAge" USING WORLD-AGE
    CALL "World-GetTime" USING WORLD-TIME
    CALL "SendPacket-UpdateTime" USING LK-CLIENT WORLD-AGE WORLD-TIME

    *> send game event "start waiting for level chunks"
    CALL "SendPacket-GameEvent" USING LK-CLIENT GAME-EVENT-LEVEL-CHUNKS FLOAT-ZERO

    *> Note: The official server sends a lot of additional packets in this phase, but they seem to be optional.
    *> For example: set ticking state (rate=20, frozen=false); step tick (steps=0 [sic.])
    *> We will skip these for now.

    *> send health and food level; experience
    CALL "SendPacket-SetHealth" USING LK-CLIENT PLAYER-HEALTH(PLAYER-ID) PLAYER-FOOD-LEVEL(PLAYER-ID) PLAYER-SATURATION(PLAYER-ID)
    CALL "SendPacket-SetExperience" USING LK-CLIENT PLAYER-XP-PROGRESS(PLAYER-ID) PLAYER-XP-LEVEL(PLAYER-ID) PLAYER-XP-TOTAL(PLAYER-ID)

    *> send inventory
    ADD 1 TO PLAYER-CONTAINER-STATE-ID(PLAYER-ID)
    CALL "SendPacket-SetContainerContent" USING LK-CLIENT PLAYER-CONTAINER-STATE-ID(PLAYER-ID)
        PLAYER-INVENTORY(PLAYER-ID) PLAYER-MOUSE-ITEM(PLAYER-ID)

    *> send selected hotbar slot
    CALL "SendPacket-SetHeldItem" USING LK-CLIENT PLAYER-HOTBAR(PLAYER-ID)

    *> enqueue surrounding chunks and send 3x3 chunks immediately around the player immediately
    CALL "SetCenterChunk" USING LK-CLIENT
    CALL "EnqueueSurroundingChunks" USING LK-CLIENT
    CALL "SendPreChunks" USING LK-CLIENT

    *> send the player list (including the new player) to the new player, and spawn player entities
    PERFORM VARYING OTHER-CLIENT FROM 1 BY 1 UNTIL OTHER-CLIENT > MAX-CLIENTS
        IF CLIENT-PRESENT(OTHER-CLIENT) = 1 AND CLIENT-STATE(OTHER-CLIENT) = CLIENT-STATE-PLAY
            MOVE CLIENT-PLAYER(OTHER-CLIENT) TO OTHER-PLAYER-ID
            CALL "SendPacket-AddPlayer" USING LK-CLIENT PLAYER-UUID(OTHER-PLAYER-ID) PLAYER-NAME(OTHER-PLAYER-ID)
            IF OTHER-CLIENT NOT = LK-CLIENT
                CALL "SendPacket-SpawnEntity" USING LK-CLIENT OTHER-PLAYER-ID PLAYER-UUID(OTHER-PLAYER-ID) PLAYER-POSITION(OTHER-PLAYER-ID) PLAYER-ROTATION(OTHER-PLAYER-ID)
            END-IF
        END-IF
    END-PERFORM

    *> Send abilities (flying, etc.)
    CALL "SendPacket-PlayerAbilities" USING LK-CLIENT PLAYER-GAMEMODE(PLAYER-ID) PLAYER-FLYING(PLAYER-ID)

    *> Set op permission level to 4 (full permissions) - mainly so the gamemode switcher works
    *> TODO: implement a proper permission system
    CALL "SendPacket-EntityEvent" USING LK-CLIENT PLAYER-ID ENTITY-EVENT-OP-4

    *> Send position ("Synchronize Player Position"). The client must confirm the teleportation.
    ADD 1 TO TELEPORT-SENT(LK-CLIENT)
    CALL "SendPacket-SetPlayerPosition" USING LK-CLIENT PLAYER-POSITION(PLAYER-ID) PLAYER-ROTATION(PLAYER-ID) TELEPORT-SENT(LK-CLIENT)

    *> send the new player to all other players
    PERFORM VARYING OTHER-CLIENT FROM 1 BY 1 UNTIL OTHER-CLIENT > MAX-CLIENTS
        IF CLIENT-PRESENT(OTHER-CLIENT) = 1 AND CLIENT-STATE(OTHER-CLIENT) = CLIENT-STATE-PLAY AND OTHER-CLIENT NOT = LK-CLIENT
            *> add the new player to the player list
            CALL "SendPacket-AddPlayer" USING OTHER-CLIENT PLAYER-UUID(PLAYER-ID) PLAYER-NAME(PLAYER-ID)
            *> spawn a player entity
            CALL "SendPacket-SpawnEntity" USING OTHER-CLIENT PLAYER-ID PLAYER-UUID(PLAYER-ID) PLAYER-POSITION(PLAYER-ID) PLAYER-ROTATION(PLAYER-ID)
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM Server-SpawnPlayer.

*> --- Server-DisconnectClient ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Server-DisconnectClient.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> constants
    01 C-COLOR-YELLOW           PIC X(16)                   VALUE "yellow".
    01 BLOCK-DESTRUCTION-RESET  BINARY-CHAR                 VALUE -1.
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
        EXIT PARAGRAPH
    END-IF

    MOVE 0 TO CLIENT-PRESENT(LK-CLIENT-ID)

    CALL "SocketClose" USING CLIENT-HNDL(LK-CLIENT-ID) GIVING ERRNO
    IF ERRNO NOT = 0
        DISPLAY "Error closing client socket: " ERRNO
    END-IF

    *> If the client was playing, send a leave message to all other clients, and remove the player from their world
    IF CLIENT-STATE(LK-CLIENT-ID) = CLIENT-STATE-PLAY
        *> send "<username> left the game" to all clients in play state, except the current client
        INITIALIZE BUFFER
        STRING FUNCTION TRIM(PLAYER-NAME(CLIENT-PLAYER(LK-CLIENT-ID))) " left the game" INTO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BYTE-COUNT
        CALL "BroadcastChatMessageExcept" USING LK-CLIENT-ID BUFFER BYTE-COUNT C-COLOR-YELLOW

        *> remove the player from the player list, and despawn the player entity
        MOVE CLIENT-PLAYER(LK-CLIENT-ID) TO PLAYER-ID
        PERFORM VARYING OTHER-CLIENT-ID FROM 1 BY 1 UNTIL OTHER-CLIENT-ID > MAX-CLIENTS
            IF CLIENT-PRESENT(OTHER-CLIENT-ID) = 1 AND CLIENT-STATE(OTHER-CLIENT-ID) = CLIENT-STATE-PLAY AND OTHER-CLIENT-ID NOT = LK-CLIENT-ID
                CALL "SendPacket-RemovePlayer" USING OTHER-CLIENT-ID PLAYER-UUID(PLAYER-ID)
                CALL "SendPacket-RemoveEntity" USING OTHER-CLIENT-ID PLAYER-ID
                IF PLAYER-BLOCK-BREAKING-STAGE(PLAYER-ID) >= 0
                    CALL "SendPacket-BlockDestruction" USING OTHER-CLIENT-ID PLAYER-ID PLAYER-BLOCK-BREAKING-POSITION(PLAYER-ID) BLOCK-DESTRUCTION-RESET
                END-IF
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
