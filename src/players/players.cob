*> --- Players-Init ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-Init.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    COPY DD-SERVER-PROPERTIES.
    01 PLAYER-INDEX             BINARY-LONG.

PROCEDURE DIVISION.
    PERFORM VARYING PLAYER-INDEX FROM 1 BY 1 UNTIL PLAYER-INDEX > MAX-PLAYERS
        MOVE 0 TO PLAYER-CLIENT(PLAYER-INDEX)
    END-PERFORM
    GOBACK.

END PROGRAM Players-Init.

*> --- Players-Save ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-Save.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    COPY DD-SERVER-PROPERTIES.
    *> temporary data
    01 FAILURE                  BINARY-CHAR UNSIGNED.
    01 PLAYER-INDEX             BINARY-LONG.

PROCEDURE DIVISION.
    PERFORM VARYING PLAYER-INDEX FROM 1 BY 1 UNTIL PLAYER-INDEX > MAX-PLAYERS
        IF PLAYER-CLIENT(PLAYER-INDEX) > 0
            CALL "Players-SavePlayer" USING PLAYER-INDEX FAILURE
            IF FAILURE NOT = 0
                DISPLAY "Error: Failed to save player data"
            END-IF
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Players-Save.

*> --- Players-NameToUUID ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-NameToUUID.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NAME-LEN                 BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-PLAYER-NAME           PIC X ANY LENGTH.
    01 LK-PLAYER-UUID           PIC X(16).

PROCEDURE DIVISION USING LK-PLAYER-NAME LK-PLAYER-UUID.
    *> For testing, we want to allow the same UUID to connect multiple times with different usernames.
    *> Since this is an offline server, we can simply generate our own UUID to achieve this.
    *> For lack of a better implementation, we will simply use the bytes of the username as the UUID.
    MOVE ALL X"00" TO LK-PLAYER-UUID
    MOVE FUNCTION STORED-CHAR-LENGTH(LK-PLAYER-NAME) TO NAME-LEN
    MOVE LK-PLAYER-NAME(1:NAME-LEN) TO LK-PLAYER-UUID(1:NAME-LEN)
    GOBACK.

END PROGRAM Players-NameToUUID.

*> --- Players-FindConnectedByUUID ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-FindConnectedByUUID.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    COPY DD-SERVER-PROPERTIES.
LINKAGE SECTION.
    01 LK-PLAYER-UUID           PIC X(16).
    01 LK-PLAYER-ID             BINARY-LONG.

PROCEDURE DIVISION USING LK-PLAYER-UUID LK-PLAYER-ID.
    PERFORM VARYING LK-PLAYER-ID FROM 1 BY 1 UNTIL LK-PLAYER-ID > MAX-PLAYERS
        IF PLAYER-CLIENT(LK-PLAYER-ID) > 0 AND PLAYER-UUID(LK-PLAYER-ID) = LK-PLAYER-UUID
            GOBACK
        END-IF
    END-PERFORM
    *> not found
    MOVE 0 TO LK-PLAYER-ID
    GOBACK.

END PROGRAM Players-FindConnectedByUUID.

*> --- Players-Connect ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-Connect.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    COPY DD-SERVER-PROPERTIES.
    01 IO-FAILURE               BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT-ID             BINARY-LONG UNSIGNED.
    01 LK-PLAYER-UUID           PIC X(16).
    01 LK-PLAYER-NAME           PIC X(16).
    *> resulting player id
    01 LK-PLAYER-ID             BINARY-LONG.

PROCEDURE DIVISION USING LK-CLIENT-ID LK-PLAYER-UUID LK-PLAYER-NAME LK-PLAYER-ID.
    *> Find a free player slot
    PERFORM VARYING LK-PLAYER-ID FROM 1 BY 1 UNTIL LK-PLAYER-ID > MAX-PLAYERS
        IF PLAYER-CLIENT(LK-PLAYER-ID) = 0
            *> Set up the player
            INITIALIZE PLAYER(LK-PLAYER-ID)
            *> TODO make the default game mode configurable
            MOVE 1 TO PLAYER-GAMEMODE(LK-PLAYER-ID)
            CALL "World-FindSpawnLocation" USING PLAYER-POSITION(LK-PLAYER-ID)
            MOVE 1 TO PLAYER-ON-GROUND(LK-PLAYER-ID)
            MOVE 20 TO PLAYER-HEALTH(LK-PLAYER-ID)
            MOVE 20 TO PLAYER-FOOD-LEVEL(LK-PLAYER-ID)
            MOVE 5 TO PLAYER-SATURATION(LK-PLAYER-ID)
            MOVE 0 TO PLAYER-HURT-TIME(LK-PLAYER-ID)
            MOVE 0 TO PLAYER-INVENTORY-STATE(LK-PLAYER-ID)
            MOVE 0 TO PLAYER-WINDOW-ID(LK-PLAYER-ID)
            MOVE -1 TO PLAYER-WINDOW-TYPE(LK-PLAYER-ID)
            MOVE 0 TO PLAYER-WINDOW-STATE(LK-PLAYER-ID)
            MOVE -1 TO PLAYER-BLOCK-BREAKING-STAGE(LK-PLAYER-ID)

            *> Attempt to load existing player data
            CALL "Players-LoadPlayer" USING LK-PLAYER-ID LK-PLAYER-UUID IO-FAILURE
            IF IO-FAILURE NOT = 0
                *> For now, ignore any errors and use the defaults
                MOVE 0 TO IO-FAILURE
            END-IF

            *> Connect the player
            MOVE LK-CLIENT-ID TO PLAYER-CLIENT(LK-PLAYER-ID)
            MOVE LK-PLAYER-UUID TO PLAYER-UUID(LK-PLAYER-ID)
            MOVE LK-PLAYER-NAME TO PLAYER-NAME(LK-PLAYER-ID)

            GOBACK
        END-IF
    END-PERFORM

    *> no free player slots
    MOVE 0 TO LK-PLAYER-ID

    GOBACK.

END PROGRAM Players-Connect.

*> --- Players-Disconnect ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-Disconnect.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    01 WINDOW-CLOSE-PTR         PROGRAM-POINTER.
    01 FAILURE                  BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-PLAYER-ID             BINARY-LONG.

PROCEDURE DIVISION USING LK-PLAYER-ID.
    *> close any open windows (this deals with items held in the cursor, in the crafting grid, etc.)
    CALL "GetCallback-WindowClose" USING PLAYER-WINDOW-TYPE(LK-PLAYER-ID) WINDOW-CLOSE-PTR
    IF WINDOW-CLOSE-PTR NOT = NULL
        CALL WINDOW-CLOSE-PTR USING LK-PLAYER-ID
    END-IF

    *> save the player data
    CALL "Players-SavePlayer" USING LK-PLAYER-ID FAILURE
    IF FAILURE NOT = 0
        DISPLAY "Error: Failed to save player data"
    END-IF

    *> make the player slot available
    MOVE 0 TO PLAYER-CLIENT(LK-PLAYER-ID)

    GOBACK.

END PROGRAM Players-Disconnect.

*> --- Players-Tick ---
*> Called every tick to update player state.
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-Tick.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    COPY DD-CLIENTS.
    COPY DD-CLIENT-STATES.
    COPY DD-SERVER-PROPERTIES.
    01 TIMER                    BINARY-LONG-LONG UNSIGNED   VALUE 0.
    01 CLIENT-ID                BINARY-LONG UNSIGNED.
    01 PLAYER-ID                BINARY-LONG UNSIGNED.
    01 VOID-DAMAGE-AMOUNT       FLOAT-SHORT                 VALUE 4.0.
    01 VOID-DAMAGE-TYPE         BINARY-LONG                 VALUE -1.

PROCEDURE DIVISION.
    *> TODO Replace this timer with per-player logic
    ADD 1 TO TIMER

    IF VOID-DAMAGE-TYPE < 0
        CALL "Registries-Lookup" USING "minecraft:damage_type" "minecraft:out_of_world" VOID-DAMAGE-TYPE
    END-IF

    *> Only tick players in the play state, as ticking may send packets that are not valid in other states.
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
            MOVE CLIENT-PLAYER(CLIENT-ID) TO PLAYER-ID
            PERFORM TickPlayer
        END-IF
    END-PERFORM

    GOBACK.

TickPlayer.
    ADD 1 TO PLAYER-WORLD-TIME(PLAYER-ID)

    *> If the player is dead, do not update their health.
    IF PLAYER-HEALTH(PLAYER-ID) <= 0
        EXIT PARAGRAPH
    END-IF

    IF PLAYER-HURT-TIME(PLAYER-ID) > 0
        SUBTRACT 1 FROM PLAYER-HURT-TIME(PLAYER-ID)
    END-IF

    *> Void damage
    IF PLAYER-Y(PLAYER-ID) < -128
        CALL "Players-Damage" USING PLAYER-ID VOID-DAMAGE-AMOUNT VOID-DAMAGE-TYPE
    END-IF

    *> TODO Implement a proper health system. For now, we simply heal 1/2 heart per second on a global timer.
    IF FUNCTION MOD(TIMER, 20) = 0
        COMPUTE PLAYER-HEALTH(PLAYER-ID) = FUNCTION MIN(20, PLAYER-HEALTH(PLAYER-ID) + 1)
        CALL "SendPacket-SetHealth" USING CLIENT-ID PLAYER-HEALTH(PLAYER-ID) PLAYER-FOOD-LEVEL(PLAYER-ID) PLAYER-SATURATION(PLAYER-ID)
    END-IF
    .

END PROGRAM Players-Tick.

*> --- Players-HandleMove ---
*> Handles movement input from the client. Each component is optional, as there are different types of movement packets.
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-HandleMove.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    78 FALL-DAMAGE-THRESHOLD    VALUE 3.0.
    01 FALL-DAMAGE-AMOUNT       FLOAT-SHORT.
    01 FALL-DAMAGE-TYPE         BINARY-LONG.
LINKAGE SECTION.
    01 LK-PLAYER                BINARY-LONG UNSIGNED.
    01 LK-POSITION.
        02 LK-POSITION-X        FLOAT-LONG.
        02 LK-POSITION-Y        FLOAT-LONG.
        02 LK-POSITION-Z        FLOAT-LONG.
    01 LK-ROTATION.
        02 LK-YAW               FLOAT-SHORT.
        02 LK-PITCH             FLOAT-SHORT.
    01 LK-FLAGS.
        02 LK-ON-GROUND         BINARY-CHAR UNSIGNED.
        02 LK-AGAINST-WALL      BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER OPTIONAL LK-POSITION OPTIONAL LK-ROTATION OPTIONAL LK-FLAGS.
    IF LK-FLAGS IS NOT OMITTED
        IF PLAYER-FLYING(LK-PLAYER) = 0
            EVALUATE TRUE
                WHEN PLAYER-ON-GROUND(LK-PLAYER) NOT = 0 AND LK-ON-GROUND = 0
                    PERFORM HandleLeaveGround
                WHEN PLAYER-ON-GROUND(LK-PLAYER) = 0 AND LK-ON-GROUND NOT = 0
                    PERFORM HandleHitGround
            END-EVALUATE
        ELSE
            MOVE 0 TO PLAYER-FALL-DISTANCE(LK-PLAYER)
        END-IF

        MOVE LK-ON-GROUND TO PLAYER-ON-GROUND(LK-PLAYER)
        MOVE LK-AGAINST-WALL TO PLAYER-AGAINST-WALL(LK-PLAYER)
    END-IF

    IF LK-POSITION IS NOT OMITTED
        IF PLAYER-ON-GROUND(LK-PLAYER) = 0
            COMPUTE PLAYER-FALL-DISTANCE(LK-PLAYER) = FUNCTION MAX(0, PLAYER-FALL-DISTANCE(LK-PLAYER) + PLAYER-Y(LK-PLAYER) - LK-POSITION-Y)
        END-IF

        MOVE LK-POSITION TO PLAYER-POSITION(LK-PLAYER)
    END-IF

    IF LK-ROTATION IS NOT OMITTED
        MOVE LK-ROTATION TO PLAYER-ROTATION(LK-PLAYER)
    END-IF

    GOBACK.

HandleLeaveGround.
    MOVE 0 TO PLAYER-FALL-DISTANCE(LK-PLAYER)
    .

HandleHitGround.
    COMPUTE FALL-DAMAGE-AMOUNT = PLAYER-FALL-DISTANCE(LK-PLAYER) - FALL-DAMAGE-THRESHOLD

    *> TODO avoid hardcoding the invulnerability period here
    IF FALL-DAMAGE-AMOUNT <= 0 OR PLAYER-WORLD-TIME(LK-PLAYER) < 20
        MOVE 0 TO PLAYER-FALL-DISTANCE(LK-PLAYER)
        EXIT PARAGRAPH
    END-IF

    CALL "Registries-Lookup" USING "minecraft:damage_type" "minecraft:fall" FALL-DAMAGE-TYPE
    CALL "Players-Damage" USING LK-PLAYER FALL-DAMAGE-AMOUNT FALL-DAMAGE-TYPE

    MOVE 0 TO PLAYER-FALL-DISTANCE(LK-PLAYER)
    .

END PROGRAM Players-HandleMove.

*> --- Players-Damage ---
*> Inflict damage on a player, taking into account the player's gamemode and the type of damage
*> from the "minecraft:damage_type" registry.
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-Damage.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    COPY DD-CLIENTS.
    COPY DD-CLIENT-STATES.
    COPY DD-SERVER-PROPERTIES.
    01 ENTITY-EVENT-DEATH       BINARY-CHAR UNSIGNED        VALUE 3.
    01 OTHER-CLIENT-ID          BINARY-LONG UNSIGNED.
    01 BUFFER                   PIC X(255).
    01 BYTE-COUNT               BINARY-LONG UNSIGNED.
    01 SOUND-ID                 BINARY-LONG.
    01 SOUND-VOLUME             FLOAT-SHORT.
    *> Whether the following data was initialized
    01 DATA-INIT                BINARY-CHAR UNSIGNED        VALUE 0.
    *> IDs of damage types for discerning the death message
    01 TYPE-FALL                BINARY-LONG UNSIGNED.
    01 TYPE-GENERIC_KILL        BINARY-LONG UNSIGNED.
    01 TYPE-OUT_OF_WORLD        BINARY-LONG UNSIGNED.
    *> IDs of damage sounds
    01 SOUND-HURT               BINARY-LONG UNSIGNED.
    01 SOUND-SMALL_FALL         BINARY-LONG UNSIGNED.
    01 SOUND-BIG_FALL           BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-PLAYER                BINARY-LONG UNSIGNED.
    01 LK-AMOUNT                FLOAT-SHORT.
    01 LK-DAMAGE-TYPE           BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER LK-AMOUNT LK-DAMAGE-TYPE.
    IF DATA-INIT = 0
        PERFORM InitData
        MOVE 1 TO DATA-INIT
    END-IF

    IF (PLAYER-HEALTH(LK-PLAYER) <= 0)
        *> Creative mode players are immune to all damage except the "/kill" command and void damage.
        OR (PLAYER-GAMEMODE(LK-PLAYER) = 1 AND NOT (LK-DAMAGE-TYPE = TYPE-GENERIC_KILL OR TYPE-OUT_OF_WORLD))
        *> Recently damaged players are immune (except for the "/kill" command).
        OR (PLAYER-HURT-TIME(LK-PLAYER) > 0 AND LK-DAMAGE-TYPE NOT = TYPE-GENERIC_KILL)
    THEN
        GOBACK
    END-IF

    COMPUTE PLAYER-HEALTH(LK-PLAYER) = FUNCTION MAX(0, PLAYER-HEALTH(LK-PLAYER) - LK-AMOUNT)
    MOVE 10 TO PLAYER-HURT-TIME(LK-PLAYER)

    CALL "SendPacket-SetHealth" USING PLAYER-CLIENT(LK-PLAYER) PLAYER-HEALTH(LK-PLAYER)
        PLAYER-FOOD-LEVEL(LK-PLAYER) PLAYER-SATURATION(LK-PLAYER)

    IF PLAYER-HEALTH(LK-PLAYER) > 0
        *> The player is not dead, so the sound won't be played via the entity event packet.
        PERFORM PickDamageSound
        PERFORM VARYING OTHER-CLIENT-ID FROM 1 BY 1 UNTIL OTHER-CLIENT-ID > MAX-CLIENTS
            IF CLIENT-STATE(OTHER-CLIENT-ID) = CLIENT-STATE-PLAY
                CALL "SendPacket-EntitySound" USING OTHER-CLIENT-ID LK-PLAYER SOUND-ID SOUND-VOLUME
            END-IF
        END-PERFORM

        GOBACK
    END-IF

    *> Play the death sound and animation to all players (including the dying player).
    *> For this to have any effect, the player must have 0 health. For the dying player, this is already the case.
    *> For all others, it will be handled by "set entity metadata" in the main loop.
    PERFORM VARYING OTHER-CLIENT-ID FROM 1 BY 1 UNTIL OTHER-CLIENT-ID > MAX-CLIENTS
        IF CLIENT-STATE(OTHER-CLIENT-ID) = CLIENT-STATE-PLAY
            CALL "SendPacket-EntityEvent" USING OTHER-CLIENT-ID LK-PLAYER ENTITY-EVENT-DEATH
        END-IF
    END-PERFORM

    PERFORM BuildDeathMessage

    MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BYTE-COUNT
    CALL "BroadcastChatMessage" USING BUFFER BYTE-COUNT OMITTED

    GOBACK.

BuildDeathMessage.
    INITIALIZE BUFFER

    EVALUATE LK-DAMAGE-TYPE
        WHEN TYPE-FALL
            STRING FUNCTION TRIM(PLAYER-NAME(LK-PLAYER)) " fell from a high place" INTO BUFFER
        WHEN TYPE-GENERIC_KILL
            STRING FUNCTION TRIM(PLAYER-NAME(LK-PLAYER)) " was killed" INTO BUFFER
        WHEN TYPE-OUT_OF_WORLD
            STRING FUNCTION TRIM(PLAYER-NAME(LK-PLAYER)) " fell out of the world" INTO BUFFER
        WHEN OTHER
            STRING FUNCTION TRIM(PLAYER-NAME(LK-PLAYER)) " died" INTO BUFFER
    END-EVALUATE
    .

PickDamageSound.
    MOVE SOUND-HURT TO SOUND-ID
    MOVE 1.0 TO SOUND-VOLUME

    EVALUATE LK-DAMAGE-TYPE
        WHEN TYPE-FALL
            IF LK-AMOUNT <= 4.0
                MOVE SOUND-SMALL_FALL TO SOUND-ID
            ELSE
                MOVE SOUND-BIG_FALL TO SOUND-ID
            END-IF
    END-EVALUATE
    .

InitData.
    CALL "Registries-Lookup" USING "minecraft:damage_type" "minecraft:fall" TYPE-FALL
    CALL "Registries-Lookup" USING "minecraft:damage_type" "minecraft:generic_kill" TYPE-GENERIC_KILL
    CALL "Registries-Lookup" USING "minecraft:damage_type" "minecraft:out_of_world" TYPE-OUT_OF_WORLD

    CALL "Registries-Lookup" USING "minecraft:sound_event" "minecraft:entity.player.hurt" SOUND-HURT
    CALL "Registries-Lookup" USING "minecraft:sound_event" "minecraft:entity.player.small_fall" SOUND-SMALL_FALL
    CALL "Registries-Lookup" USING "minecraft:sound_event" "minecraft:entity.player.big_fall" SOUND-BIG_FALL
    .

END PROGRAM Players-Damage.
