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

*> --- Players-PlayerFileName ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-PlayerFileName.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UUID-STR                 PIC X(36).
LINKAGE SECTION.
    01 LK-PLAYER-UUID           PIC X(16).
    01 LK-PLAYER-FILE-NAME      PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-PLAYER-UUID LK-PLAYER-FILE-NAME.
    CALL "UUID-ToString" USING LK-PLAYER-UUID UUID-STR
    MOVE SPACES TO LK-PLAYER-FILE-NAME
    STRING "save/playerdata/" UUID-STR ".dat" INTO LK-PLAYER-FILE-NAME
    GOBACK.

END PROGRAM Players-PlayerFileName.

*> --- Players-SavePlayer ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-SavePlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    *> File name and data
    01 PLAYER-FILE-NAME         PIC X(64).
    01 ERRNO                    BINARY-LONG.
    01 NBT-BUFFER               PIC X(64000).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    01 COMPRESSED-BUFFER        PIC X(64000).
    01 COMPRESSED-LENGTH        BINARY-LONG UNSIGNED.
    *> temporary data
    01 TAG-NAME                 PIC X(256).
    01 NAME-LEN                 BINARY-LONG UNSIGNED.
    01 STR                      PIC X(256).
    01 STR-LEN                  BINARY-LONG UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT32                    BINARY-LONG.
    01 INVENTORY-INDEX          BINARY-LONG UNSIGNED.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-ENCODER.
LINKAGE SECTION.
    01 LK-PLAYER-ID             BINARY-LONG.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER-ID LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> root tag
    MOVE 1 TO NBT-ENCODER-OFFSET
    CALL "NbtEncode-RootCompound" USING NBT-ENCODER-STATE NBT-BUFFER

    *> UUID (4 integers, MSB to LSB)
    MOVE "UUID" TO TAG-NAME
    MOVE 4 TO NAME-LEN
    CALL "NbtEncode-UUID" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-UUID(LK-PLAYER-ID)

    *> game mode ("playerGameType")
    MOVE "playerGameType" TO TAG-NAME
    MOVE 14 TO NAME-LEN
    CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-GAMEMODE(LK-PLAYER-ID)

    *> position ("Pos")
    MOVE "Pos" TO TAG-NAME
    MOVE 3 TO NAME-LEN
    CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN
    CALL "NbtEncode-Double" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED PLAYER-X(LK-PLAYER-ID)
    CALL "NbtEncode-Double" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED PLAYER-Y(LK-PLAYER-ID)
    CALL "NbtEncode-Double" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED PLAYER-Z(LK-PLAYER-ID)
    CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER

    *> rotation ("Rotation")
    MOVE "Rotation" TO TAG-NAME
    MOVE 8 TO NAME-LEN
    CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN
    CALL "NbtEncode-Float" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED PLAYER-YAW(LK-PLAYER-ID)
    CALL "NbtEncode-Float" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED PLAYER-PITCH(LK-PLAYER-ID)
    CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER

    *> health ("Health")
    MOVE "Health" TO TAG-NAME
    MOVE 6 TO NAME-LEN
    CALL "NbtEncode-Float" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-HEALTH(LK-PLAYER-ID)

    *> food level ("foodLevel")
    MOVE "foodLevel" TO TAG-NAME
    MOVE 9 TO NAME-LEN
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-FOOD-LEVEL(LK-PLAYER-ID)

    *> food saturation ("foodSaturationLevel")
    MOVE "foodSaturationLevel" TO TAG-NAME
    MOVE 19 TO NAME-LEN
    CALL "NbtEncode-Float" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-SATURATION(LK-PLAYER-ID)

    *> experience level ("XpLevel")
    MOVE "XpLevel" TO TAG-NAME
    MOVE 7 TO NAME-LEN
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-XP-LEVEL(LK-PLAYER-ID)

    *> experience progress towards next level ("XpP")
    MOVE "XpP" TO TAG-NAME
    MOVE 3 TO NAME-LEN
    CALL "NbtEncode-Float" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-XP-PROGRESS(LK-PLAYER-ID)

    *> total experience ("XpTotal")
    MOVE "XpTotal" TO TAG-NAME
    MOVE 7 TO NAME-LEN
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-XP-TOTAL(LK-PLAYER-ID)

    *> selected hotbar slot ("SelectedItemSlot")
    MOVE "SelectedItemSlot" TO TAG-NAME
    MOVE 16 TO NAME-LEN
    CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-HOTBAR(LK-PLAYER-ID)

    *> inventory ("Inventory")
    MOVE "Inventory" TO TAG-NAME
    MOVE 9 TO NAME-LEN
    CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

    *> Skip the first slot, as it is the crafting output
    PERFORM VARYING INVENTORY-INDEX FROM 2 BY 1 UNTIL INVENTORY-INDEX > 46
        IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER-ID, INVENTORY-INDEX) > 0
            CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER OMITTED OMITTED

            MOVE "Slot" TO TAG-NAME
            MOVE 4 TO NAME-LEN
            COMPUTE INT8 = INVENTORY-INDEX - 1
            EVALUATE INT8
                WHEN 36 THRU 44
                    SUBTRACT 36 FROM INT8
                WHEN 8
                    MOVE 100 TO INT8
                WHEN 7
                    MOVE 101 TO INT8
                WHEN 6
                    MOVE 102 TO INT8
                WHEN 5
                    MOVE 103 TO INT8
                WHEN 45
                    MOVE -106 TO INT8
                WHEN 1 THRU 4
                    ADD 79 TO INT8
            END-EVALUATE
            CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN INT8

            MOVE "id" TO TAG-NAME
            MOVE 2 TO NAME-LEN
            *> item ID needs to be converted to a string for future-proofing
            CALL "Registries-Get-EntryName" USING "minecraft:item" PLAYER-INVENTORY-SLOT-ID(LK-PLAYER-ID, INVENTORY-INDEX) STR
            MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
            CALL "NbtEncode-String" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN STR STR-LEN

            MOVE "count" TO TAG-NAME
            MOVE 5 TO NAME-LEN
            CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER-ID, INVENTORY-INDEX)

            *> TODO encode the structured components
            MOVE PLAYER-INVENTORY-SLOT-NBT-LENGTH(LK-PLAYER-ID, INVENTORY-INDEX) TO INT32
            IF INT32 > 0
                MOVE "tag" TO TAG-NAME
                MOVE 3 TO NAME-LEN
                CALL "NbtEncode-ByteBuffer" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-INVENTORY-SLOT-NBT-DATA(LK-PLAYER-ID, INVENTORY-INDEX) INT32
            END-IF

            CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER
        END-IF
    END-PERFORM
    CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER

    *> abilities
    MOVE "abilities" TO TAG-NAME
    MOVE 9 TO NAME-LEN
    CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

    *> flying
    MOVE "flying" TO TAG-NAME
    MOVE 6 TO NAME-LEN
    CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-FLYING(LK-PLAYER-ID)

    *> end abilities
    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER

    *> end root tag
    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER

    *> Create directories. Ignore errors, as they are likely to be caused by the directories already existing.
    CALL "CBL_CREATE_DIR" USING "save"
    CALL "CBL_CREATE_DIR" USING "save/playerdata"

    *> write the data to disk in gzip-compressed form
    COMPUTE NBT-BUFFER-LENGTH = NBT-ENCODER-OFFSET - 1
    MOVE LENGTH OF COMPRESSED-BUFFER TO COMPRESSED-LENGTH
    CALL "GzipCompress" USING NBT-BUFFER NBT-BUFFER-LENGTH COMPRESSED-BUFFER COMPRESSED-LENGTH GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF
    CALL "Players-PlayerFileName" USING PLAYER-UUID(LK-PLAYER-ID) PLAYER-FILE-NAME
    CALL "Files-WriteAll" USING PLAYER-FILE-NAME COMPRESSED-BUFFER COMPRESSED-LENGTH LK-FAILURE

    GOBACK.

END PROGRAM Players-SavePlayer.

*> --- Players-LoadPlayer ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-LoadPlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    *> File name and data
    01 PLAYER-FILE-NAME         PIC X(255).
    01 ERRNO                    BINARY-LONG.
    01 COMPRESSED-BUFFER        PIC X(64000).
    01 COMPRESSED-LENGTH        BINARY-LONG UNSIGNED.
    01 NBT-BUFFER               PIC X(64000).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    *> temporary data
    01 TAG-NAME                 PIC X(256).
    01 NAME-LEN                 BINARY-LONG UNSIGNED.
    01 AT-END                   BINARY-CHAR UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT32                    BINARY-LONG.
    01 STR                      PIC X(256).
    01 STR-LEN                  BINARY-LONG UNSIGNED.
    01 INVENTORY-INDEX          BINARY-LONG UNSIGNED.
    01 INVENTORY-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==INVENTORY==.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-DECODER.
LINKAGE SECTION.
    01 LK-PLAYER-ID             BINARY-LONG.
    01 LK-PLAYER-UUID           PIC X(16).
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER-ID LK-PLAYER-UUID LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> Read the NBT file
    CALL "Players-PlayerFileName" USING LK-PLAYER-UUID PLAYER-FILE-NAME
    CALL "Files-ReadAll" USING PLAYER-FILE-NAME NBT-BUFFER NBT-BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0 OR NBT-BUFFER-LENGTH = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> Check for the gzip magic number, and decompress if present
    IF NBT-BUFFER(1:2) = X"1F8B"
        MOVE NBT-BUFFER(1:NBT-BUFFER-LENGTH) TO COMPRESSED-BUFFER(1:NBT-BUFFER-LENGTH)
        MOVE NBT-BUFFER-LENGTH TO COMPRESSED-LENGTH
        MOVE LENGTH OF NBT-BUFFER TO NBT-BUFFER-LENGTH
        CALL "GzipDecompress" USING COMPRESSED-BUFFER COMPRESSED-LENGTH NBT-BUFFER NBT-BUFFER-LENGTH GIVING ERRNO
        IF ERRNO NOT = 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
    END-IF

    *> root tag
    MOVE 1 TO NBT-DECODER-OFFSET
    CALL "NbtDecode-RootCompound" USING NBT-DECODER-STATE NBT-BUFFER

    PERFORM UNTIL EXIT
        CALL "NbtDecode-Peek" USING NBT-DECODER-STATE NBT-BUFFER AT-END TAG-NAME NAME-LEN
        IF AT-END = 1
            EXIT PERFORM
        END-IF
        EVALUATE TAG-NAME(1:NAME-LEN)
            *> UUID (4 integers, MSB to LSB)
            WHEN "UUID"
                *> ignored, as we already have the UUID
                CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER

            WHEN "playerGameType"
                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-GAMEMODE(LK-PLAYER-ID)

            WHEN "Pos"
                CALL "NbtDecode-List" USING NBT-DECODER-STATE NBT-BUFFER INT32
                CALL "NbtDecode-Double" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-X(LK-PLAYER-ID)
                CALL "NbtDecode-Double" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-Y(LK-PLAYER-ID)
                CALL "NbtDecode-Double" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-Z(LK-PLAYER-ID)
                CALL "NbtDecode-EndList" USING NBT-DECODER-STATE NBT-BUFFER

            WHEN "Rotation"
                CALL "NbtDecode-List" USING NBT-DECODER-STATE NBT-BUFFER INT32
                CALL "NbtDecode-Float" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-YAW(LK-PLAYER-ID)
                CALL "NbtDecode-Float" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-PITCH(LK-PLAYER-ID)
                CALL "NbtDecode-EndList" USING NBT-DECODER-STATE NBT-BUFFER

            WHEN "Health"
                CALL "NbtDecode-Float" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-HEALTH(LK-PLAYER-ID)

            WHEN "foodLevel"
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-FOOD-LEVEL(LK-PLAYER-ID)

            WHEN "foodSaturationLevel"
                CALL "NbtDecode-Float" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-SATURATION(LK-PLAYER-ID)

            WHEN "XpLevel"
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-XP-LEVEL(LK-PLAYER-ID)

            WHEN "XpP"
                CALL "NbtDecode-Float" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-XP-PROGRESS(LK-PLAYER-ID)

            WHEN "XpTotal"
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-XP-TOTAL(LK-PLAYER-ID)

            WHEN "SelectedItemSlot"
                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-HOTBAR(LK-PLAYER-ID)

            WHEN "Inventory"
                CALL "NbtDecode-List" USING NBT-DECODER-STATE NBT-BUFFER INT32
                PERFORM INT32 TIMES
                    *> Start of slot compound
                    CALL "NbtDecode-Compound" USING NBT-DECODER-STATE NBT-BUFFER
                    MOVE 0 TO INVENTORY-INDEX
                    INITIALIZE INVENTORY-SLOT
                    *> default structured components (0x00 = nothing to add, 0x00 = nothing to remove)
                    MOVE 2 TO INVENTORY-SLOT-NBT-LENGTH
                    MOVE X"0000" TO INVENTORY-SLOT-NBT-DATA(1:INVENTORY-SLOT-NBT-LENGTH)

                    *> Slot properties
                    PERFORM UNTIL EXIT
                        CALL "NbtDecode-Peek" USING NBT-DECODER-STATE NBT-BUFFER AT-END TAG-NAME NAME-LEN
                        IF AT-END = 1
                            EXIT PERFORM
                        END-IF
                        EVALUATE TAG-NAME(1:NAME-LEN)
                            WHEN "Slot"
                                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER INT8
                                EVALUATE INT8
                                    WHEN 0 THRU 8
                                        ADD 36 TO INT8
                                    WHEN 100
                                        MOVE 8 TO INT8
                                    WHEN 101
                                        MOVE 7 TO INT8
                                    WHEN 102
                                        MOVE 6 TO INT8
                                    WHEN 103
                                        MOVE 5 TO INT8
                                    WHEN -106
                                        MOVE 45 TO INT8
                                    WHEN 80 THRU 83
                                        SUBTRACT 79 FROM INT8
                                END-EVALUATE
                                COMPUTE INVENTORY-INDEX = INT8 + 1

                            WHEN "id"
                                *> Item ID needs to be converted from a string to a number
                                CALL "NbtDecode-String" USING NBT-DECODER-STATE NBT-BUFFER STR STR-LEN
                                CALL "Registries-Get-EntryId" USING "minecraft:item" STR INVENTORY-SLOT-ID

                            WHEN "count"
                                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER INVENTORY-SLOT-COUNT

                            *> TODO use "components" instead, and decode it as a compound
                            WHEN "tag"
                                CALL "NbtDecode-ByteBuffer" USING NBT-DECODER-STATE NBT-BUFFER INVENTORY-SLOT-NBT-DATA INVENTORY-SLOT-NBT-LENGTH

                            WHEN OTHER
                                CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER
                        END-EVALUATE
                    END-PERFORM

                    *> End of slot compound
                    CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER

                    *> If the slot had complete data, set it in the player's inventory
                    IF INVENTORY-INDEX > 0 AND INVENTORY-SLOT-ID > 0 AND INVENTORY-SLOT-COUNT > 0
                        MOVE INVENTORY-SLOT-ID TO PLAYER-INVENTORY-SLOT-ID(LK-PLAYER-ID, INVENTORY-INDEX)
                        MOVE INVENTORY-SLOT-COUNT TO PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER-ID, INVENTORY-INDEX)
                        MOVE INVENTORY-SLOT-NBT-DATA(1:INVENTORY-SLOT-NBT-LENGTH) TO PLAYER-INVENTORY-SLOT-NBT-DATA(LK-PLAYER-ID, INVENTORY-INDEX)
                        MOVE INVENTORY-SLOT-NBT-LENGTH TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(LK-PLAYER-ID, INVENTORY-INDEX)
                    END-IF
                END-PERFORM
                CALL "NbtDecode-EndList" USING NBT-DECODER-STATE NBT-BUFFER

            WHEN "abilities"
                *> Start of abilities compound
                CALL "NbtDecode-Compound" USING NBT-DECODER-STATE NBT-BUFFER
                PERFORM UNTIL EXIT
                    CALL "NbtDecode-Peek" USING NBT-DECODER-STATE NBT-BUFFER AT-END TAG-NAME NAME-LEN
                    IF AT-END = 1
                        EXIT PERFORM
                    END-IF
                    EVALUATE TAG-NAME(1:NAME-LEN)
                        WHEN "flying"
                            CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-FLYING(LK-PLAYER-ID)
                        WHEN OTHER
                            CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER
                    END-EVALUATE
                END-PERFORM
                *> End of abilities compound
                CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER

            WHEN OTHER
                CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER
        END-EVALUATE
    END-PERFORM

    *> end root tag
    CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER

    GOBACK.

END PROGRAM Players-LoadPlayer.

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
            MOVE 20 TO PLAYER-HEALTH(LK-PLAYER-ID)
            MOVE 20 TO PLAYER-FOOD-LEVEL(LK-PLAYER-ID)
            MOVE 5 TO PLAYER-SATURATION(LK-PLAYER-ID)
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
