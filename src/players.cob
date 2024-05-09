*> --- Players-Init ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-Init.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-PLAYERS.
    *> temporary data
    01 PLAYER-INDEX             BINARY-CHAR.

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
    *> Constants
    01 C-MINECRAFT-ITEM         PIC X(16) VALUE "minecraft:item".
    01 C-MINECRAFT-AIR          PIC X(16) VALUE "minecraft:air".
    *> File name and data
    01 PLAYER-FILE-NAME         PIC X(64).
    01 ERRNO                    BINARY-LONG.
    01 NBT-BUFFER               PIC X(64000).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    01 COMPRESSED-BUFFER        PIC X(64000).
    01 COMPRESSED-LENGTH        BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-PLAYERS.
    *> temporary data
    01 OFFSET                   BINARY-LONG UNSIGNED.
    01 TAG-NAME                 PIC X(256).
    01 NAME-LEN                 BINARY-LONG UNSIGNED.
    01 STR                      PIC X(256).
    01 STR-LEN                  BINARY-LONG UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT32                    BINARY-LONG.
    01 INT32-BYTES              REDEFINES INT32 PIC X(4).
    01 INVENTORY-INDEX          BINARY-LONG UNSIGNED.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-ENCODER.
LINKAGE SECTION.
    01 LK-PLAYER-ID             BINARY-CHAR.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER-ID LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> root tag
    MOVE 1 TO OFFSET
    CALL "NbtEncode-RootCompound" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET

    *> UUID (4 integers, MSB to LSB)
    MOVE "UUID" TO TAG-NAME
    MOVE 4 TO NAME-LEN
    MOVE 4 TO INT32
    CALL "NbtEncode-IntArray" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN INT32
    MOVE FUNCTION REVERSE(PLAYER-UUID(LK-PLAYER-ID)(1:4)) TO INT32-BYTES
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED INT32
    MOVE FUNCTION REVERSE(PLAYER-UUID(LK-PLAYER-ID)(5:4)) TO INT32-BYTES
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED INT32
    MOVE FUNCTION REVERSE(PLAYER-UUID(LK-PLAYER-ID)(9:4)) TO INT32-BYTES
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED INT32
    MOVE FUNCTION REVERSE(PLAYER-UUID(LK-PLAYER-ID)(13:4)) TO INT32-BYTES
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED INT32

    *> game mode ("playerGameType")
    MOVE "playerGameType" TO TAG-NAME
    MOVE 14 TO NAME-LEN
    CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN PLAYER-GAMEMODE(LK-PLAYER-ID)

    *> position ("Pos")
    MOVE "Pos" TO TAG-NAME
    MOVE 3 TO NAME-LEN
    CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN
    CALL "NbtEncode-Double" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED PLAYER-X(LK-PLAYER-ID)
    CALL "NbtEncode-Double" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED PLAYER-Y(LK-PLAYER-ID)
    CALL "NbtEncode-Double" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED PLAYER-Z(LK-PLAYER-ID)
    CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET

    *> rotation ("Rotation")
    MOVE "Rotation" TO TAG-NAME
    MOVE 8 TO NAME-LEN
    CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN
    CALL "NbtEncode-Float" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED PLAYER-YAW(LK-PLAYER-ID)
    CALL "NbtEncode-Float" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED PLAYER-PITCH(LK-PLAYER-ID)
    CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET

    *> selected hotbar slot ("SelectedItemSlot")
    MOVE "SelectedItemSlot" TO TAG-NAME
    MOVE 16 TO NAME-LEN
    CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN PLAYER-HOTBAR(LK-PLAYER-ID)

    *> inventory ("Inventory")
    MOVE "Inventory" TO TAG-NAME
    MOVE 9 TO NAME-LEN
    CALL "NbtEncode-List" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN
    PERFORM VARYING INVENTORY-INDEX FROM 1 BY 1 UNTIL INVENTORY-INDEX > 46
        IF PLAYER-INVENTORY-SLOT-ID(LK-PLAYER-ID, INVENTORY-INDEX) > 0 AND PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER-ID, INVENTORY-INDEX) > 0
            CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET OMITTED OMITTED

            MOVE "Slot" TO TAG-NAME
            MOVE 4 TO NAME-LEN
            COMPUTE INT8 = INVENTORY-INDEX - 1
            CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN INT8

            MOVE "id" TO TAG-NAME
            MOVE 2 TO NAME-LEN
            *> item ID needs to be converted to a string for future-proofing
            CALL "Registries-Get-EntryName" USING C-MINECRAFT-ITEM PLAYER-INVENTORY-SLOT-ID(LK-PLAYER-ID, INVENTORY-INDEX) STR
            MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
            CALL "NbtEncode-String" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN STR STR-LEN

            MOVE "count" TO TAG-NAME
            MOVE 5 TO NAME-LEN
            CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER-ID, INVENTORY-INDEX)

            *> TODO encode the NBT data as a compound - requires encoding and decoding at the network layer
            MOVE PLAYER-INVENTORY-SLOT-NBT-LENGTH(LK-PLAYER-ID, INVENTORY-INDEX) TO INT32
            IF INT32 > 0
                MOVE "tag" TO TAG-NAME
                MOVE 3 TO NAME-LEN
                CALL "NbtEncode-ByteBuffer" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN PLAYER-INVENTORY-SLOT-NBT-DATA(LK-PLAYER-ID, INVENTORY-INDEX) INT32
            END-IF

            CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET
        END-IF
    END-PERFORM
    CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET

    *> abilities
    MOVE "abilities" TO TAG-NAME
    MOVE 9 TO NAME-LEN
    CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN

    *> flying
    MOVE "flying" TO TAG-NAME
    MOVE 6 TO NAME-LEN
    CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET TAG-NAME NAME-LEN PLAYER-FLYING(LK-PLAYER-ID)

    *> end abilities
    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET

    *> end root tag
    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER OFFSET

    *> Create directories. Ignore errors, as they are likely to be caused by the directories already existing.
    CALL "CBL_CREATE_DIR" USING "save"
    CALL "CBL_CREATE_DIR" USING "save/playerdata"

    *> write the data to disk in gzip-compressed form
    COMPUTE NBT-BUFFER-LENGTH = OFFSET - 1
    MOVE LENGTH OF COMPRESSED-BUFFER TO COMPRESSED-LENGTH
    CALL "GzipCompress" USING NBT-BUFFER NBT-BUFFER-LENGTH COMPRESSED-BUFFER COMPRESSED-LENGTH GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF
    CALL "Players-PlayerFileName" USING PLAYER-UUID(LK-PLAYER-ID) PLAYER-FILE-NAME
    CALL "Files-WriteAll" USING PLAYER-FILE-NAME COMPRESSED-BUFFER COMPRESSED-LENGTH

    GOBACK.

END PROGRAM Players-SavePlayer.

*> --- Players-LoadPlayer ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-LoadPlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> Constants
    01 C-MINECRAFT-ITEM         PIC X(16) VALUE "minecraft:item".
    *> File name and data
    01 PLAYER-FILE-NAME         PIC X(255).
    01 ERRNO                    BINARY-LONG.
    01 COMPRESSED-BUFFER        PIC X(64000).
    01 COMPRESSED-LENGTH        BINARY-LONG UNSIGNED.
    01 NBT-BUFFER               PIC X(64000).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-PLAYERS.
    *> temporary data
    01 OFFSET                   BINARY-LONG UNSIGNED.
    01 TAG-NAME                 PIC X(256).
    01 NAME-LEN                 BINARY-LONG UNSIGNED.
    01 AT-END                   BINARY-CHAR UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT32                    BINARY-LONG.
    01 INT32-BYTES              REDEFINES INT32 PIC X(4).
    01 STR                      PIC X(256).
    01 STR-LEN                  BINARY-LONG UNSIGNED.
    01 INVENTORY-INDEX          BINARY-LONG UNSIGNED.
    01 INVENTORY-ID             BINARY-LONG.
    01 INVENTORY-COUNT          BINARY-CHAR UNSIGNED.
    01 INVENTORY-NBT-DATA       PIC X(1024).
    01 INVENTORY-NBT-LENGTH     BINARY-LONG UNSIGNED.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-DECODER.
LINKAGE SECTION.
    01 LK-PLAYER-ID             BINARY-CHAR.
    01 LK-PLAYER-UUID           PIC X(16).
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER-ID LK-PLAYER-UUID LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> Read the NBT file
    CALL "Players-PlayerFileName" USING LK-PLAYER-UUID PLAYER-FILE-NAME
    CALL "Files-ReadAll" USING PLAYER-FILE-NAME NBT-BUFFER NBT-BUFFER-LENGTH
    IF NBT-BUFFER-LENGTH = 0
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
    MOVE 1 TO OFFSET
    CALL "NbtDecode-RootCompound" USING NBT-DECODER-STATE NBT-BUFFER OFFSET

    PERFORM UNTIL EXIT
        CALL "NbtDecode-Peek" USING NBT-DECODER-STATE NBT-BUFFER OFFSET AT-END TAG-NAME NAME-LEN
        IF AT-END = 1
            EXIT PERFORM
        END-IF
        EVALUATE TAG-NAME(1:NAME-LEN)
            *> UUID (4 integers, MSB to LSB)
            WHEN "UUID"
                CALL "NbtDecode-List" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INT32
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INT32
                MOVE FUNCTION REVERSE(INT32-BYTES) TO LK-PLAYER-UUID(1:4)
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INT32
                MOVE FUNCTION REVERSE(INT32-BYTES) TO LK-PLAYER-UUID(5:4)
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INT32
                MOVE FUNCTION REVERSE(INT32-BYTES) TO LK-PLAYER-UUID(9:4)
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INT32
                MOVE FUNCTION REVERSE(INT32-BYTES) TO LK-PLAYER-UUID(13:4)
                CALL "NbtDecode-EndList" USING NBT-DECODER-STATE NBT-BUFFER OFFSET

            WHEN "playerGameType"
                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER OFFSET PLAYER-GAMEMODE(LK-PLAYER-ID)

            WHEN "Pos"
                CALL "NbtDecode-List" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INT32
                CALL "NbtDecode-Double" USING NBT-DECODER-STATE NBT-BUFFER OFFSET PLAYER-X(LK-PLAYER-ID)
                CALL "NbtDecode-Double" USING NBT-DECODER-STATE NBT-BUFFER OFFSET PLAYER-Y(LK-PLAYER-ID)
                CALL "NbtDecode-Double" USING NBT-DECODER-STATE NBT-BUFFER OFFSET PLAYER-Z(LK-PLAYER-ID)
                CALL "NbtDecode-EndList" USING NBT-DECODER-STATE NBT-BUFFER OFFSET

            WHEN "Rotation"
                CALL "NbtDecode-List" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INT32
                CALL "NbtDecode-Float" USING NBT-DECODER-STATE NBT-BUFFER OFFSET PLAYER-YAW(LK-PLAYER-ID)
                CALL "NbtDecode-Float" USING NBT-DECODER-STATE NBT-BUFFER OFFSET PLAYER-PITCH(LK-PLAYER-ID)
                CALL "NbtDecode-EndList" USING NBT-DECODER-STATE NBT-BUFFER OFFSET

            WHEN "SelectedItemSlot"
                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER OFFSET PLAYER-HOTBAR(LK-PLAYER-ID)

            WHEN "Inventory"
                CALL "NbtDecode-List" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INT32
                PERFORM INT32 TIMES
                    *> Start of slot compound
                    CALL "NbtDecode-Compound" USING NBT-DECODER-STATE NBT-BUFFER OFFSET
                    MOVE 0 TO INVENTORY-INDEX
                    MOVE 0 TO INVENTORY-ID
                    MOVE 0 TO INVENTORY-COUNT
                    MOVE 0 TO INVENTORY-NBT-LENGTH

                    *> Slot properties
                    PERFORM UNTIL EXIT
                        CALL "NbtDecode-Peek" USING NBT-DECODER-STATE NBT-BUFFER OFFSET AT-END TAG-NAME NAME-LEN
                        IF AT-END = 1
                            EXIT PERFORM
                        END-IF
                        EVALUATE TAG-NAME(1:NAME-LEN)
                            WHEN "Slot"
                                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INT8
                                COMPUTE INVENTORY-INDEX = INT8 + 1

                            WHEN "id"
                                *> Item ID needs to be converted from a string to a number
                                CALL "NbtDecode-String" USING NBT-DECODER-STATE NBT-BUFFER OFFSET STR STR-LEN
                                CALL "Registries-Get-EntryId" USING C-MINECRAFT-ITEM STR INVENTORY-ID

                            WHEN "count"
                                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INVENTORY-COUNT

                            WHEN "tag"
                                *> TODO decode the NBT data as a compound - requires encoding and decoding at the network layer
                                CALL "NbtDecode-ByteBuffer" USING NBT-DECODER-STATE NBT-BUFFER OFFSET INVENTORY-NBT-DATA INVENTORY-NBT-LENGTH

                            WHEN OTHER
                                CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER OFFSET
                        END-EVALUATE
                    END-PERFORM

                    *> End of slot compound
                    CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER OFFSET

                    *> If the slot had complete data, set it in the player's inventory
                    IF INVENTORY-INDEX > 0 AND INVENTORY-ID > 0 AND INVENTORY-COUNT > 0
                        MOVE INVENTORY-ID TO PLAYER-INVENTORY-SLOT-ID(LK-PLAYER-ID, INVENTORY-INDEX)
                        MOVE INVENTORY-COUNT TO PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER-ID, INVENTORY-INDEX)
                        MOVE INVENTORY-NBT-DATA(1:INVENTORY-NBT-LENGTH) TO PLAYER-INVENTORY-SLOT-NBT-DATA(LK-PLAYER-ID, INVENTORY-INDEX)
                        MOVE INVENTORY-NBT-LENGTH TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(LK-PLAYER-ID, INVENTORY-INDEX)
                    END-IF
                END-PERFORM
                CALL "NbtDecode-EndList" USING NBT-DECODER-STATE NBT-BUFFER OFFSET

            WHEN "abilities"
                *> Start of abilities compound
                CALL "NbtDecode-Compound" USING NBT-DECODER-STATE NBT-BUFFER OFFSET
                PERFORM UNTIL EXIT
                    CALL "NbtDecode-Peek" USING NBT-DECODER-STATE NBT-BUFFER OFFSET AT-END TAG-NAME NAME-LEN
                    IF AT-END = 1
                        EXIT PERFORM
                    END-IF
                    EVALUATE TAG-NAME(1:NAME-LEN)
                        WHEN "flying"
                            CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER OFFSET PLAYER-FLYING(LK-PLAYER-ID)
                        WHEN OTHER
                            CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER OFFSET
                    END-EVALUATE
                END-PERFORM
                *> End of abilities compound
                CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER OFFSET

            WHEN OTHER
                CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER OFFSET
        END-EVALUATE
    END-PERFORM

    *> end root tag
    CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER OFFSET

    GOBACK.

END PROGRAM Players-LoadPlayer.

*> --- Players-Save ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-Save.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-PLAYERS.
    *> temporary data
    01 FAILURE                  BINARY-CHAR UNSIGNED.
    01 PLAYER-INDEX             BINARY-CHAR.

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

*> --- Players-FindConnectedByUUID ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-FindConnectedByUUID.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-PLAYERS.
LINKAGE SECTION.
    01 LK-PLAYER-UUID           PIC X(16).
    01 LK-PLAYER-ID             BINARY-CHAR.

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
    *> shared data
    COPY DD-PLAYERS.
    *> temporary data
    01 IO-FAILURE               BINARY-CHAR UNSIGNED.
    01 PLAYER-INVENTORY-INDEX   BINARY-CHAR.
LINKAGE SECTION.
    01 LK-CLIENT-ID             BINARY-LONG UNSIGNED.
    01 LK-PLAYER-UUID           PIC X(16).
    01 LK-PLAYER-NAME           PIC X(16).
    01 LK-PLAYER-NAME-LENGTH    BINARY-LONG UNSIGNED.
    *> resulting player id
    01 LK-PLAYER-ID             BINARY-CHAR.

PROCEDURE DIVISION USING LK-CLIENT-ID LK-PLAYER-UUID LK-PLAYER-NAME LK-PLAYER-NAME-LENGTH LK-PLAYER-ID.
    *> Find a free player slot
    PERFORM VARYING LK-PLAYER-ID FROM 1 BY 1 UNTIL LK-PLAYER-ID > MAX-PLAYERS
        IF PLAYER-CLIENT(LK-PLAYER-ID) = 0
            *> Set up the player
            INITIALIZE PLAYER(LK-PLAYER-ID)
            *> TODO make the default game mode configurable
            MOVE 1 TO PLAYER-GAMEMODE(LK-PLAYER-ID)
            *> TODO Invent a better spawn location system
            MOVE 64 TO PLAYER-Y(LK-PLAYER-ID)
            *> Attempt to load existing player data
            CALL "Players-LoadPlayer" USING LK-PLAYER-ID LK-PLAYER-UUID IO-FAILURE
            IF IO-FAILURE NOT = 0
                *> For now, ignore any errors and use the defaults
                MOVE 0 TO IO-FAILURE
            END-IF
            *> Connect the player
            MOVE LK-CLIENT-ID TO PLAYER-CLIENT(LK-PLAYER-ID)
            MOVE LK-PLAYER-UUID TO PLAYER-UUID(LK-PLAYER-ID)
            MOVE LK-PLAYER-NAME(1:LK-PLAYER-NAME-LENGTH) TO PLAYER-NAME(LK-PLAYER-ID)
            MOVE LK-PLAYER-NAME-LENGTH TO PLAYER-NAME-LENGTH(LK-PLAYER-ID)
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
    01 FAILURE                  BINARY-CHAR UNSIGNED.
    *> shared data
    COPY DD-PLAYERS.
LINKAGE SECTION.
    01 LK-PLAYER-ID             BINARY-CHAR.

PROCEDURE DIVISION USING LK-PLAYER-ID.
    *> save the player data
    CALL "Players-SavePlayer" USING LK-PLAYER-ID FAILURE
    IF FAILURE NOT = 0
        DISPLAY "Error: Failed to save player data"
    END-IF
    *> make the player slot available
    MOVE 0 TO PLAYER-CLIENT(LK-PLAYER-ID)
    GOBACK.

END PROGRAM Players-Disconnect.
