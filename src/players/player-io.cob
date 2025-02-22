*> --- Players-PlayerFileName ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-PlayerFileName.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-SERVER-PROPERTIES.
    01 UUID-STR                 PIC X(36).
LINKAGE SECTION.
    01 LK-PLAYER-UUID           PIC X(16).
    01 LK-PLAYER-FILE-NAME      PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-PLAYER-UUID LK-PLAYER-FILE-NAME.
    CALL "UUID-ToString" USING LK-PLAYER-UUID UUID-STR
    MOVE SPACES TO LK-PLAYER-FILE-NAME
    STRING FUNCTION TRIM(SP-LEVEL-NAME) "/playerdata/" UUID-STR ".dat" INTO LK-PLAYER-FILE-NAME
    GOBACK.

END PROGRAM Players-PlayerFileName.

*> --- Players-SavePlayer ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Players-SavePlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PLAYERS.
    COPY DD-SERVER-PROPERTIES.
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

    *> on ground ("OnGround")
    MOVE "OnGround" TO TAG-NAME
    MOVE 8 TO NAME-LEN
    CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-ON-GROUND(LK-PLAYER-ID)

    *> fall distance ("FallDistance")
    MOVE "FallDistance" TO TAG-NAME
    MOVE 12 TO NAME-LEN
    CALL "NbtEncode-Float" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN PLAYER-FALL-DISTANCE(LK-PLAYER-ID)

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
    CALL "CBL_CREATE_DIR" USING SP-LEVEL-NAME
    INITIALIZE STR
    STRING FUNCTION TRIM(SP-LEVEL-NAME) "/playerdata" INTO STR
    CALL "CBL_CREATE_DIR" USING STR

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

            WHEN "OnGround"
                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-ON-GROUND(LK-PLAYER-ID)

            WHEN "FallDistance"
                CALL "NbtDecode-Float" USING NBT-DECODER-STATE NBT-BUFFER PLAYER-FALL-DISTANCE(LK-PLAYER-ID)

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
