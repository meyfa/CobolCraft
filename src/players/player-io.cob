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
    01 ITEM-REGISTRY            BINARY-LONG                     VALUE -1.
    *> File name and data
    01 PLAYER-FILE-NAME         PIC X(64).
    01 ERRNO                    BINARY-LONG.
    01 BUFFER                   PIC X(64000).
    01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
    01 COMPRESSED-BUFFER        PIC X(64000).
    01 COMPRESSED-LENGTH        BINARY-LONG UNSIGNED.
    *> temporary data
    01 STR                      PIC X(256).
    01 LEN                      BINARY-LONG UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT16                    BINARY-SHORT.
    01 INT32                    BINARY-LONG.
    01 INVENTORY-INDEX          BINARY-LONG UNSIGNED.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-ENCODER.
LINKAGE SECTION.
    01 LK-PLAYER-ID             BINARY-LONG.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-PLAYER-ID LK-FAILURE.
    IF ITEM-REGISTRY < 0
        CALL "Registries-LookupRegistry" USING "minecraft:item" ITEM-REGISTRY
    END-IF

    MOVE 0 TO LK-FAILURE

    *> root tag
    MOVE 1 TO NBTENC-OFFSET
    CALL "NbtEncode-RootCompound" USING NBTENC BUFFER

    CALL "NbtEncode-UUID" USING NBTENC BUFFER "UUID" PLAYER-UUID(LK-PLAYER-ID)
    CALL "NbtEncode-Byte" USING NBTENC BUFFER "playerGameType" PLAYER-GAMEMODE(LK-PLAYER-ID)

    CALL "NbtEncode-List" USING NBTENC BUFFER "Pos"
    CALL "NbtEncode-Double" USING NBTENC BUFFER OMITTED PLAYER-X(LK-PLAYER-ID)
    CALL "NbtEncode-Double" USING NBTENC BUFFER OMITTED PLAYER-Y(LK-PLAYER-ID)
    CALL "NbtEncode-Double" USING NBTENC BUFFER OMITTED PLAYER-Z(LK-PLAYER-ID)
    CALL "NbtEncode-EndList" USING NBTENC BUFFER

    CALL "NbtEncode-List" USING NBTENC BUFFER "Rotation"
    CALL "NbtEncode-Float" USING NBTENC BUFFER OMITTED PLAYER-YAW(LK-PLAYER-ID)
    CALL "NbtEncode-Float" USING NBTENC BUFFER OMITTED PLAYER-PITCH(LK-PLAYER-ID)
    CALL "NbtEncode-EndList" USING NBTENC BUFFER

    CALL "NbtEncode-Byte" USING NBTENC BUFFER "OnGround" PLAYER-ON-GROUND(LK-PLAYER-ID)
    CALL "NbtEncode-Float" USING NBTENC BUFFER "FallDistance" PLAYER-FALL-DISTANCE(LK-PLAYER-ID)

    MOVE PLAYER-HURT-TIME(LK-PLAYER-ID) TO INT16
    CALL "NbtEncode-Short" USING NBTENC BUFFER "HurtTime" INT16

    CALL "NbtEncode-Float" USING NBTENC BUFFER "Health" PLAYER-HEALTH(LK-PLAYER-ID)
    CALL "NbtEncode-Int" USING NBTENC BUFFER "foodLevel" PLAYER-FOOD-LEVEL(LK-PLAYER-ID)
    CALL "NbtEncode-Float" USING NBTENC BUFFER "foodSaturationLevel" PLAYER-SATURATION(LK-PLAYER-ID)

    CALL "NbtEncode-Int" USING NBTENC BUFFER "XpLevel" PLAYER-XP-LEVEL(LK-PLAYER-ID)
    CALL "NbtEncode-Float" USING NBTENC BUFFER "XpP" PLAYER-XP-PROGRESS(LK-PLAYER-ID)
    CALL "NbtEncode-Int" USING NBTENC BUFFER "XpTotal" PLAYER-XP-TOTAL(LK-PLAYER-ID)

    CALL "NbtEncode-Byte" USING NBTENC BUFFER "SelectedItemSlot" PLAYER-HOTBAR(LK-PLAYER-ID)
    CALL "NbtEncode-List" USING NBTENC BUFFER "Inventory"

    *> Skip the first slot, as it is the crafting output
    PERFORM VARYING INVENTORY-INDEX FROM 2 BY 1 UNTIL INVENTORY-INDEX > 46
        IF PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER-ID, INVENTORY-INDEX) > 0
            CALL "NbtEncode-Compound" USING NBTENC BUFFER OMITTED

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
            CALL "NbtEncode-Byte" USING NBTENC BUFFER "Slot" INT8

            *> item ID needs to be converted to a string for future-proofing
            CALL "Registries-EntryName" USING ITEM-REGISTRY PLAYER-INVENTORY-SLOT-ID(LK-PLAYER-ID, INVENTORY-INDEX) STR
            MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO LEN
            CALL "NbtEncode-String" USING NBTENC BUFFER "id" STR LEN

            CALL "NbtEncode-Byte" USING NBTENC BUFFER "count" PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER-ID, INVENTORY-INDEX)

            *> TODO encode the structured components
            MOVE PLAYER-INVENTORY-SLOT-NBT-LENGTH(LK-PLAYER-ID, INVENTORY-INDEX) TO INT32
            IF INT32 > 0
                CALL "NbtEncode-ByteBuffer" USING NBTENC BUFFER "tag" PLAYER-INVENTORY-SLOT-NBT-DATA(LK-PLAYER-ID, INVENTORY-INDEX) INT32
            END-IF

            CALL "NbtEncode-EndCompound" USING NBTENC BUFFER
        END-IF
    END-PERFORM
    CALL "NbtEncode-EndList" USING NBTENC BUFFER

    CALL "NbtEncode-Compound" USING NBTENC BUFFER "abilities"
    CALL "NbtEncode-Byte" USING NBTENC BUFFER "flying" PLAYER-FLYING(LK-PLAYER-ID)
    CALL "NbtEncode-EndCompound" USING NBTENC BUFFER

    *> end root tag
    CALL "NbtEncode-EndCompound" USING NBTENC BUFFER

    *> Create directories. Ignore errors, as they are likely to be caused by the directories already existing.
    CALL "CBL_CREATE_DIR" USING SP-LEVEL-NAME
    INITIALIZE STR
    STRING FUNCTION TRIM(SP-LEVEL-NAME) "/playerdata" INTO STR
    CALL "CBL_CREATE_DIR" USING STR

    *> write the data to disk in gzip-compressed form
    COMPUTE BUFFER-LENGTH = NBTENC-OFFSET - 1
    MOVE LENGTH OF COMPRESSED-BUFFER TO COMPRESSED-LENGTH
    CALL "GzipCompress" USING BUFFER BUFFER-LENGTH COMPRESSED-BUFFER COMPRESSED-LENGTH GIVING ERRNO
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
    01 BUFFER                   PIC X(64000).
    01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
    *> temporary data
    01 TAG                      PIC X(256).
    01 AT-END                   BINARY-CHAR UNSIGNED.
    01 INT8                     BINARY-CHAR.
    01 INT16                    BINARY-SHORT.
    01 INT32                    BINARY-LONG.
    01 STR                      PIC X(256).
    01 LEN                      BINARY-LONG UNSIGNED.
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
    CALL "Files-ReadAll" USING PLAYER-FILE-NAME BUFFER BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0 OR BUFFER-LENGTH = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> Check for the gzip magic number, and decompress if present
    IF BUFFER(1:2) = X"1F8B"
        MOVE BUFFER(1:BUFFER-LENGTH) TO COMPRESSED-BUFFER(1:BUFFER-LENGTH)
        MOVE BUFFER-LENGTH TO COMPRESSED-LENGTH
        MOVE LENGTH OF BUFFER TO BUFFER-LENGTH
        CALL "GzipDecompress" USING COMPRESSED-BUFFER COMPRESSED-LENGTH BUFFER BUFFER-LENGTH GIVING ERRNO
        IF ERRNO NOT = 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
    END-IF

    *> root tag
    MOVE 1 TO NBTDEC-OFFSET
    CALL "NbtDecode-RootCompound" USING NBTDEC BUFFER

    PERFORM UNTIL EXIT
        CALL "NbtDecode-Peek" USING NBTDEC BUFFER AT-END TAG
        IF AT-END = 1
            EXIT PERFORM
        END-IF
        EVALUATE TAG
            *> UUID (4 integers, MSB to LSB)
            WHEN "UUID"
                *> ignored, as we already have the UUID
                CALL "NbtDecode-Skip" USING NBTDEC BUFFER

            WHEN "playerGameType"
                CALL "NbtDecode-Byte" USING NBTDEC BUFFER PLAYER-GAMEMODE(LK-PLAYER-ID)

            WHEN "Pos"
                CALL "NbtDecode-List" USING NBTDEC BUFFER INT32
                CALL "NbtDecode-Double" USING NBTDEC BUFFER PLAYER-X(LK-PLAYER-ID)
                CALL "NbtDecode-Double" USING NBTDEC BUFFER PLAYER-Y(LK-PLAYER-ID)
                CALL "NbtDecode-Double" USING NBTDEC BUFFER PLAYER-Z(LK-PLAYER-ID)
                CALL "NbtDecode-EndList" USING NBTDEC BUFFER

            WHEN "Rotation"
                CALL "NbtDecode-List" USING NBTDEC BUFFER INT32
                CALL "NbtDecode-Float" USING NBTDEC BUFFER PLAYER-YAW(LK-PLAYER-ID)
                CALL "NbtDecode-Float" USING NBTDEC BUFFER PLAYER-PITCH(LK-PLAYER-ID)
                CALL "NbtDecode-EndList" USING NBTDEC BUFFER

            WHEN "OnGround"
                CALL "NbtDecode-Byte" USING NBTDEC BUFFER PLAYER-ON-GROUND(LK-PLAYER-ID)

            WHEN "FallDistance"
                CALL "NbtDecode-Float" USING NBTDEC BUFFER PLAYER-FALL-DISTANCE(LK-PLAYER-ID)

            WHEN "HurtTime"
                CALL "NbtDecode-Short" USING NBTDEC BUFFER INT16
                MOVE INT16 TO PLAYER-HURT-TIME(LK-PLAYER-ID)

            WHEN "Health"
                CALL "NbtDecode-Float" USING NBTDEC BUFFER PLAYER-HEALTH(LK-PLAYER-ID)

            WHEN "foodLevel"
                CALL "NbtDecode-Int" USING NBTDEC BUFFER PLAYER-FOOD-LEVEL(LK-PLAYER-ID)

            WHEN "foodSaturationLevel"
                CALL "NbtDecode-Float" USING NBTDEC BUFFER PLAYER-SATURATION(LK-PLAYER-ID)

            WHEN "XpLevel"
                CALL "NbtDecode-Int" USING NBTDEC BUFFER PLAYER-XP-LEVEL(LK-PLAYER-ID)

            WHEN "XpP"
                CALL "NbtDecode-Float" USING NBTDEC BUFFER PLAYER-XP-PROGRESS(LK-PLAYER-ID)

            WHEN "XpTotal"
                CALL "NbtDecode-Int" USING NBTDEC BUFFER PLAYER-XP-TOTAL(LK-PLAYER-ID)

            WHEN "SelectedItemSlot"
                CALL "NbtDecode-Byte" USING NBTDEC BUFFER PLAYER-HOTBAR(LK-PLAYER-ID)

            WHEN "Inventory"
                CALL "NbtDecode-List" USING NBTDEC BUFFER INT32
                PERFORM INT32 TIMES
                    *> Start of slot compound
                    CALL "NbtDecode-Compound" USING NBTDEC BUFFER
                    MOVE 0 TO INVENTORY-INDEX
                    INITIALIZE INVENTORY-SLOT
                    *> default structured components (0x00 = nothing to add, 0x00 = nothing to remove)
                    MOVE 2 TO INVENTORY-SLOT-NBT-LENGTH
                    MOVE X"0000" TO INVENTORY-SLOT-NBT-DATA(1:INVENTORY-SLOT-NBT-LENGTH)

                    *> Slot properties
                    PERFORM UNTIL EXIT
                        CALL "NbtDecode-Peek" USING NBTDEC BUFFER AT-END TAG
                        IF AT-END = 1
                            EXIT PERFORM
                        END-IF
                        EVALUATE TAG
                            WHEN "Slot"
                                CALL "NbtDecode-Byte" USING NBTDEC BUFFER INT8
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
                                CALL "NbtDecode-String" USING NBTDEC BUFFER STR LEN
                                CALL "Registries-Lookup" USING "minecraft:item" STR INVENTORY-SLOT-ID

                            WHEN "count"
                                CALL "NbtDecode-Byte" USING NBTDEC BUFFER INVENTORY-SLOT-COUNT

                            *> TODO use "components" instead, and decode it as a compound
                            WHEN "tag"
                                CALL "NbtDecode-ByteBuffer" USING NBTDEC BUFFER INVENTORY-SLOT-NBT-DATA INVENTORY-SLOT-NBT-LENGTH

                            WHEN OTHER
                                CALL "NbtDecode-Skip" USING NBTDEC BUFFER
                        END-EVALUATE
                    END-PERFORM

                    *> End of slot compound
                    CALL "NbtDecode-EndCompound" USING NBTDEC BUFFER

                    *> If the slot had complete data, set it in the player's inventory
                    IF INVENTORY-INDEX > 0 AND INVENTORY-SLOT-ID > 0 AND INVENTORY-SLOT-COUNT > 0
                        MOVE INVENTORY-SLOT-ID TO PLAYER-INVENTORY-SLOT-ID(LK-PLAYER-ID, INVENTORY-INDEX)
                        MOVE INVENTORY-SLOT-COUNT TO PLAYER-INVENTORY-SLOT-COUNT(LK-PLAYER-ID, INVENTORY-INDEX)
                        MOVE INVENTORY-SLOT-NBT-DATA(1:INVENTORY-SLOT-NBT-LENGTH) TO PLAYER-INVENTORY-SLOT-NBT-DATA(LK-PLAYER-ID, INVENTORY-INDEX)
                        MOVE INVENTORY-SLOT-NBT-LENGTH TO PLAYER-INVENTORY-SLOT-NBT-LENGTH(LK-PLAYER-ID, INVENTORY-INDEX)
                    END-IF
                END-PERFORM
                CALL "NbtDecode-EndList" USING NBTDEC BUFFER

            WHEN "abilities"
                *> Start of abilities compound
                CALL "NbtDecode-Compound" USING NBTDEC BUFFER
                PERFORM UNTIL EXIT
                    CALL "NbtDecode-Peek" USING NBTDEC BUFFER AT-END TAG
                    IF AT-END = 1
                        EXIT PERFORM
                    END-IF
                    EVALUATE TAG
                        WHEN "flying"
                            CALL "NbtDecode-Byte" USING NBTDEC BUFFER PLAYER-FLYING(LK-PLAYER-ID)
                        WHEN OTHER
                            CALL "NbtDecode-Skip" USING NBTDEC BUFFER
                    END-EVALUATE
                END-PERFORM
                *> End of abilities compound
                CALL "NbtDecode-EndCompound" USING NBTDEC BUFFER

            WHEN OTHER
                CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        END-EVALUATE
    END-PERFORM

    *> end root tag
    CALL "NbtDecode-EndCompound" USING NBTDEC BUFFER

    GOBACK.

END PROGRAM Players-LoadPlayer.
