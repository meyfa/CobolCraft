*> --- RegisterItem-Bucket ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Bucket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 USE-PTR                  PROGRAM-POINTER.
    01 BLOCK-COUNT              BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX              BINARY-LONG UNSIGNED.
    01 BLOCK-NAME               PIC X(64).
    01 BLOCK-TYPE               PIC X(64).

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"
    CALL "SetCallback-ItemUse" USING "minecraft:bucket" USE-PTR
    GOBACK.

    *> --- Callback-Use ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Use.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CURRENT==.
        01 FLUID-LEVEL              PIC X(32).
        01 BLOCK-ID                 BINARY-LONG.
        01 BLOCK-STATE              BINARY-LONG.
        01 ITEM-ID                  BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-SLOT LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-STATE
        CALL "Facing-GetRelative" USING LK-FACE BLOCK-POSITION

        *> Allow replacing water or lava source blocks (fluid level 0)
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-STATE
        CALL "Blocks-ToDescription" USING BLOCK-STATE CURRENT-DESCRIPTION
        IF CURRENT-NAME NOT = "minecraft:water" AND CURRENT-NAME NOT = "minecraft:lava"
            GOBACK
        END-IF
        CALL "Blocks-Description-GetValue" USING CURRENT-DESCRIPTION "level" FLUID-LEVEL
        IF FLUID-LEVEL NOT = "0"
            GOBACK
        END-IF

        *> Place air
        CALL "Registries-Lookup" USING "minecraft:block" "minecraft:air" BLOCK-ID
        CALL "Blocks-GetDefaultStateId" USING BLOCK-ID BLOCK-STATE
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-STATE

        *> Replace the bucket with a water or lava bucket
        IF PLAYER-GAMEMODE(LK-PLAYER) NOT = 1
            EVALUATE CURRENT-NAME
                WHEN "minecraft:water"
                    CALL "Registries-Lookup" USING "minecraft:item" "minecraft:water_bucket" ITEM-ID
                WHEN "minecraft:lava"
                    CALL "Registries-Lookup" USING "minecraft:item" "minecraft:lava_bucket" ITEM-ID
            END-EVALUATE

            IF ITEM-ID >= 0
                CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT ITEM-ID
            END-IF
        END-IF

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Bucket.
