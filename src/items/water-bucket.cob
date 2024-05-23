*> --- RegisterItem-WaterBucket ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-WaterBucket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-WATER_BUCKET         PIC X(32) GLOBAL    VALUE "minecraft:water_bucket".
    01 C-MINECRAFT-WATER                PIC X(32) GLOBAL    VALUE "minecraft:water".
    01 C-MINECRAFT-LAVA                 PIC X(32) GLOBAL    VALUE "minecraft:lava".
    01 C-MINECRAFT-AIR                  PIC X(32) GLOBAL    VALUE "minecraft:air".
    01 USE-PTR                          PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-NAME                       PIC X(64).
    01 BLOCK-TYPE                       PIC X(64).

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"
    CALL "SetCallback-ItemUse" USING C-MINECRAFT-WATER_BUCKET USE-PTR
    GOBACK.

    *> --- Callback-Use ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Use.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        *> Block state description for the block currently in the world.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CURRENT==.
        01 BLOCK-ID                 BINARY-LONG.
        COPY DD-PLAYERS.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> Compute the position of the block to be affected
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "Facing-GetRelative" USING LK-FACE BLOCK-POSITION

        *> TODO allow replacing some blocks other than air (grass, etc.)
        *> TODO implement waterlogging

        *> Allow replacing air, water, or lava
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID CURRENT-DESCRIPTION
        IF CURRENT-NAME NOT = C-MINECRAFT-AIR AND CURRENT-NAME NOT = C-MINECRAFT-WATER AND CURRENT-NAME NOT = C-MINECRAFT-LAVA
            GOBACK
        END-IF

        *> Place water
        CALL "Blocks-Get-DefaultStateId" USING C-MINECRAFT-WATER BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-WaterBucket.
