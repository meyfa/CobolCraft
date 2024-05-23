*> --- RegisterItem-Bucket ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Bucket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-BUCKET               PIC X(32) GLOBAL    VALUE "minecraft:bucket".
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
    CALL "SetCallback-ItemUse" USING C-MINECRAFT-BUCKET USE-PTR
    GOBACK.

    *> --- Callback-Use ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Use.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 C-LEVEL                  PIC X(5)                VALUE "level".
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        *> Block state description for the block currently in the world.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CURRENT==.
        01 FLUID-LEVEL              PIC X(32).
        01 BLOCK-ID                 BINARY-LONG.
        COPY DD-PLAYERS.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        CALL "Facing-GetRelative" USING LK-FACE BLOCK-POSITION

        *> Allow replacing water or lava source blocks (fluid level 0)
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID CURRENT-DESCRIPTION
        IF CURRENT-NAME NOT = C-MINECRAFT-WATER AND CURRENT-NAME NOT = C-MINECRAFT-LAVA
            GOBACK
        END-IF
        CALL "Blocks-Description-GetValue" USING CURRENT-DESCRIPTION C-LEVEL FLUID-LEVEL
        IF FLUID-LEVEL NOT = "0"
            GOBACK
        END-IF

        *> Place air
        CALL "Blocks-Get-DefaultStateId" USING C-MINECRAFT-AIR BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Bucket.
