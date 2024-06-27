*> --- RegisterItem-LavaBucket ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-LavaBucket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-LAVA_BUCKET          PIC X(32) GLOBAL    VALUE "minecraft:lava_bucket".
    01 C-MINECRAFT-LAVA                 PIC X(32) GLOBAL    VALUE "minecraft:lava".
    01 USE-PTR                          PROGRAM-POINTER.

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"
    CALL "SetCallback-ItemUse" USING C-MINECRAFT-LAVA_BUCKET USE-PTR
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
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        COPY DD-PLAYERS.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        *> Place the fluid
        CALL "Blocks-Get-DefaultStateId" USING C-MINECRAFT-LAVA BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-LavaBucket.
