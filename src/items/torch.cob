*> --- RegisterItem-Torch ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Torch.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-TORCH                PIC X(32) GLOBAL    VALUE "minecraft:torch".
    01 C-MINECRAFT-SOUL_TORCH           PIC X(32) GLOBAL    VALUE "minecraft:soul_torch".
    01 C-MINECRAFT-REDSTONE_TORCH       PIC X(32) GLOBAL    VALUE "minecraft:redstone_torch".
    01 C-MINECRAFT-WALL_TORCH           PIC X(32) GLOBAL    VALUE "minecraft:wall_torch".
    01 C-MINECRAFT-SOUL_WALL_TORCH      PIC X(32) GLOBAL    VALUE "minecraft:soul_wall_torch".
    01 C-MINECRAFT-REDSTONE_WALL_TORCH  PIC X(32) GLOBAL    VALUE "minecraft:redstone_wall_torch".
    01 USE-PTR                          PROGRAM-POINTER.

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"
    CALL "SetCallback-ItemUse" USING C-MINECRAFT-TORCH USE-PTR
    CALL "SetCallback-ItemUse" USING C-MINECRAFT-SOUL_TORCH USE-PTR
    CALL "SetCallback-ItemUse" USING C-MINECRAFT-REDSTONE_TORCH USE-PTR
    GOBACK.

    *> --- Callback-Use ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Use.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        *> Block state description for the wall torch variant.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==WALL_TORCH==.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(5).
        01 BOUNDS-CHECK             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> TODO reduce duplication with other callbacks

        *> Compute the position of the block to be affected
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "Facing-GetRelative" USING LK-FACE BLOCK-POSITION
        CALL "Facing-ToString" USING LK-FACE FACING

        *> Ensure the position is not outside the world
        CALL "World-CheckBounds" USING BLOCK-POSITION BOUNDS-CHECK
        IF BOUNDS-CHECK NOT = 0
            GOBACK
        END-IF

        *> Ensure the block was previously air
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        IF BLOCK-ID NOT = 0
            GOBACK
        END-IF

        *> TODO: check for solid block where the torch will be placed

        IF FACING = "up" OR FACING = "down"
            CALL "Blocks-Get-DefaultStateId" USING LK-ITEM-NAME BLOCK-ID
            CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID
        ELSE
            *> use the correct wall torch type
            EVALUATE LK-ITEM-NAME
                WHEN C-MINECRAFT-TORCH
                    MOVE C-MINECRAFT-WALL_TORCH TO WALL_TORCH-NAME
                WHEN C-MINECRAFT-SOUL_TORCH
                    MOVE C-MINECRAFT-SOUL_WALL_TORCH TO WALL_TORCH-NAME
                WHEN C-MINECRAFT-REDSTONE_TORCH
                    MOVE C-MINECRAFT-REDSTONE_WALL_TORCH TO WALL_TORCH-NAME
            END-EVALUATE

            MOVE 1 TO WALL_TORCH-PROPERTY-COUNT
            MOVE "facing" TO WALL_TORCH-PROPERTY-NAME(1)
            MOVE FACING TO WALL_TORCH-PROPERTY-VALUE(1)

            CALL "Blocks-Get-StateId" USING WALL_TORCH-DESCRIPTION BLOCK-ID
            CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID
        END-IF

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Torch.
