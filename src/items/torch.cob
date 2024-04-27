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
        *> TODO move this to a copybook somehow
        01 WALL_TORCH-DESCRIPTOR.
            02 WALL_TORCH-NAME              PIC X(64).
            02 WALL_TORCH-PROPS             BINARY-LONG UNSIGNED.
            02 WALL_TORCH-PROP OCCURS 16 TIMES.
                03 WALL_TORCH-PROP-NAME     PIC X(64).
                03 WALL_TORCH-PROP-VALUE    PIC X(64).
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 BOUNDS-CHECK             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE.
        *> TODO reduce duplication with other callbacks

        EVALUATE LK-ITEM-NAME
            WHEN C-MINECRAFT-TORCH
                MOVE C-MINECRAFT-WALL_TORCH TO WALL_TORCH-NAME
            WHEN C-MINECRAFT-SOUL_TORCH
                MOVE C-MINECRAFT-SOUL_WALL_TORCH TO WALL_TORCH-NAME
            WHEN C-MINECRAFT-REDSTONE_TORCH
                MOVE C-MINECRAFT-REDSTONE_WALL_TORCH TO WALL_TORCH-NAME
        END-EVALUATE

        MOVE 1 TO WALL_TORCH-PROPS
        MOVE "facing" TO WALL_TORCH-PROP-NAME(1)

        *> Compute the position of the block to be affected
        MOVE LK-POSITION TO BLOCK-POSITION
        EVALUATE LK-FACE
            WHEN 0
                COMPUTE BLOCK-Y = BLOCK-Y - 1
            WHEN 1
                COMPUTE BLOCK-Y = BLOCK-Y + 1
            WHEN 2
                COMPUTE BLOCK-Z = BLOCK-Z - 1
                MOVE "north" TO WALL_TORCH-PROP-VALUE(1)
            WHEN 3
                COMPUTE BLOCK-Z = BLOCK-Z + 1
                MOVE "south" TO WALL_TORCH-PROP-VALUE(1)
            WHEN 4
                COMPUTE BLOCK-X = BLOCK-X - 1
                MOVE "west" TO WALL_TORCH-PROP-VALUE(1)
            WHEN 5
                COMPUTE BLOCK-X = BLOCK-X + 1
                MOVE "east" TO WALL_TORCH-PROP-VALUE(1)
        END-EVALUATE

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

        IF LK-FACE = 0 OR LK-FACE = 1
            CALL "Blocks-Get-DefaultStateId" USING LK-ITEM-NAME BLOCK-ID
            CALL "World-SetBlock" USING BLOCK-POSITION BLOCK-ID
        ELSE
            CALL "Blocks-Get-StateId" USING WALL_TORCH-DESCRIPTOR BLOCK-ID
            CALL "World-SetBlock" USING BLOCK-POSITION BLOCK-ID
        END-IF

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Torch.
