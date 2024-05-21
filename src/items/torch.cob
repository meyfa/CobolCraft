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
        01 FACING-UP                PIC X(4) GLOBAL         VALUE "up".
        *> Block state description for the wall torch variant.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==WALL_TORCH==.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(5).
        01 BOUNDS-CHECK             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
        01 TARGET-BLOCK-POSITION.
            02 TARGET-BLOCK-X       BINARY-LONG.
            02 TARGET-BLOCK-Y       BINARY-LONG.
            02 TARGET-BLOCK-Z       BINARY-LONG.
        01 FACE-CALLBACK            PROGRAM-POINTER.
        01 BLOCK-FACE               BINARY-CHAR UNSIGNED.
        COPY DD-PLAYERS.
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

        *> TODO change torch facing if not legal (missing solid block) while another facing would be legal

        IF FACING = "up" OR FACING = "down"
            *> Check for solid block below
            MOVE BLOCK-POSITION TO TARGET-BLOCK-POSITION
            SUBTRACT 1 FROM TARGET-BLOCK-Y
            CALL "World-GetBlock" USING TARGET-BLOCK-POSITION BLOCK-ID
            CALL "GetCallback-BlockFace" USING BLOCK-ID FACE-CALLBACK
            CALL FACE-CALLBACK USING BLOCK-ID FACING-UP BLOCK-FACE
            IF BLOCK-FACE = 0
                GOBACK
            END-IF

            *> Place the torch
            CALL "Blocks-Get-DefaultStateId" USING LK-ITEM-NAME BLOCK-ID
            CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID
        ELSE
            *> Check for solid block where the torch would attach
            MOVE BLOCK-POSITION TO TARGET-BLOCK-POSITION
            EVALUATE FACING
                WHEN "north"
                    ADD 1 TO TARGET-BLOCK-Z
                WHEN "south"
                    SUBTRACT 1 FROM TARGET-BLOCK-Z
                WHEN "east"
                    SUBTRACT 1 FROM TARGET-BLOCK-X
                WHEN "west"
                    ADD 1 TO TARGET-BLOCK-X
            END-EVALUATE
            CALL "World-GetBlock" USING TARGET-BLOCK-POSITION BLOCK-ID
            CALL "GetCallback-BlockFace" USING BLOCK-ID FACE-CALLBACK
            CALL FACE-CALLBACK USING BLOCK-ID FACING BLOCK-FACE
            IF BLOCK-FACE = 0
                GOBACK
            END-IF

            *> Use the correct wall torch type
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
