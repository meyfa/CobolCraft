*> --- RegisterItem-Torch ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Torch.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 USE-PTR                  PROGRAM-POINTER.

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"
    CALL "SetCallback-ItemUse" USING "minecraft:torch" USE-PTR
    CALL "SetCallback-ItemUse" USING "minecraft:soul_torch" USE-PTR
    CALL "SetCallback-ItemUse" USING "minecraft:redstone_torch" USE-PTR
    GOBACK.

    *> --- Callback-Use ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Use.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        *> Block state description for the wall torch variant.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==WALL_TORCH==.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(5).
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
        01 BLOCK-STATE              BINARY-LONG.
        01 TARGET-BLOCK-POSITION.
            02 TARGET-BLOCK-X       BINARY-LONG.
            02 TARGET-BLOCK-Y       BINARY-LONG.
            02 TARGET-BLOCK-Z       BINARY-LONG.
        01 FACE-CALLBACK            PROGRAM-POINTER.
        01 BLOCK-FACE               BINARY-CHAR UNSIGNED.
        COPY DD-PLAYERS.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-SLOT LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> TODO reduce duplication with other callbacks

        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        CALL "Facing-ToString" USING LK-FACE FACING

        *> TODO change torch facing if not legal (missing solid block) while another facing would be legal

        IF FACING = "up" OR "down"
            *> Check for solid block below
            MOVE BLOCK-POSITION TO TARGET-BLOCK-POSITION
            SUBTRACT 1 FROM TARGET-BLOCK-Y
            CALL "World-GetBlock" USING TARGET-BLOCK-POSITION BLOCK-STATE
            CALL "GetCallback-BlockFace" USING BLOCK-STATE FACE-CALLBACK
            CALL FACE-CALLBACK USING BLOCK-STATE "up" BLOCK-FACE
            IF BLOCK-FACE = 0
                GOBACK
            END-IF

            *> Place the torch
            CALL "Registries-Lookup" USING "minecraft:block" LK-ITEM-NAME BLOCK-ID
            CALL "Blocks-GetDefaultStateId" USING BLOCK-ID BLOCK-STATE
            CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-STATE

            CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT
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
            CALL "World-GetBlock" USING TARGET-BLOCK-POSITION BLOCK-STATE
            CALL "GetCallback-BlockFace" USING BLOCK-STATE FACE-CALLBACK
            CALL FACE-CALLBACK USING BLOCK-STATE FACING BLOCK-FACE
            IF BLOCK-FACE = 0
                GOBACK
            END-IF

            *> Use the correct wall torch type
            EVALUATE LK-ITEM-NAME
                WHEN "minecraft:torch"
                    MOVE "minecraft:wall_torch" TO WALL_TORCH-NAME
                WHEN "minecraft:soul_torch"
                    MOVE "minecraft:soul_wall_torch" TO WALL_TORCH-NAME
                WHEN "minecraft:redstone_torch"
                    MOVE "minecraft:redstone_wall_torch" TO WALL_TORCH-NAME
            END-EVALUATE

            MOVE 1 TO WALL_TORCH-PROPERTY-COUNT
            MOVE "facing" TO WALL_TORCH-PROPERTY-NAME(1)
            MOVE FACING TO WALL_TORCH-PROPERTY-VALUE(1)

            CALL "Blocks-FromDescription" USING WALL_TORCH-DESCRIPTION BLOCK-STATE
            CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-STATE

            CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT
        END-IF

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Torch.
