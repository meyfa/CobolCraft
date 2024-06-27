*> --- RegisterItem-Slab ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Slab.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-SLAB                 PIC X(32) GLOBAL    VALUE "minecraft:slab".
    01 USE-PTR                          PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-NAME                       PIC X(64).
    01 BLOCK-TYPE                       PIC X(64).

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"

    *> Loop over all blocks and register the callback for each slab
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        IF BLOCK-TYPE = C-MINECRAFT-SLAB
            CALL "Blocks-Iterate-Name" USING BLOCK-INDEX BLOCK-NAME
            CALL "SetCallback-ItemUse" USING BLOCK-NAME USE-PTR
        END-IF
    END-PERFORM

    GOBACK.

    *> --- Callback-Use ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Use.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 C-TYPE                   PIC X(4)                VALUE "type".
        *> Block state description for the block currently in the world.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CURRENT==.
        01 CURRENT-TYPE             PIC X(16).
        *> Block state description for the block to place.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==SLAB==.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(16).
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
        01 CB-PTR-REPLACEABLE       PROGRAM-POINTER.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-ITEM-NAME TO SLAB-NAME

        MOVE 2 TO SLAB-PROPERTY-COUNT
        MOVE C-TYPE TO SLAB-PROPERTY-NAME(1)
        MOVE "waterlogged" TO SLAB-PROPERTY-NAME(2)
        MOVE "false" TO SLAB-PROPERTY-VALUE(2)

        CALL "Facing-ToString" USING LK-FACE FACING
        EVALUATE FACING
            WHEN "up"
                MOVE "bottom" TO SLAB-PROPERTY-VALUE(1)
            WHEN "down"
                MOVE "top" TO SLAB-PROPERTY-VALUE(1)
            WHEN OTHER
                IF LK-CURSOR-Y > 0.5
                    MOVE "top" TO SLAB-PROPERTY-VALUE(1)
                ELSE
                    MOVE "bottom" TO SLAB-PROPERTY-VALUE(1)
                END-IF
        END-EVALUATE

        *> The slab can be placed at the clicked position if the block there is replaceable.
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        CALL "GetCallback-BlockReplaceable" USING BLOCK-ID CB-PTR-REPLACEABLE
        CALL CB-PTR-REPLACEABLE USING BLOCK-ID CHECK-RESULT
        IF CHECK-RESULT = 1
            PERFORM PlaceBlock
        END-IF

        *> Otherwise, check if the clicked position is a single slab, and double it.
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID CURRENT-DESCRIPTION
        IF CURRENT-NAME = SLAB-NAME
            CALL "Blocks-Description-GetValue" USING CURRENT-DESCRIPTION C-TYPE CURRENT-TYPE
            EVALUATE FACING ALSO CURRENT-TYPE
                WHEN "up" ALSO "bottom"
                    MOVE "double" TO SLAB-PROPERTY-VALUE(1)
                    PERFORM PlaceBlock
                WHEN "down" ALSO "top"
                    MOVE "double" TO SLAB-PROPERTY-VALUE(1)
                    PERFORM PlaceBlock
            END-EVALUATE
        END-IF

        *> Try the adjacent block (check replaceability).
        CALL "Facing-GetRelative" USING LK-FACE BLOCK-POSITION
        CALL "World-CheckBounds" USING BLOCK-POSITION CHECK-RESULT
        IF CHECK-RESULT NOT = 0
            GOBACK
        END-IF
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        CALL "GetCallback-BlockReplaceable" USING BLOCK-ID CB-PTR-REPLACEABLE
        CALL CB-PTR-REPLACEABLE USING BLOCK-ID CHECK-RESULT
        IF CHECK-RESULT = 1
            PERFORM PlaceBlock
        END-IF

        *> Try doubling the adjacent block.
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID CURRENT-DESCRIPTION
        IF CURRENT-NAME = SLAB-NAME
            CALL "Blocks-Description-GetValue" USING CURRENT-DESCRIPTION C-TYPE CURRENT-TYPE
            *> Counter-intuitively, Minecraft does not care about the slab type (top/bottom) here.
            IF CURRENT-TYPE = "double"
                GOBACK
            END-IF
            MOVE "double" TO SLAB-PROPERTY-VALUE(1)
            PERFORM PlaceBlock
        END-IF

        *> No position found.
        GOBACK.

    PlaceBlock SECTION.
        CALL "Blocks-Get-StateId" USING SLAB-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID
        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Slab.
