*> --- RegisterItem-Slab ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Slab.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BLOCK-REGISTRY           BINARY-LONG.
    01 USE-PTR                  PROGRAM-POINTER.
    01 BLOCK-COUNT              BINARY-LONG UNSIGNED.
    01 BLOCK-ID                 BINARY-LONG UNSIGNED.
    01 BLOCK-NAME               PIC X(64).
    01 BLOCK-TYPE               PIC X(64).

PROCEDURE DIVISION.
    CALL "Registries-LookupRegistry" USING "minecraft:block" BLOCK-REGISTRY

    SET USE-PTR TO ENTRY "Callback-Use"

    *> Loop over all blocks and register the callback for each slab
    CALL "Registries-EntryCount" USING BLOCK-REGISTRY BLOCK-COUNT
    PERFORM VARYING BLOCK-ID FROM 0 BY 1 UNTIL BLOCK-ID >= BLOCK-COUNT
        CALL "Blocks-GetType" USING BLOCK-ID BLOCK-TYPE
        IF BLOCK-TYPE = "minecraft:slab"
            CALL "Registries-EntryName" USING BLOCK-REGISTRY BLOCK-ID BLOCK-NAME
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
        01 BLOCK-STATE              BINARY-LONG.
        01 CB-PTR-REPLACEABLE       PROGRAM-POINTER.
        *> hitbox for the block to place
        01 PLACE-AABB.
            COPY DD-AABB REPLACING LEADING ==PREFIX== BY ==PLACE==.
        01 IS-BOTTOM                BINARY-CHAR UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-SLOT LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-ITEM-NAME TO SLAB-NAME

        MOVE 2 TO SLAB-PROPERTY-COUNT
        MOVE "type" TO SLAB-PROPERTY-NAME(1)
        MOVE "waterlogged" TO SLAB-PROPERTY-NAME(2)
        MOVE "false" TO SLAB-PROPERTY-VALUE(2)

        CALL "Facing-ToString" USING LK-FACE FACING
        EVALUATE FACING
            WHEN "up"
                MOVE "bottom" TO SLAB-PROPERTY-VALUE(1)
                MOVE 1 TO IS-BOTTOM
            WHEN "down"
                MOVE "top" TO SLAB-PROPERTY-VALUE(1)
                MOVE 0 TO IS-BOTTOM
            WHEN OTHER
                IF LK-CURSOR-Y > 0.5
                    MOVE "top" TO SLAB-PROPERTY-VALUE(1)
                    MOVE 0 TO IS-BOTTOM
                ELSE
                    MOVE "bottom" TO SLAB-PROPERTY-VALUE(1)
                    MOVE 1 TO IS-BOTTOM
                END-IF
        END-EVALUATE

        *> The slab can be placed at the clicked position if the block there is replaceable.
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-STATE
        CALL "GetCallback-BlockReplaceable" USING BLOCK-STATE CB-PTR-REPLACEABLE
        CALL CB-PTR-REPLACEABLE USING BLOCK-STATE CHECK-RESULT
        IF CHECK-RESULT = 1
            PERFORM PlaceBlock
        END-IF

        *> Otherwise, check if the clicked position is a single slab, and double it.
        CALL "Blocks-ToDescription" USING BLOCK-STATE CURRENT-DESCRIPTION
        IF CURRENT-NAME = SLAB-NAME
            CALL "Blocks-Description-GetValue" USING CURRENT-DESCRIPTION "type" CURRENT-TYPE
            EVALUATE FACING ALSO CURRENT-TYPE
                WHEN "up" ALSO "bottom"
                    MOVE "double" TO SLAB-PROPERTY-VALUE(1)
                    MOVE 0 TO IS-BOTTOM
                    PERFORM PlaceBlock
                WHEN "down" ALSO "top"
                    MOVE "double" TO SLAB-PROPERTY-VALUE(1)
                    MOVE 1 TO IS-BOTTOM
                    PERFORM PlaceBlock
            END-EVALUATE
        END-IF

        *> Try the adjacent block (check replaceability).
        CALL "Facing-GetRelative" USING LK-FACE BLOCK-POSITION
        CALL "World-CheckBounds" USING BLOCK-POSITION CHECK-RESULT
        IF CHECK-RESULT NOT = 0
            GOBACK
        END-IF
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-STATE
        CALL "GetCallback-BlockReplaceable" USING BLOCK-STATE CB-PTR-REPLACEABLE
        CALL CB-PTR-REPLACEABLE USING BLOCK-STATE CHECK-RESULT
        IF CHECK-RESULT = 1
            PERFORM PlaceBlock
        END-IF

        *> Try doubling the adjacent block.
        CALL "Blocks-ToDescription" USING BLOCK-STATE CURRENT-DESCRIPTION
        IF CURRENT-NAME = SLAB-NAME
            CALL "Blocks-Description-GetValue" USING CURRENT-DESCRIPTION "type" CURRENT-TYPE
            *> Counter-intuitively, Minecraft does not care about the slab type (top/bottom) here.
            EVALUATE CURRENT-TYPE
                WHEN "double"
                    GOBACK
                WHEN "top"
                    MOVE 1 TO IS-BOTTOM
                WHEN "bottom"
                    MOVE 0 TO IS-BOTTOM
            END-EVALUATE
            MOVE "double" TO SLAB-PROPERTY-VALUE(1)
            PERFORM PlaceBlock
        END-IF

        *> No position found.
        GOBACK.

    PlaceBlock.
        *> Check for player collisison
        *> TODO make this more generic
        MOVE BLOCK-X TO PLACE-AABB-MIN-X
        MOVE BLOCK-Z TO PLACE-AABB-MIN-Z
        COMPUTE PLACE-AABB-MAX-X = BLOCK-X + 1
        COMPUTE PLACE-AABB-MAX-Z = BLOCK-Z + 1
        EVALUATE IS-BOTTOM
            WHEN 0
                COMPUTE PLACE-AABB-MIN-Y = BLOCK-Y + 0.5
                COMPUTE PLACE-AABB-MAX-Y = BLOCK-Y + 1.0
            WHEN 1
                MOVE BLOCK-Y TO PLACE-AABB-MIN-Y
                COMPUTE PLACE-AABB-MAX-Y = BLOCK-Y + 0.5
        END-EVALUATE
        CALL "CheckPlayerCollision" USING PLACE-AABB CHECK-RESULT
        IF CHECK-RESULT NOT = 0
            GOBACK
        END-IF

        CALL "Blocks-FromDescription" USING SLAB-DESCRIPTION BLOCK-STATE
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-STATE

        CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Slab.
