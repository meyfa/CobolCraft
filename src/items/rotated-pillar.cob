*> --- RegisterItem-RotatedPillar ---
*> This is responsible for all rotated pillars, such as logs, basalt, bone blocks, etc.
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-RotatedPillar.

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

    *> Loop over all blocks and register the callback for each rotated pillar
    CALL "Registries-EntryCount" USING BLOCK-REGISTRY BLOCK-COUNT
    PERFORM VARYING BLOCK-ID FROM 0 BY 1 UNTIL BLOCK-ID >= BLOCK-COUNT
        CALL "Blocks-GetType" USING BLOCK-ID BLOCK-TYPE
        IF BLOCK-TYPE = "minecraft:rotated_pillar"
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
        *> Block state description for the pillar variant.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==PLACE==.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(5).
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
        01 PLACE-AABB.
            COPY DD-AABB REPLACING LEADING ==PREFIX== BY ==PLACE==.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-SLOT LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> TODO reduce duplication with other callbacks

        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        *> Check for player collisison
        *> TODO make this more generic
        MOVE BLOCK-X TO PLACE-AABB-MIN-X
        MOVE BLOCK-Y TO PLACE-AABB-MIN-Y
        MOVE BLOCK-Z TO PLACE-AABB-MIN-Z
        COMPUTE PLACE-AABB-MAX-X = BLOCK-X + 1
        COMPUTE PLACE-AABB-MAX-Y = BLOCK-Y + 1
        COMPUTE PLACE-AABB-MAX-Z = BLOCK-Z + 1
        CALL "CheckPlayerCollision" USING PLACE-AABB CHECK-RESULT
        IF CHECK-RESULT NOT = 0
            GOBACK
        END-IF

        CALL "Facing-ToString" USING LK-FACE FACING

        MOVE LK-ITEM-NAME TO PLACE-NAME
        MOVE 1 TO PLACE-PROPERTY-COUNT
        MOVE "axis" TO PLACE-PROPERTY-NAME(1)
        EVALUATE TRUE
            WHEN FACING = "up" OR FACING = "down"
                MOVE "y" TO PLACE-PROPERTY-VALUE(1)
            WHEN FACING = "north" OR FACING = "south"
                MOVE "z" TO PLACE-PROPERTY-VALUE(1)
            WHEN FACING = "east" OR FACING = "west"
                MOVE "x" TO PLACE-PROPERTY-VALUE(1)
        END-EVALUATE

        CALL "Blocks-FromDescription" USING PLACE-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID

        CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-RotatedPillar.
