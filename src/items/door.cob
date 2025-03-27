*> --- RegisterItem-Door ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Door.

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

    *> Loop over all blocks and register the callback for each door
    CALL "Registries-EntryCount" USING BLOCK-REGISTRY BLOCK-COUNT
    PERFORM VARYING BLOCK-ID FROM 0 BY 1 UNTIL BLOCK-ID >= BLOCK-COUNT
        CALL "Blocks-GetType" USING BLOCK-ID BLOCK-TYPE
        IF BLOCK-TYPE = "minecraft:door"
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
        *> Block state description for the block to place.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==PLACE==.
        01 BLOCK-POSITION-LOWER.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 BLOCK-POSITION-UPPER.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(16).
        01 HINGE                    PIC X(16).
        01 BLOCK-STATE              BINARY-LONG.
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        01 CB-PTR-REPLACEABLE       PROGRAM-POINTER.
        *> Block state description for neighboring blocks.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==NEIGHBOR==.
        01 NEIGHBOR-LEFT-POSITION.
            02 NEIGHBOR-LEFT-X      BINARY-LONG.
            02 NEIGHBOR-LEFT-Y      BINARY-LONG.
            02 NEIGHBOR-LEFT-Z      BINARY-LONG.
        01 NEIGHBOR-RIGHT-POSITION.
            02 NEIGHBOR-RIGHT-X     BINARY-LONG.
            02 NEIGHBOR-RIGHT-Y     BINARY-LONG.
            02 NEIGHBOR-RIGHT-Z     BINARY-LONG.
        01 NEIGHBOR-FACING          PIC X(16).
        01 NEIGHBOR-HINGE           PIC X(16).
        01 NEIGHBOR-HALF            PIC X(16).
        01 HAS-DOOR-LEFT            BINARY-CHAR UNSIGNED.
        01 HAS-DOOR-RIGHT           BINARY-CHAR UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-SLOT LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> TODO reduce duplication with other callbacks

        *> Compute the position of the lower block
        MOVE LK-POSITION TO BLOCK-POSITION-LOWER
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION-LOWER LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        *> Compute the position of the upper block
        MOVE BLOCK-POSITION-LOWER TO BLOCK-POSITION-UPPER
        ADD 1 TO BLOCK-Y OF BLOCK-POSITION-UPPER
        CALL "World-GetBlock" USING BLOCK-POSITION-UPPER BLOCK-STATE
        CALL "GetCallback-BlockReplaceable" USING BLOCK-STATE CB-PTR-REPLACEABLE
        CALL CB-PTR-REPLACEABLE USING BLOCK-STATE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        MOVE LK-ITEM-NAME TO PLACE-NAME

        MOVE 5 TO PLACE-PROPERTY-COUNT
        *> facing, hinge: set depending on player facing and cursor position; half: set when placing the blocks
        MOVE "facing" TO PLACE-PROPERTY-NAME(1)
        MOVE "hinge" TO PLACE-PROPERTY-NAME(2)
        MOVE "half" TO PLACE-PROPERTY-NAME(3)
        *> open, powered: set to false
        MOVE "open" TO PLACE-PROPERTY-NAME(4)
        MOVE "false" TO PLACE-PROPERTY-VALUE(4)
        MOVE "powered" TO PLACE-PROPERTY-NAME(5)
        MOVE "false" TO PLACE-PROPERTY-VALUE(5)

        *> Use the player's yaw to determine the facing
        EVALUATE FUNCTION MOD(PLAYER-YAW(LK-PLAYER) + 45, 360)
            WHEN < 90
                MOVE "south" TO FACING
            WHEN < 180
                MOVE "west" TO FACING
            WHEN < 270
                MOVE "north" TO FACING
            WHEN OTHER
                MOVE "east" TO FACING
        END-EVALUATE

        *> Use the click position to determine the hinge
        EVALUATE TRUE
            WHEN FACING = "north" AND LK-CURSOR-X < 0.5
                MOVE "left" TO HINGE
            WHEN FACING = "north"
                MOVE "right" TO HINGE
            WHEN FACING = "east" AND LK-CURSOR-Z < 0.5
                MOVE "left" TO HINGE
            WHEN FACING = "east"
                MOVE "right" TO HINGE
            WHEN FACING = "south" AND LK-CURSOR-X < 0.5
                MOVE "right" TO HINGE
            WHEN FACING = "south"
                MOVE "left" TO HINGE
            WHEN FACING = "west" AND LK-CURSOR-Z < 0.5
                MOVE "right" TO HINGE
            WHEN FACING = "west"
                MOVE "left" TO HINGE
        END-EVALUATE

        *> Check for matching door blocks to the left and right
        MOVE BLOCK-POSITION-LOWER TO NEIGHBOR-LEFT-POSITION
        MOVE BLOCK-POSITION-LOWER TO NEIGHBOR-RIGHT-POSITION
        EVALUATE FACING
            WHEN "north"
                ADD 1 TO NEIGHBOR-LEFT-X
                SUBTRACT 1 FROM NEIGHBOR-RIGHT-X
            WHEN "east"
                SUBTRACT 1 FROM NEIGHBOR-LEFT-Z
                ADD 1 TO NEIGHBOR-RIGHT-Z
            WHEN "south"
                SUBTRACT 1 FROM NEIGHBOR-LEFT-X
                ADD 1 TO NEIGHBOR-RIGHT-X
            WHEN "west"
                ADD 1 TO NEIGHBOR-LEFT-Z
                SUBTRACT 1 FROM NEIGHBOR-RIGHT-Z
        END-EVALUATE

        *> left
        CALL "World-GetBlock" USING NEIGHBOR-LEFT-POSITION BLOCK-STATE
        CALL "Blocks-ToDescription" USING BLOCK-STATE NEIGHBOR-DESCRIPTION
        MOVE 0 TO HAS-DOOR-LEFT
        IF NEIGHBOR-NAME = PLACE-NAME
            CALL "Blocks-Description-GetValue" USING NEIGHBOR-DESCRIPTION "facing" NEIGHBOR-FACING
            CALL "Blocks-Description-GetValue" USING NEIGHBOR-DESCRIPTION "hinge" NEIGHBOR-HINGE
            CALL "Blocks-Description-GetValue" USING NEIGHBOR-DESCRIPTION "half" NEIGHBOR-HALF
            IF NEIGHBOR-FACING = FACING AND NEIGHBOR-HINGE = "left" AND NEIGHBOR-HALF = "lower"
                MOVE 1 TO HAS-DOOR-LEFT
            END-IF
        END-IF

        *> right
        CALL "World-GetBlock" USING NEIGHBOR-RIGHT-POSITION BLOCK-STATE
        CALL "Blocks-ToDescription" USING BLOCK-STATE NEIGHBOR-DESCRIPTION
        MOVE 0 TO HAS-DOOR-RIGHT
        IF NEIGHBOR-NAME = PLACE-NAME
            CALL "Blocks-Description-GetValue" USING NEIGHBOR-DESCRIPTION "facing" NEIGHBOR-FACING
            CALL "Blocks-Description-GetValue" USING NEIGHBOR-DESCRIPTION "hinge" NEIGHBOR-HINGE
            CALL "Blocks-Description-GetValue" USING NEIGHBOR-DESCRIPTION "half" NEIGHBOR-HALF
            IF NEIGHBOR-FACING = FACING AND NEIGHBOR-HINGE = "right" AND NEIGHBOR-HALF = "lower"
                MOVE 1 TO HAS-DOOR-RIGHT
            END-IF
        END-IF

        *> The hinge may switch sides in case there is a door on the other side, but not on the current side.
        EVALUATE TRUE
            WHEN HINGE = "left" AND HAS-DOOR-LEFT = 1 AND HAS-DOOR-RIGHT = 0
                MOVE "right" TO HINGE
            WHEN HINGE = "right" AND HAS-DOOR-LEFT = 0 AND HAS-DOOR-RIGHT = 1
                MOVE "left" TO HINGE
        END-EVALUATE

        MOVE FACING TO PLACE-PROPERTY-VALUE(1)
        MOVE HINGE TO PLACE-PROPERTY-VALUE(2)

        *> TODO check for solid block below

        *> Place the door
        MOVE "lower" TO PLACE-PROPERTY-VALUE(3)
        CALL "Blocks-FromDescription" USING PLACE-DESCRIPTION BLOCK-STATE
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION-LOWER BLOCK-STATE
        MOVE "upper" TO PLACE-PROPERTY-VALUE(3)
        CALL "Blocks-FromDescription" USING PLACE-DESCRIPTION BLOCK-STATE
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION-UPPER BLOCK-STATE

        CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Door.
