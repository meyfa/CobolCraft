*> --- RegisterItem-Stairs ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Stairs.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-STAIR                PIC X(32) GLOBAL    VALUE "minecraft:stair".
    01 USE-PTR                          PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-NAME                       PIC X(64).
    01 BLOCK-TYPE                       PIC X(64).

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"

    *> Loop over all blocks and register the callback for each matching block type
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        IF BLOCK-TYPE = C-MINECRAFT-STAIR
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
        01 C-HALF                   PIC X(4)                VALUE "half".
        01 C-FACING                 PIC X(6)                VALUE "facing".
        01 C-SHAPE                  PIC X(5)                VALUE "shape".
        01 C-WATERLOGGED            PIC X(11)               VALUE "waterlogged".
        *> Block state description for the block to place.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==PLACE==.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(16).
        01 BLOCK-ID                 BINARY-LONG.
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-ITEM-NAME TO PLACE-NAME

        *> Compute the position of the block to be affected
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        MOVE 4 TO PLACE-PROPERTY-COUNT
        MOVE C-HALF TO PLACE-PROPERTY-NAME(1)
        MOVE C-FACING TO PLACE-PROPERTY-NAME(2)
        MOVE C-SHAPE TO PLACE-PROPERTY-NAME(3)
        MOVE "straight" TO PLACE-PROPERTY-VALUE(3)
        MOVE C-WATERLOGGED TO PLACE-PROPERTY-NAME(4)
        MOVE "false" TO PLACE-PROPERTY-VALUE(4)

        *> Which stair half is placed (top or bottom) depends on where the player clicked
        CALL "Facing-ToString" USING LK-FACE FACING
        EVALUATE FACING
            WHEN "up"
                MOVE "bottom" TO PLACE-PROPERTY-VALUE(1)
            WHEN "down"
                MOVE "top" TO PLACE-PROPERTY-VALUE(1)
            WHEN OTHER
                IF LK-CURSOR-Y > 0.5
                    MOVE "top" TO PLACE-PROPERTY-VALUE(1)
                ELSE
                    MOVE "bottom" TO PLACE-PROPERTY-VALUE(1)
                END-IF
        END-EVALUATE

        *> Which way the stairs face depends on the player's rotation
        EVALUATE FUNCTION MOD(PLAYER-YAW(LK-PLAYER) + 45, 360)
            WHEN < 90
                MOVE "south" TO PLACE-PROPERTY-VALUE(2)
            WHEN < 180
                MOVE "west" TO PLACE-PROPERTY-VALUE(2)
            WHEN < 270
                MOVE "north" TO PLACE-PROPERTY-VALUE(2)
            WHEN OTHER
                MOVE "east" TO PLACE-PROPERTY-VALUE(2)
        END-EVALUATE

        *> TODO connect with neighboring stairs

        CALL "Blocks-Get-StateId" USING PLACE-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Stairs.
