*> --- RegisterItem-Stairs ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Stairs.

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

    *> Loop over all blocks and register the callback for each matching block type
    CALL "Registries-EntryCount" USING BLOCK-REGISTRY BLOCK-COUNT
    PERFORM VARYING BLOCK-ID FROM 0 BY 1 UNTIL BLOCK-ID >= BLOCK-COUNT
        CALL "Blocks-GetType" USING BLOCK-ID BLOCK-TYPE
        IF BLOCK-TYPE = "minecraft:stair"
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

    PROCEDURE DIVISION USING LK-PLAYER LK-SLOT LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-ITEM-NAME TO PLACE-NAME

        *> Compute the position of the block to be affected
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        MOVE 4 TO PLACE-PROPERTY-COUNT
        MOVE "half" TO PLACE-PROPERTY-NAME(1)
        MOVE "facing" TO PLACE-PROPERTY-NAME(2)
        MOVE "shape" TO PLACE-PROPERTY-NAME(3)
        MOVE "straight" TO PLACE-PROPERTY-VALUE(3)
        MOVE "waterlogged" TO PLACE-PROPERTY-NAME(4)
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

        CALL "Blocks-FromDescription" USING PLACE-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID

        CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Stairs.
