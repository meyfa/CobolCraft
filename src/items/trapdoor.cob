*> --- RegisterItem-Trapdoor ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Trapdoor.

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

    *> Loop over all blocks and register the callback for each trapdoor
    CALL "Registries-EntryCount" USING BLOCK-REGISTRY BLOCK-COUNT
    PERFORM VARYING BLOCK-ID FROM 0 BY 1 UNTIL BLOCK-ID >= BLOCK-COUNT
        CALL "Blocks-GetType" USING BLOCK-ID BLOCK-TYPE
        IF BLOCK-TYPE = "minecraft:trapdoor"
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
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        01 BLOCK-STATE              BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-SLOT LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        MOVE LK-ITEM-NAME TO PLACE-NAME

        MOVE 5 TO PLACE-PROPERTY-COUNT
        *> facing, half: filled in depending on click position
        MOVE "half" TO PLACE-PROPERTY-NAME(1)
        MOVE "facing" TO PLACE-PROPERTY-NAME(2)
        *> open, powered, waterlogged: default to false
        MOVE "open" TO PLACE-PROPERTY-NAME(3)
        MOVE "false" TO PLACE-PROPERTY-VALUE(3)
        MOVE "powered" TO PLACE-PROPERTY-NAME(4)
        MOVE "false" TO PLACE-PROPERTY-VALUE(4)
        MOVE "waterlogged" TO PLACE-PROPERTY-NAME(5)
        MOVE "false" TO PLACE-PROPERTY-VALUE(5)

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

        *> Use the player's yaw to determine the facing
        EVALUATE FUNCTION MOD(PLAYER-YAW(LK-PLAYER) + 45, 360)
            WHEN < 90
                MOVE "north" TO PLACE-PROPERTY-VALUE(2)
            WHEN < 180
                MOVE "east" TO PLACE-PROPERTY-VALUE(2)
            WHEN < 270
                MOVE "south" TO PLACE-PROPERTY-VALUE(2)
            WHEN OTHER
                MOVE "west" TO PLACE-PROPERTY-VALUE(2)
        END-EVALUATE

        CALL "Blocks-FromDescription" USING PLACE-DESCRIPTION BLOCK-STATE
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-STATE

        CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Trapdoor.
