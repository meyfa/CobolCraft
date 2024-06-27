*> --- RegisterItem-Button ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Button.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-BUTTON               PIC X(32) GLOBAL    VALUE "minecraft:button".
    01 USE-PTR                          PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-NAME                       PIC X(64).
    01 BLOCK-TYPE                       PIC X(64).

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"

    *> Loop over all blocks and register the callback for each button
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        IF BLOCK-TYPE = C-MINECRAFT-BUTTON
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
        01 C-FACE                   PIC X(4)                VALUE "face".
        01 C-FACING                 PIC X(6)                VALUE "facing".
        *> Block state description for the block to place.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==PLACE==.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(16).
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> TODO reduce duplication with other callbacks

        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        *> TODO: check for solid block where the button will be placed

        MOVE LK-ITEM-NAME TO PLACE-NAME

        MOVE 3 TO PLACE-PROPERTY-COUNT
        MOVE C-FACE TO PLACE-PROPERTY-NAME(1)
        MOVE C-FACING TO PLACE-PROPERTY-NAME(2)
        MOVE "powered" TO PLACE-PROPERTY-NAME(3)
        MOVE "false" TO PLACE-PROPERTY-VALUE(3)

        MOVE "wall" TO PLACE-PROPERTY-VALUE(1)
        EVALUATE FACING
            WHEN "up"
                MOVE "floor" TO PLACE-PROPERTY-VALUE(1)
            WHEN "down"
                MOVE "ceiling" TO PLACE-PROPERTY-VALUE(1)
            WHEN OTHER
                MOVE FACING TO PLACE-PROPERTY-VALUE(2)
        END-EVALUATE

        *> for ceiling/floor buttons, use the player's yaw to determine the facing
        IF FACING = "up" OR FACING = "down"
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
        END-IF

        CALL "Blocks-Get-StateId" USING PLACE-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Button.
