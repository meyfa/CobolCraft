*> --- RegisterItem-Bed ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Bed.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 USE-PTR                  PROGRAM-POINTER.
    01 BLOCK-COUNT              BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX              BINARY-LONG UNSIGNED.
    01 BLOCK-NAME               PIC X(64).
    01 BLOCK-TYPE               PIC X(64).

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"

    *> Loop over all blocks and register the callback for each bed
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        IF BLOCK-TYPE = "minecraft:bed"
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
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==PLACE==.
        01 BED-FACE                 BINARY-LONG.
        01 BLOCK-POSITION-FOOT.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 BLOCK-POSITION-HEAD.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(16).
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        01 CB-PTR-REPLACEABLE       PROGRAM-POINTER.
        01 BLOCK-ID                 BINARY-LONG.
        01 BLOCK-ENTITY-TYPE        BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> TODO reduce duplication with other callbacks

        *> Compute the position of the foot block - use the clicked block position if the block is replaceable.
        *> Otherwise, use the position next to it.
        MOVE LK-POSITION TO BLOCK-POSITION-FOOT
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION-FOOT LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        MOVE LK-ITEM-NAME TO PLACE-NAME

        MOVE 3 TO PLACE-PROPERTY-COUNT
        MOVE "occupied" TO PLACE-PROPERTY-NAME(1)
        MOVE "false" TO PLACE-PROPERTY-VALUE(1)
        MOVE "part" TO PLACE-PROPERTY-NAME(2)
        MOVE "facing" TO PLACE-PROPERTY-NAME(3)

        *> Use the player's yaw to determine the facing
        EVALUATE FUNCTION MOD(PLAYER-YAW(LK-PLAYER) + 45, 360)
            WHEN < 90
                MOVE "south" TO PLACE-PROPERTY-VALUE(3)
            WHEN < 180
                MOVE "west" TO PLACE-PROPERTY-VALUE(3)
            WHEN < 270
                MOVE "north" TO PLACE-PROPERTY-VALUE(3)
            WHEN OTHER
                MOVE "east" TO PLACE-PROPERTY-VALUE(3)
        END-EVALUATE

        *> Compute the position of the head block
        MOVE BLOCK-POSITION-FOOT TO BLOCK-POSITION-HEAD
        CALL "Facing-FromString" USING PLACE-PROPERTY-VALUE(3) BED-FACE
        CALL "Facing-GetRelative" USING BED-FACE BLOCK-POSITION-HEAD

        *> Ensure the head block is also replaceable
        CALL "World-GetBlock" USING BLOCK-POSITION-HEAD BLOCK-ID
        CALL "GetCallback-BlockReplaceable" USING BLOCK-ID CB-PTR-REPLACEABLE
        CALL CB-PTR-REPLACEABLE USING BLOCK-ID CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        *> Place the bed
        MOVE "foot" TO PLACE-PROPERTY-VALUE(2)
        CALL "Blocks-Get-StateId" USING PLACE-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION-FOOT BLOCK-ID
        MOVE "head" TO PLACE-PROPERTY-VALUE(2)
        CALL "Blocks-Get-StateId" USING PLACE-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION-HEAD BLOCK-ID

        *> Place the block entities
        CALL "Registries-Get-EntryId" USING "minecraft:block_entity_type" "minecraft:bed" BLOCK-ENTITY-TYPE
        CALL "World-SetBlockEntity" USING BLOCK-POSITION-FOOT BLOCK-ENTITY-TYPE
        CALL "World-SetBlockEntity" USING BLOCK-POSITION-HEAD BLOCK-ENTITY-TYPE

        CALL "ItemUtil-ConsumeItem" USING LK-PLAYER

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Bed.
