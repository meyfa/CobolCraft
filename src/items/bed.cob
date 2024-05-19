*> --- RegisterItem-Bed ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Bed.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-BLOCK_ENTITY_TYPE    PIC X(32) GLOBAL    VALUE "minecraft:block_entity_type".
    01 C-MINECRAFT-BED                  PIC X(32) GLOBAL    VALUE "minecraft:bed".
    01 USE-PTR                          PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-NAME                       PIC X(64).
    01 BLOCK-TYPE                       PIC X(64).

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"

    *> Loop over all blocks and register the callback for each bed
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        IF BLOCK-TYPE = C-MINECRAFT-BED
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
        01 C-OCCUPIED               PIC X(8)                VALUE "occupied".
        01 C-PART                   PIC X(4)                VALUE "part".
        01 C-FACING                 PIC X(6)                VALUE "facing".
        *> Block state description for the block to place.
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
        01 BOUNDS-CHECK             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
        01 BLOCK-ENTITY-TYPE        BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> TODO reduce duplication with other callbacks

        *> Compute the position of the foot block
        MOVE LK-POSITION TO BLOCK-POSITION-FOOT
        CALL "Facing-GetRelative" USING LK-FACE BLOCK-POSITION-FOOT

        MOVE LK-ITEM-NAME TO PLACE-NAME

        MOVE 3 TO PLACE-PROPERTY-COUNT
        MOVE C-OCCUPIED TO PLACE-PROPERTY-NAME(1)
        MOVE "false" TO PLACE-PROPERTY-VALUE(1)
        MOVE C-PART TO PLACE-PROPERTY-NAME(2)
        MOVE C-FACING TO PLACE-PROPERTY-NAME(3)

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

        *> Ensure both blocks are air
        CALL "World-GetBlock" USING BLOCK-POSITION-FOOT BLOCK-ID
        IF BLOCK-ID NOT = 0 GOBACK.
        CALL "World-GetBlock" USING BLOCK-POSITION-HEAD BLOCK-ID
        IF BLOCK-ID NOT = 0 GOBACK.

        *> Place the bed
        MOVE "foot" TO PLACE-PROPERTY-VALUE(2)
        CALL "Blocks-Get-StateId" USING PLACE-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION-FOOT BLOCK-ID
        MOVE "head" TO PLACE-PROPERTY-VALUE(2)
        CALL "Blocks-Get-StateId" USING PLACE-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION-HEAD BLOCK-ID

        *> Place the block entities
        CALL "Registries-Get-EntryId" USING C-MINECRAFT-BLOCK_ENTITY_TYPE C-MINECRAFT-BED BLOCK-ENTITY-TYPE
        CALL "World-SetBlockEntity" USING BLOCK-POSITION-FOOT BLOCK-ENTITY-TYPE
        CALL "World-SetBlockEntity" USING BLOCK-POSITION-HEAD BLOCK-ENTITY-TYPE

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Bed.
