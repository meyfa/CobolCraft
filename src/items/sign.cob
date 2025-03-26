*> --- RegisterItem-Sign ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Sign.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ITEM-REGISTRY            BINARY-LONG                 GLOBAL.
    01 BLOCK-ENTITY-TYPE        BINARY-LONG                 GLOBAL.
    01 USE-PTR                  PROGRAM-POINTER.
    COPY DD-TAGS.
    01 IDX-REGISTRY             BINARY-LONG UNSIGNED.
    01 IDX-TAG                  BINARY-LONG UNSIGNED.
    01 IDX-ITEM                 BINARY-LONG UNSIGNED.
    01 ITEM-NAME                PIC X(64).

PROCEDURE DIVISION.
    CALL "Registries-LookupRegistry" USING "minecraft:item" ITEM-REGISTRY

    CALL "Registries-Lookup" USING "minecraft:block_entity_type" "minecraft:sign" BLOCK-ENTITY-TYPE
    COPY ASSERT REPLACING COND BY ==BLOCK-ENTITY-TYPE >= 0==, MSG BY =="RegisterItem-Sign: Failed block entity lookup"==.

    SET USE-PTR TO ENTRY "Callback-Use"

    *> Iterate over "minecraft:signs" tag to find sign items
    *> TODO Make this simpler and reusable

    PERFORM VARYING IDX-REGISTRY FROM 1 BY 1 UNTIL IDX-REGISTRY > TAGS-REGISTRY-COUNT
        IF TAGS-REGISTRY-NAME(IDX-REGISTRY) = "minecraft:item"
            EXIT PERFORM
        END-IF
    END-PERFORM

    PERFORM VARYING IDX-TAG FROM 1 BY 1 UNTIL IDX-TAG > TAGS-REGISTRY-LENGTH(IDX-REGISTRY)
        IF TAGS-REGISTRY-TAG-NAME(IDX-REGISTRY, IDX-TAG) = "minecraft:signs"
            EXIT PERFORM
        END-IF
    END-PERFORM

    PERFORM VARYING IDX-ITEM FROM 1 BY 1 UNTIL IDX-ITEM > TAGS-REGISTRY-TAG-LENGTH(IDX-REGISTRY, IDX-TAG)
        *> TODO avoid the name lookup
        CALL "Registries-EntryName" USING ITEM-REGISTRY TAGS-REGISTRY-TAG-ENTRY(IDX-REGISTRY, IDX-TAG, IDX-ITEM) ITEM-NAME
        CALL "SetCallback-ItemUse" USING ITEM-NAME USE-PTR
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
        01 FACING                   PIC X(5).
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        01 BLOCK-STATE              BINARY-LONG.
        01 ROTATION-INT             BINARY-LONG.
        01 ROTATION-DISPLAY         PIC -(2)9.
        01 BLOCK-NAME-LENGTH        BINARY-LONG UNSIGNED.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-SLOT LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "ItemUtil-GetReplaceablePosition" USING BLOCK-POSITION LK-FACE CHECK-RESULT
        IF CHECK-RESULT = 0
            GOBACK
        END-IF

        CALL "Facing-ToString" USING LK-FACE FACING

        *> TODO handle placement on blocks other than the clicked one (when not a valid position)
        EVALUATE TRUE
            WHEN FACING = "north" OR "south" OR "east" OR "west"
                PERFORM GetWallSignBlock
            WHEN OTHER
                PERFORM GetStandingSignBlock
        END-EVALUATE

        CALL "Blocks-FromDescription" USING PLACE-DESCRIPTION BLOCK-STATE
        IF BLOCK-STATE > 0
            CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-STATE
            CALL "World-SetBlockEntity" USING BLOCK-POSITION BLOCK-ENTITY-TYPE

            CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT

            MOVE BLOCK-POSITION TO PLAYER-UPDATE-SIGN-POSITION(LK-PLAYER)

            MOVE 1 TO CHECK-RESULT
            CALL "SendPacket-OpenSignEditor" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION CHECK-RESULT
        END-IF

        GOBACK.

    GetStandingSignBlock.
        MOVE LK-ITEM-NAME TO PLACE-NAME
        MOVE 2 TO PLACE-PROPERTY-COUNT

        MOVE "waterlogged" TO PLACE-PROPERTY-NAME(1)
        MOVE "false" TO PLACE-PROPERTY-VALUE(1)

        MOVE "rotation" TO PLACE-PROPERTY-NAME(2)
        COMPUTE ROTATION-INT = PLAYER-YAW(LK-PLAYER) / 22.5 + 8.5
        MOVE FUNCTION MOD(ROTATION-INT, 16) TO ROTATION-DISPLAY
        MOVE FUNCTION TRIM(ROTATION-DISPLAY) TO PLACE-PROPERTY-VALUE(2)
        .

    GetWallSignBlock.
        MOVE LK-ITEM-NAME TO PLACE-NAME

        *> HACK: Convert name ending in _sign to end in _wall_sign
        *> TODO make this more robust
        MOVE FUNCTION STORED-CHAR-LENGTH(PLACE-NAME) TO BLOCK-NAME-LENGTH
        MOVE "_wall_sign" TO PLACE-NAME(BLOCK-NAME-LENGTH - 4:)

        MOVE 2 TO PLACE-PROPERTY-COUNT

        MOVE "waterlogged" TO PLACE-PROPERTY-NAME(1)
        MOVE "false" TO PLACE-PROPERTY-VALUE(1)

        MOVE "facing" TO PLACE-PROPERTY-NAME(2)
        MOVE FACING TO PLACE-PROPERTY-VALUE(2)
        .

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Sign.
