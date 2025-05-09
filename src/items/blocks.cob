*> --- RegisterItem-Block ---
*> Register a generic block item.
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Block.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 USE-PTR                  PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-ITEM-NAME             PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-ITEM-NAME.
    SET USE-PTR TO ENTRY "Callback-Use"
    CALL "SetCallback-ItemUse" USING LK-ITEM-NAME USE-PTR
    GOBACK.

    *> --- Callback-Use ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Use.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
        01 BLOCK-STATE              BINARY-LONG.
        01 PLACE-AABB.
            COPY DD-AABB REPLACING LEADING ==PREFIX== BY ==PLACE==.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-SLOT LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
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

        *> Place the block. For this default handler, we assume the block has the same name as the item.
        CALL "Registries-Lookup" USING "minecraft:block" LK-ITEM-NAME BLOCK-ID
        IF BLOCK-ID > 0
            CALL "Blocks-GetDefaultStateId" USING BLOCK-ID BLOCK-STATE
            CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-STATE
        END-IF

        CALL "ItemUtil-ConsumeItem" USING LK-PLAYER LK-SLOT

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Block.
