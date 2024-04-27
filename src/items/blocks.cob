*> --- RegisterItem-Block ---
*> Register a generic block item.
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Block.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 USE-PTR                      PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-ITEM-NAME                 PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-ITEM-NAME.
    SET USE-PTR TO ENTRY "Callback-Use"
    CALL "SetCallback-ItemUse" USING LK-ITEM-NAME USE-PTR
    GOBACK.

    *> --- Callback-Use ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Use.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 BOUNDS-CHECK             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE.
        *> Compute the position of the block to be affected
        MOVE LK-POSITION TO BLOCK-POSITION
        EVALUATE LK-FACE
            WHEN 0
                COMPUTE BLOCK-Y = BLOCK-Y - 1
            WHEN 1
                COMPUTE BLOCK-Y = BLOCK-Y + 1
            WHEN 2
                COMPUTE BLOCK-Z = BLOCK-Z - 1
            WHEN 3
                COMPUTE BLOCK-Z = BLOCK-Z + 1
            WHEN 4
                COMPUTE BLOCK-X = BLOCK-X - 1
            WHEN 5
                COMPUTE BLOCK-X = BLOCK-X + 1
        END-EVALUATE

        *> Ensure the position is not outside the world
        CALL "World-CheckBounds" USING BLOCK-POSITION BOUNDS-CHECK
        IF BOUNDS-CHECK NOT = 0
            GOBACK
        END-IF

        *> Ensure the block was previously air
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        IF BLOCK-ID NOT = 0
            GOBACK
        END-IF

        *> Place the block. For this default handler, we assume the block has the same name as the item.
        CALL "Blocks-Get-DefaultStateId" USING LK-ITEM-NAME BLOCK-ID
        IF BLOCK-ID > 0
            CALL "World-SetBlock" USING BLOCK-POSITION BLOCK-ID
        END-IF

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Block.
