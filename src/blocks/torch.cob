*> --- RegisterBlock-Torch ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Torch.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 HARDNESS                 FLOAT-SHORT                 VALUE 0.0.
    01 FACE-PTR                 PROGRAM-POINTER.
    01 BLOCK-COUNT              BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX              BINARY-LONG UNSIGNED.
    01 BLOCK-NAME               PIC X(64).
    01 BLOCK-MINIMUM-STATE-ID   BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID   BINARY-LONG.
    01 STATE-ID                 BINARY-LONG.

PROCEDURE DIVISION.
    SET FACE-PTR TO ENTRY "Callback-Face"

    *> Loop over all blocks and register the callback for each matching block
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Name" USING BLOCK-INDEX BLOCK-NAME
        IF BLOCK-NAME = "minecraft:torch" OR "minecraft:soul_torch" OR "minecraft:redstone_torch" OR
                "minecraft:wall_torch" OR "minecraft:soul_wall_torch" OR "minecraft:redstone_wall_torch"
            CALL "Blocks-Iterate-StateIds" USING BLOCK-INDEX BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
            PERFORM VARYING STATE-ID FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL STATE-ID > BLOCK-MAXIMUM-STATE-ID
                CALL "SetCallback-BlockFace" USING STATE-ID FACE-PTR
            END-PERFORM
            *> set metadata
            CALL "Blocks-SetHardness" USING BLOCK-INDEX HARDNESS
        END-IF
    END-PERFORM

    GOBACK.

    *> --- Callback-Face ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Face.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-FACE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-FACE LK-RESULT.
        *> Torches have no solid faces.
        MOVE 0 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Face.

END PROGRAM RegisterBlock-Torch.
