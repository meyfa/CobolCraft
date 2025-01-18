*> --- RegisterBlock-TallGrass ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-TallGrass.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-TALL_GRASS           PIC X(32) GLOBAL    VALUE "minecraft:tall_grass".
    01 HARDNESS                         FLOAT-SHORT         VALUE 0.0.
    01 FACE-PTR                         PROGRAM-POINTER.
    01 REPLACEABLE-PTR                  PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-TYPE                       PIC X(64).
    01 BLOCK-NAME                       PIC X(64).
    01 BLOCK-MINIMUM-STATE-ID           BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID           BINARY-LONG.
    01 STATE-ID                         BINARY-LONG.

PROCEDURE DIVISION.
    SET FACE-PTR TO ENTRY "Callback-Face"
    SET REPLACEABLE-PTR TO ENTRY "Callback-Replaceable"

    *> Loop over all blocks and register the callback for each matching block type
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        CALL "Blocks-Iterate-Name" USING BLOCK-INDEX BLOCK-NAME
        *> Note: The "minecraft:tall_grass" block itself doesn't have the type "minecraft:tall_grass" like the other
        *> grass types, but rather "minecraft:double_plant". However, "minecraft:double_plant" in general is not
        *> replaceable, so this is a special case.
        IF BLOCK-TYPE = C-MINECRAFT-TALL_GRASS OR BLOCK-NAME = C-MINECRAFT-TALL_GRASS
            CALL "Blocks-Iterate-StateIds" USING BLOCK-INDEX BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
            PERFORM VARYING STATE-ID FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL STATE-ID > BLOCK-MAXIMUM-STATE-ID
                CALL "SetCallback-BlockFace" USING STATE-ID FACE-PTR
                CALL "SetCallback-BlockReplaceable" USING STATE-ID REPLACEABLE-PTR
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
        *> Tall grass has no solid faces.
        MOVE 0 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Face.

    *> --- Callback-Replaceable ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Replaceable.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-REPLACEABLE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-RESULT.
        *> Tall grass is replaceable.
        MOVE 1 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Replaceable.

END PROGRAM RegisterBlock-TallGrass.
