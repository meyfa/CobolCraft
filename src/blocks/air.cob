*> --- RegisterBlock-Air ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Air.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 FACE-PTR                 PROGRAM-POINTER.
    01 BLOCK-ID                 BINARY-LONG.
    01 BLOCK-STATE              BINARY-LONG.

PROCEDURE DIVISION.
    SET FACE-PTR TO ENTRY "Callback-Face"

    CALL "Registries-Lookup" USING "minecraft:block" "minecraft:air" BLOCK-ID
    CALL "Blocks-GetDefaultStateId" USING BLOCK-ID BLOCK-STATE
    CALL "SetCallback-BlockFace" USING BLOCK-STATE FACE-PTR

    CALL "Registries-Lookup" USING "minecraft:block" "minecraft:cave_air" BLOCK-ID
    CALL "Blocks-GetDefaultStateId" USING BLOCK-ID BLOCK-STATE
    CALL "SetCallback-BlockFace" USING BLOCK-STATE FACE-PTR

    CALL "Registries-Lookup" USING "minecraft:block" "minecraft:void_air" BLOCK-ID
    CALL "Blocks-GetDefaultStateId" USING BLOCK-ID BLOCK-STATE
    CALL "SetCallback-BlockFace" USING BLOCK-STATE FACE-PTR

    GOBACK.

    *> --- Callback-Face ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Face.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-FACE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-FACE LK-RESULT.
        *> Air has no solid faces.
        MOVE 0 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Face.

END PROGRAM RegisterBlock-Air.
