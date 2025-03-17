*> --- RegisterBlock-Air ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Air.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 FACE-PTR                 PROGRAM-POINTER.
    01 BLOCK-STATE-ID           BINARY-LONG.

PROCEDURE DIVISION.
    SET FACE-PTR TO ENTRY "Callback-Face"

    CALL "Blocks-Get-DefaultStateId" USING "minecraft:air" BLOCK-STATE-ID
    CALL "SetCallback-BlockFace" USING BLOCK-STATE-ID FACE-PTR

    CALL "Blocks-Get-DefaultStateId" USING "minecraft:cave_air" BLOCK-STATE-ID
    CALL "SetCallback-BlockFace" USING BLOCK-STATE-ID FACE-PTR

    CALL "Blocks-Get-DefaultStateId" USING "minecraft:void_air" BLOCK-STATE-ID
    CALL "SetCallback-BlockFace" USING BLOCK-STATE-ID FACE-PTR

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
