*> --- RegisterBlock-Water ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Water.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 FACE-PTR                 PROGRAM-POINTER.
    01 BLOCK-STATE-ID           BINARY-LONG.

PROCEDURE DIVISION.
    SET FACE-PTR TO ENTRY "Callback-Face"

    CALL "Blocks-Get-DefaultStateId" USING "minecraft:water" BLOCK-STATE-ID
    CALL "SetCallback-BlockFace" USING BLOCK-STATE-ID FACE-PTR

    GOBACK.

    *> --- Callback-Face ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Face.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-FACE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-FACE LK-RESULT.
        *> Liquids have no solid faces.
        MOVE 0 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Face.

END PROGRAM RegisterBlock-Water.
