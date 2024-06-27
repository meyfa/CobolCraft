*> --- RegisterBlock-Water ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Water.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-WATER                PIC X(32) GLOBAL    VALUE "minecraft:water".
    01 FACE-PTR                         PROGRAM-POINTER.
    01 REPLACEABLE-PTR                  PROGRAM-POINTER.
    01 BLOCK-STATE-ID                   BINARY-LONG.

PROCEDURE DIVISION.
    SET FACE-PTR TO ENTRY "Callback-Face"
    SET REPLACEABLE-PTR TO ENTRY "Callback-Replaceable"

    CALL "Blocks-Get-DefaultStateId" USING C-MINECRAFT-WATER BLOCK-STATE-ID
    CALL "SetCallback-BlockFace" USING BLOCK-STATE-ID FACE-PTR
    CALL "SetCallback-BlockReplaceable" USING BLOCK-STATE-ID REPLACEABLE-PTR

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

    *> --- Callback-Replaceable ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Replaceable.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-REPLACEABLE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-RESULT.
        *> Liquids are replaceable.
        MOVE 1 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Replaceable.

END PROGRAM RegisterBlock-Water.
