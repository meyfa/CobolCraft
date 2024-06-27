*> --- ItemUtil-GetReplaceablePosition ---
*> Blocks should be placed at the clicked position if the block is replaceable. For non-replaceable blocks, they should
*> be placed next to the clicked position (depending on the face clicked). This helper method calculates the correct
*> position for the block to be placed at.
*> In case there is no valid position (no replaceable block, or outside of world bounds), the valid flag will be 0.
IDENTIFICATION DIVISION.
PROGRAM-ID. ItemUtil-GetReplaceablePosition.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BLOCK-ID                 BINARY-LONG.
    01 CHECK-RESULT             BINARY-CHAR UNSIGNED.
    01 CB-PTR-REPLACEABLE       PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                     BINARY-LONG.
        02 LK-Y                     BINARY-LONG.
        02 LK-Z                     BINARY-LONG.
    01 LK-FACE                  BINARY-LONG.
    01 LK-VALID                 BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-POSITION LK-FACE LK-VALID.
    CALL "World-GetBlock" USING LK-POSITION BLOCK-ID
    CALL "GetCallback-BlockReplaceable" USING BLOCK-ID CB-PTR-REPLACEABLE
    CALL CB-PTR-REPLACEABLE USING BLOCK-ID CHECK-RESULT
    IF CHECK-RESULT = 1
        MOVE 1 TO LK-VALID
        GOBACK
    END-IF

    CALL "Facing-GetRelative" USING LK-FACE LK-POSITION
    CALL "World-CheckBounds" USING LK-POSITION CHECK-RESULT
    IF CHECK-RESULT NOT = 0
        MOVE 0 TO LK-VALID
        GOBACK
    END-IF

    CALL "World-GetBlock" USING LK-POSITION BLOCK-ID
    CALL "GetCallback-BlockReplaceable" USING BLOCK-ID CB-PTR-REPLACEABLE
    CALL CB-PTR-REPLACEABLE USING BLOCK-ID CHECK-RESULT
    IF CHECK-RESULT = 0
        MOVE 0 TO LK-VALID
        GOBACK
    END-IF

    MOVE 1 TO LK-VALID
    GOBACK.

END PROGRAM ItemUtil-GetReplaceablePosition.
