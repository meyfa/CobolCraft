*> --- RegisterBlock-Generic ---
*> Register handlers for a generic block.
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Generic.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 DESTROY-PTR                  PROGRAM-POINTER.
    01 FACE-PTR                     PROGRAM-POINTER.
    01 REPLACEABLE-PTR              PROGRAM-POINTER.
    01 ITEM-PTR                     PROGRAM-POINTER.
    01 HARDNESS                     FLOAT-SHORT             VALUE 1.0.
    01 BLOCK-COUNT                  BINARY-LONG.
    01 BLOCK-INDEX                  BINARY-LONG.
    01 BLOCK-MINIMUM-STATE-ID       BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID       BINARY-LONG.
    01 BLOCK-STATE-ID               BINARY-LONG.

PROCEDURE DIVISION.
    SET DESTROY-PTR TO ENTRY "Callback-Destroy"
    SET FACE-PTR TO ENTRY "Callback-Face"
    SET REPLACEABLE-PTR TO ENTRY "Callback-Replaceable"
    SET ITEM-PTR TO ENTRY "Callback-Item"

    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-StateIds" USING BLOCK-INDEX BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
        PERFORM VARYING BLOCK-STATE-ID FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL BLOCK-STATE-ID > BLOCK-MAXIMUM-STATE-ID
            CALL "SetCallback-BlockDestroy" USING BLOCK-STATE-ID DESTROY-PTR
            CALL "SetCallback-BlockFace" USING BLOCK-STATE-ID FACE-PTR
            CALL "SetCallback-BlockReplaceable" USING BLOCK-STATE-ID REPLACEABLE-PTR
            CALL "SetCallback-BlockItem" USING BLOCK-STATE-ID ITEM-PTR
        END-PERFORM
        *> set metadata
        *> TODO figure out a better default hardness value
        CALL "Blocks-SetHardness" USING BLOCK-INDEX HARDNESS
    END-PERFORM

    GOBACK.

    *> --- Callback-Destroy ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Destroy.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 AIR-BLOCK-STATE          BINARY-LONG             VALUE 0.
        COPY DD-PLAYERS.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-DESTROY.

    PROCEDURE DIVISION USING LK-PLAYER LK-POSITION LK-FACE.
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) LK-POSITION AIR-BLOCK-STATE
        GOBACK.

    END PROGRAM Callback-Destroy.

    *> --- Callback-Face ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Face.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-FACE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-FACE LK-RESULT.
        *> Assume all block faces are solid by default.
        MOVE 1 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Face.

    *> --- Callback-Replaceable ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Replaceable.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-REPLACEABLE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-RESULT.
        *> Assume all blocks are non-replaceable by default (right-clicking the block with a block in hand will not
        *> replace the block).
        MOVE 0 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Replaceable.

    *> --- Callback-Item ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Item.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-ITEM.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-ITEM-IDENTIFIER.
        *> Assume the block has the same name as an item.
        CALL "Blocks-Get-Name" USING LK-BLOCK-STATE LK-ITEM-IDENTIFIER
        GOBACK.

    END PROGRAM Callback-Item.

END PROGRAM RegisterBlock-Generic.
