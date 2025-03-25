*> --- RegisterBlock-Generic ---
*> Register handlers for a generic block.
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Generic.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 HARDNESS                 FLOAT-SHORT                 VALUE 1.0.
    01 DESTROY-PTR              PROGRAM-POINTER.
    01 FACE-PTR                 PROGRAM-POINTER.
    01 REPLACEABLE-PTR          PROGRAM-POINTER.
    01 ITEM-PTR                 PROGRAM-POINTER.
    01 BLOCK-COUNT              BINARY-LONG.
    01 BLOCK-INDEX              BINARY-LONG.
    01 BLOCK-MINIMUM-STATE-ID   BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID   BINARY-LONG.
    01 BLOCK-STATE-ID           BINARY-LONG.

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
        COPY DD-PLAYERS.
        01 AIR-BLOCK-STATE          BINARY-LONG             VALUE 0.
        01 BLOCKSTATE-ID            BINARY-LONG.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CLICKED==.
        01 BLOCK-REGISTRY-ID        BINARY-LONG.
        01 LOOT-PTR                 PROGRAM-POINTER.
        01 SURVIVES-EXPLOSION       BINARY-LONG             VALUE 1.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-DESTROY.

    PROCEDURE DIVISION USING LK-PLAYER LK-POSITION LK-FACE.
        CALL "World-GetBlock" USING LK-POSITION BLOCKSTATE-ID
        IF BLOCKSTATE-ID = AIR-BLOCK-STATE
            GOBACK
        END-IF

        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) LK-POSITION AIR-BLOCK-STATE

        IF PLAYER-GAMEMODE(LK-PLAYER) = 0 OR 2
            CALL "Blocks-Get-StateDescription" USING BLOCKSTATE-ID CLICKED-DESCRIPTION

            *> get loot table callback
            CALL "Registries-Lookup" USING "minecraft:block" CLICKED-NAME BLOCK-REGISTRY-ID
            CALL "GetCallback-BlockLoot" USING BLOCK-REGISTRY-ID LOOT-PTR

            IF LOOT-PTR NOT = NULL
                CALL LOOT-PTR USING LK-POSITION BLOCKSTATE-ID SURVIVES-EXPLOSION
            END-IF
        END-IF

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
    WORKING-STORAGE SECTION.
        COPY DD-TAGS.
        01 TAGS-BLOCK-REGISTRY      BINARY-LONG UNSIGNED.
        01 REPLACEABLE-TAG          BINARY-LONG UNSIGNED.
        01 TAG-INDEX                BINARY-LONG UNSIGNED.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CLICKED==.
        01 BLOCK-REGISTRY-ID        BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-REPLACEABLE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-RESULT.
        *> On first call: Look up indices into the tags table
        IF TAGS-BLOCK-REGISTRY = 0
            *> find the block-related tags
            PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-REGISTRY-COUNT
                IF TAGS-REGISTRY-NAME(TAG-INDEX) = "minecraft:block"
                    MOVE TAG-INDEX TO TAGS-BLOCK-REGISTRY
                    EXIT PERFORM
                END-IF
            END-PERFORM

            *> find the "minecraft:replaceable" tag
            PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-REGISTRY-LENGTH(TAGS-BLOCK-REGISTRY)
                IF TAGS-REGISTRY-TAG-NAME(TAGS-BLOCK-REGISTRY, TAG-INDEX) = "minecraft:replaceable"
                    MOVE TAG-INDEX TO REPLACEABLE-TAG
                    EXIT PERFORM
                END-IF
            END-PERFORM
        END-IF

        *> Obtain the block ID from the block state ID
        CALL "Blocks-Get-StateDescription" USING LK-BLOCK-STATE CLICKED-DESCRIPTION
        CALL "Registries-Lookup" USING "minecraft:block" CLICKED-NAME BLOCK-REGISTRY-ID
        IF BLOCK-REGISTRY-ID < 0
            MOVE 0 TO LK-RESULT
            GOBACK
        END-IF

        *> Check if the block has the "minecraft:replaceable" tag.
        PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-REGISTRY-TAG-LENGTH(TAGS-BLOCK-REGISTRY, REPLACEABLE-TAG)
            IF TAGS-REGISTRY-TAG-ENTRY(TAGS-BLOCK-REGISTRY, REPLACEABLE-TAG, TAG-INDEX) = BLOCK-REGISTRY-ID
                MOVE 1 TO LK-RESULT
                GOBACK
            END-IF
        END-PERFORM

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
