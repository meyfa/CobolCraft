*> --- RegisterBlock-Generic ---
*> Register handlers for a generic block.
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Generic.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BLOCK-REGISTRY           BINARY-LONG.
    01 HARDNESS                 FLOAT-SHORT                 VALUE 1.0.
    01 DESTROY-PTR              PROGRAM-POINTER.
    01 FACE-PTR                 PROGRAM-POINTER.
    01 REPLACEABLE-PTR          PROGRAM-POINTER.
    01 ITEM-PTR                 PROGRAM-POINTER.
    01 BLOCK-COUNT              BINARY-LONG.
    01 BLOCK-ID                 BINARY-LONG.
    01 BLOCK-MINIMUM-STATE-ID   BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID   BINARY-LONG.
    01 BLOCK-STATE              BINARY-LONG.

PROCEDURE DIVISION.
    CALL "Registries-LookupRegistry" USING "minecraft:block" BLOCK-REGISTRY

    SET DESTROY-PTR TO ENTRY "Callback-Destroy"
    SET FACE-PTR TO ENTRY "Callback-Face"
    SET REPLACEABLE-PTR TO ENTRY "Callback-Replaceable"
    SET ITEM-PTR TO ENTRY "Callback-Item"

    CALL "Registries-EntryCount" USING BLOCK-REGISTRY BLOCK-COUNT
    PERFORM VARYING BLOCK-ID FROM 0 BY 1 UNTIL BLOCK-ID >= BLOCK-COUNT
        CALL "Blocks-GetStateIds" USING BLOCK-ID BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
        PERFORM VARYING BLOCK-STATE FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL BLOCK-STATE > BLOCK-MAXIMUM-STATE-ID
            CALL "SetCallback-BlockDestroy" USING BLOCK-STATE DESTROY-PTR
            CALL "SetCallback-BlockFace" USING BLOCK-STATE FACE-PTR
            CALL "SetCallback-BlockReplaceable" USING BLOCK-STATE REPLACEABLE-PTR
            CALL "SetCallback-BlockItem" USING BLOCK-STATE ITEM-PTR
        END-PERFORM
        *> set metadata
        *> TODO figure out a better default hardness value
        CALL "Blocks-SetHardness" USING BLOCK-ID HARDNESS
    END-PERFORM

    GOBACK.

    *> --- Callback-Destroy ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Destroy.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 AIR-BLOCK-STATE          BINARY-LONG             VALUE 0.
        01 BLOCK-STATE              BINARY-LONG.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CLICKED==.
        01 BLOCK-REGISTRY           BINARY-LONG.
        01 LOOT-PTR                 PROGRAM-POINTER.
        01 SURVIVES-EXPLOSION       BINARY-LONG             VALUE 1.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-DESTROY.

    PROCEDURE DIVISION USING LK-PLAYER LK-POSITION LK-FACE.
        CALL "World-GetBlock" USING LK-POSITION BLOCK-STATE
        IF BLOCK-STATE = AIR-BLOCK-STATE
            GOBACK
        END-IF

        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) LK-POSITION AIR-BLOCK-STATE

        IF PLAYER-GAMEMODE(LK-PLAYER) = 0 OR 2
            CALL "Blocks-ToDescription" USING BLOCK-STATE CLICKED-DESCRIPTION

            *> get loot table callback
            CALL "Registries-Lookup" USING "minecraft:block" CLICKED-NAME BLOCK-REGISTRY
            CALL "GetCallback-BlockLoot" USING BLOCK-REGISTRY LOOT-PTR

            IF LOOT-PTR NOT = NULL
                CALL LOOT-PTR USING LK-POSITION BLOCK-STATE SURVIVES-EXPLOSION
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
        01 BLOCK-REGISTRY           BINARY-LONG.
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
        CALL "Blocks-ToDescription" USING LK-BLOCK-STATE CLICKED-DESCRIPTION
        CALL "Registries-Lookup" USING "minecraft:block" CLICKED-NAME BLOCK-REGISTRY
        IF BLOCK-REGISTRY < 0
            MOVE 0 TO LK-RESULT
            GOBACK
        END-IF

        *> Check if the block has the "minecraft:replaceable" tag.
        PERFORM VARYING TAG-INDEX FROM 1 BY 1 UNTIL TAG-INDEX > TAGS-REGISTRY-TAG-LENGTH(TAGS-BLOCK-REGISTRY, REPLACEABLE-TAG)
            IF TAGS-REGISTRY-TAG-ENTRY(TAGS-BLOCK-REGISTRY, REPLACEABLE-TAG, TAG-INDEX) = BLOCK-REGISTRY
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
    WORKING-STORAGE SECTION.
        01 BLOCK-REGISTRY           BINARY-LONG                 VALUE -1.
        01 BLOCK-ID                 BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-ITEM.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-ITEM-IDENTIFIER.
        IF BLOCK-REGISTRY < 0
            CALL "Registries-LookupRegistry" USING "minecraft:block" BLOCK-REGISTRY
        END-IF

        *> Assume the block has the same name as an item.
        CALL "Blocks-ForStateId" USING LK-BLOCK-STATE BLOCK-ID
        CALL "Registries-EntryName" USING BLOCK-REGISTRY BLOCK-ID LK-ITEM-IDENTIFIER

        GOBACK.

    END PROGRAM Callback-Item.

END PROGRAM RegisterBlock-Generic.
