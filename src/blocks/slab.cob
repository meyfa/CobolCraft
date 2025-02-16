*> --- RegisterBlock-Slab ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Slab.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 DESTROY-PTR              PROGRAM-POINTER.
    01 FACE-PTR                 PROGRAM-POINTER.
    01 BLOCK-COUNT              BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX              BINARY-LONG UNSIGNED.
    01 BLOCK-TYPE               PIC X(64).
    01 BLOCK-MINIMUM-STATE-ID   BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID   BINARY-LONG.
    01 STATE-ID                 BINARY-LONG.

PROCEDURE DIVISION.
    SET DESTROY-PTR TO ENTRY "Callback-Destroy"
    SET FACE-PTR TO ENTRY "Callback-Face"

    *> Loop over all blocks and register the callback for each matching block type
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        IF BLOCK-TYPE = "minecraft:slab"
            CALL "Blocks-Iterate-StateIds" USING BLOCK-INDEX BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
            PERFORM VARYING STATE-ID FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL STATE-ID > BLOCK-MAXIMUM-STATE-ID
                CALL "SetCallback-BlockDestroy" USING STATE-ID DESTROY-PTR
                CALL "SetCallback-BlockFace" USING STATE-ID FACE-PTR
            END-PERFORM
            *> TODO set metadata
        END-IF
    END-PERFORM

    GOBACK.

    *> --- Callback-Destroy ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Destroy.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-PLAYERS.
        01 AIR-BLOCK-STATE          BINARY-LONG             VALUE 0.
        01 BLOCK-ID                 BINARY-LONG.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CLICKED==.
        01 PROPERTY-VALUE           PIC X(16).
        01 DROPPED-ITEM-SLOT.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==DROPPED-ITEM==.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-DESTROY.

    PROCEDURE DIVISION USING LK-PLAYER LK-POSITION LK-FACE.
        CALL "World-GetBlock" USING LK-POSITION BLOCK-ID
        IF BLOCK-ID = AIR-BLOCK-STATE
            GOBACK
        END-IF

        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) LK-POSITION AIR-BLOCK-STATE

        IF PLAYER-GAMEMODE(LK-PLAYER) = 0 OR 2
            CALL "Blocks-Get-StateDescription" USING BLOCK-ID CLICKED-DESCRIPTION
            CALL "Registries-Get-EntryId" USING "minecraft:item" CLICKED-NAME DROPPED-ITEM-SLOT-ID
            IF DROPPED-ITEM-SLOT-ID >= 0
                CALL "Blocks-Description-GetValue" USING CLICKED-DESCRIPTION "type" PROPERTY-VALUE
                IF PROPERTY-VALUE = "double"
                    MOVE 2 TO DROPPED-ITEM-SLOT-COUNT
                ELSE
                    MOVE 1 TO DROPPED-ITEM-SLOT-COUNT
                END-IF

                *> TODO data components
                MOVE 2 TO DROPPED-ITEM-SLOT-NBT-LENGTH
                MOVE X"0000" TO DROPPED-ITEM-SLOT-NBT-DATA(1:2)

                CALL "World-DropItem-FromBlock" USING DROPPED-ITEM-SLOT LK-POSITION
            END-IF
        END-IF

        GOBACK.

    END PROGRAM Callback-Destroy.

    *> --- Callback-Face ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Face.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==BLOCK==.
        01 PROPERTY-VALUE           PIC X(16).
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-FACE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-FACE LK-RESULT.
        CALL "Blocks-Get-StateDescription" USING LK-BLOCK-STATE BLOCK-DESCRIPTION
        CALL "Blocks-Description-GetValue" USING BLOCK-DESCRIPTION "type" PROPERTY-VALUE

        EVALUATE TRUE
            *> Double slabs are solid everywhere
            WHEN PROPERTY-VALUE = "double"
                MOVE 1 TO LK-RESULT
            *> Bottom slabs are solid at the bottom; top slabs at the top
            WHEN PROPERTY-VALUE = "bottom" AND LK-FACE = "down"
                MOVE 1 TO LK-RESULT
            WHEN PROPERTY-VALUE = "top" AND LK-FACE = "up"
                MOVE 1 TO LK-RESULT
            *> Otherwise, slabs aren't solid
            WHEN OTHER
                MOVE 0 TO LK-RESULT
        END-EVALUATE

        GOBACK.

    END PROGRAM Callback-Face.

END PROGRAM RegisterBlock-Slab.
