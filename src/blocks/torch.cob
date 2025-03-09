*> --- RegisterBlock-Torch ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Torch.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 HARDNESS                 FLOAT-SHORT                 VALUE 0.0.
    01 DESTROY-PTR              PROGRAM-POINTER.
    01 FACE-PTR                 PROGRAM-POINTER.
    01 BLOCK-COUNT              BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX              BINARY-LONG UNSIGNED.
    01 BLOCK-NAME               PIC X(64).
    01 BLOCK-MINIMUM-STATE-ID   BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID   BINARY-LONG.
    01 STATE-ID                 BINARY-LONG.

PROCEDURE DIVISION.
    SET DESTROY-PTR TO ENTRY "Callback-Destroy"
    SET FACE-PTR TO ENTRY "Callback-Face"

    *> Loop over all blocks and register the callback for each matching block
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Name" USING BLOCK-INDEX BLOCK-NAME
        IF BLOCK-NAME = "minecraft:torch" OR "minecraft:soul_torch" OR "minecraft:redstone_torch" OR
                "minecraft:wall_torch" OR "minecraft:soul_wall_torch" OR "minecraft:redstone_wall_torch"
            CALL "Blocks-Iterate-StateIds" USING BLOCK-INDEX BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
            PERFORM VARYING STATE-ID FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL STATE-ID > BLOCK-MAXIMUM-STATE-ID
                CALL "SetCallback-BlockDestroy" USING STATE-ID DESTROY-PTR
                CALL "SetCallback-BlockFace" USING STATE-ID FACE-PTR
            END-PERFORM
            *> set metadata
            CALL "Blocks-SetHardness" USING BLOCK-INDEX HARDNESS
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
        01 DROPPED-ITEM-SLOT.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==DROPPED-ITEM==.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-DESTROY.

    PROCEDURE DIVISION USING LK-PLAYER LK-POSITION LK-FACE.
        CALL "World-GetBlock" USING LK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID CLICKED-DESCRIPTION

        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) LK-POSITION AIR-BLOCK-STATE

        *> Torches are a special case, as wall torches drop the corresponding torch item
        EVALUATE CLICKED-NAME
            WHEN "minecraft:wall_torch"
                MOVE "minecraft:torch" TO CLICKED-NAME
            WHEN "minecraft:soul_wall_torch"
                MOVE "minecraft:soul_torch" TO CLICKED-NAME
            WHEN "minecraft:redstone_wall_torch"
                MOVE "minecraft:redstone_torch" TO CLICKED-NAME
        END-EVALUATE

        IF PLAYER-GAMEMODE(LK-PLAYER) = 0 OR 2
            CALL "Registries-Get-EntryId" USING "minecraft:item" CLICKED-NAME DROPPED-ITEM-SLOT-ID
            IF DROPPED-ITEM-SLOT-ID >= 0
                MOVE 1 TO DROPPED-ITEM-SLOT-COUNT
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
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-FACE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-FACE LK-RESULT.
        *> Torches have no solid faces.
        MOVE 0 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Face.

END PROGRAM RegisterBlock-Torch.
