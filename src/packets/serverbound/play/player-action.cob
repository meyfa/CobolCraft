IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-PlayerAction.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    COPY DD-CLIENT-STATES.
    COPY DD-SERVER-PROPERTIES.
    01 PLAYER-ID                BINARY-LONG.
    *> payload
    01 ACTION-ENUM              BINARY-LONG.
    01 LOCATION.
        02 LOCATION-X           BINARY-LONG.
        02 LOCATION-Y           BINARY-LONG.
        02 LOCATION-Z           BINARY-LONG.
    01 FACE-ENUM                BINARY-CHAR.
    01 SEQUENCE-ID              BINARY-LONG.
    *> variables
    01 BLOCK-FACE               BINARY-LONG.
    01 BLOCK-STATE              BINARY-LONG.
    01 BLOCK-ID                 BINARY-LONG UNSIGNED.
    01 BOUNDS-CHECK             BINARY-CHAR UNSIGNED.
    01 BLOCK-HARDNESS           FLOAT-SHORT.
    01 CALLBACK-PTR             PROGRAM-POINTER.
    01 OTHER-CLIENT             BINARY-LONG UNSIGNED.
    01 SLOT-INDEX               BINARY-LONG UNSIGNED.
    01 DROP-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==DROP==.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET ACTION-ENUM
    CALL "Decode-Position" USING LK-BUFFER LK-OFFSET LOCATION
    CALL "Decode-Byte" USING LK-BUFFER LK-OFFSET FACE-ENUM
    MOVE FACE-ENUM TO BLOCK-FACE
    CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET SEQUENCE-ID

    EVALUATE TRUE
        WHEN ACTION-ENUM = 0
            PERFORM StartedDigging
        WHEN ACTION-ENUM = 1
            PERFORM CancelledDigging
        WHEN ACTION-ENUM = 2
            PERFORM FinishedDigging
        WHEN ACTION-ENUM = 3
            PERFORM DropItemStack
        WHEN ACTION-ENUM = 4
            PERFORM DropItem
        WHEN ACTION-ENUM = 6
            PERFORM SwapOffhand
        WHEN OTHER
            *> TODO The following is a workaround to reset the player's inventory until we support all actions fully
            CALL "Inventory-SyncPlayerInventory" USING PLAYER-ID
    END-EVALUATE

    GOBACK.

StartedDigging.
    EVALUATE TRUE
        *> disallow block breaking in adventure and spectator mode
        WHEN PLAYER-GAMEMODE(PLAYER-ID) = 2 OR 3
            CONTINUE

        *> creative
        WHEN PLAYER-GAMEMODE(PLAYER-ID) = 1
            PERFORM BreakBlock

        *> survival
        WHEN OTHER
            CALL "World-CheckBounds" USING LOCATION BOUNDS-CHECK
            IF BOUNDS-CHECK = 0
                *> TODO take held item, potion effects, etc. into account for instamine
                CALL "World-GetBlock" USING LOCATION BLOCK-STATE
                CALL "Blocks-ForStateId" USING BLOCK-STATE BLOCK-ID
                IF BLOCK-ID <= 0
                    PERFORM StartBreakingBlock
                ELSE
                    CALL "Blocks-GetHardness" USING BLOCK-ID BLOCK-HARDNESS
                    IF BLOCK-HARDNESS > 0
                        PERFORM StartBreakingBlock
                    ELSE
                        PERFORM BreakBlock
                    END-IF
                END-IF
            END-IF
            CONTINUE
    END-EVALUATE

    CALL "SendPacket-AckBlockChange" USING LK-CLIENT SEQUENCE-ID
    .

CancelledDigging.
    PERFORM ResetBlockBreaking
    CALL "SendPacket-AckBlockChange" USING LK-CLIENT SEQUENCE-ID
    .

FinishedDigging.
    *> TODO check whether the player was actually breaking the block...
    PERFORM ResetBlockBreaking
    IF PLAYER-GAMEMODE(PLAYER-ID) = 0
        PERFORM BreakBlock
    END-IF
    CALL "SendPacket-AckBlockChange" USING LK-CLIENT SEQUENCE-ID
    .

*> called by either StartedDigging or FinishedDigging, depending on gamemode and block destroy speed
BreakBlock.
    CALL "World-CheckBounds" USING LOCATION BOUNDS-CHECK
    IF BOUNDS-CHECK = 0
        CALL "World-GetBlock" USING LOCATION BLOCK-STATE
        CALL "GetCallback-BlockDestroy" USING BLOCK-STATE CALLBACK-PTR
        IF CALLBACK-PTR NOT = NULL
            CALL CALLBACK-PTR USING PLAYER-ID LOCATION BLOCK-FACE
        END-IF
    END-IF
    PERFORM ResetBlockBreaking
    .

StartBreakingBlock.
    MOVE LOCATION TO PLAYER-BLOCK-BREAKING-POSITION(PLAYER-ID)
    MOVE 0 TO PLAYER-BLOCK-BREAKING-STAGE(PLAYER-ID)
    *> TODO update the block breaking stage continuously until the block is broken
    .

ResetBlockBreaking.
    IF PLAYER-BLOCK-BREAKING-STAGE(PLAYER-ID) < 0
        EXIT PARAGRAPH
    END-IF
    MOVE -1 TO PLAYER-BLOCK-BREAKING-STAGE(PLAYER-ID)
    PERFORM VARYING OTHER-CLIENT FROM 1 BY 1 UNTIL OTHER-CLIENT > MAX-CLIENTS
        IF CLIENT-STATE(OTHER-CLIENT) = CLIENT-STATE-PLAY AND OTHER-CLIENT NOT = LK-CLIENT
            CALL "SendPacket-BlockDestruction" USING OTHER-CLIENT PLAYER-ID PLAYER-BLOCK-BREAKING-POSITION(PLAYER-ID)
                PLAYER-BLOCK-BREAKING-STAGE(PLAYER-ID)
        END-IF
    END-PERFORM
    .

DropItemStack.
    COMPUTE SLOT-INDEX = 36 + PLAYER-HOTBAR(PLAYER-ID)
    MOVE PLAYER-INVENTORY-SLOT(PLAYER-ID, SLOT-INDEX + 1) TO DROP-SLOT
    IF DROP-SLOT-COUNT > 0
        MOVE 0 TO PLAYER-INVENTORY-SLOT-COUNT(PLAYER-ID, SLOT-INDEX + 1)
        CALL "World-DropItem-FromPlayer" USING DROP-SLOT PLAYER-ID
    END-IF
    .

DropItem.
    COMPUTE SLOT-INDEX = 36 + PLAYER-HOTBAR(PLAYER-ID)
    MOVE PLAYER-INVENTORY-SLOT(PLAYER-ID, SLOT-INDEX + 1) TO DROP-SLOT
    IF DROP-SLOT-COUNT > 0
        MOVE 1 TO DROP-SLOT-COUNT
        SUBTRACT 1 FROM PLAYER-INVENTORY-SLOT-COUNT(PLAYER-ID, SLOT-INDEX + 1)
        CALL "World-DropItem-FromPlayer" USING DROP-SLOT PLAYER-ID
    END-IF
    .

SwapOffhand.
    COMPUTE SLOT-INDEX = 36 + PLAYER-HOTBAR(PLAYER-ID)
    MOVE PLAYER-INVENTORY-SLOT(PLAYER-ID, SLOT-INDEX + 1) TO DROP-SLOT
    MOVE PLAYER-INVENTORY-SLOT(PLAYER-ID, 46) TO PLAYER-INVENTORY-SLOT(PLAYER-ID, SLOT-INDEX + 1)
    MOVE DROP-SLOT TO PLAYER-INVENTORY-SLOT(PLAYER-ID, 46)
    CALL "Inventory-SyncPlayerInventory" USING PLAYER-ID
    .

END PROGRAM RecvPacket-PlayerAction.
