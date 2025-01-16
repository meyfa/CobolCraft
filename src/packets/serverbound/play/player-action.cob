IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-PlayerAction.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
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
    01 BLOCK-STATE-ID           BINARY-LONG.
    01 BOUNDS-CHECK             BINARY-CHAR UNSIGNED.
    01 CALLBACK-PTR             PROGRAM-POINTER.
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
        *> started digging
        WHEN ACTION-ENUM = 0
            *> Check the position validity
            CALL "World-CheckBounds" USING LOCATION BOUNDS-CHECK
            IF BOUNDS-CHECK = 0
                *> Call the block's destroy handler
                CALL "World-GetBlock" USING LOCATION BLOCK-STATE-ID
                CALL "GetCallback-BlockDestroy" USING BLOCK-STATE-ID CALLBACK-PTR
                IF CALLBACK-PTR NOT = NULL
                    CALL CALLBACK-PTR USING PLAYER-ID LOCATION BLOCK-FACE
                END-IF
            END-IF

            *> Acknowledge the action
            CALL "SendPacket-AckBlockChange" USING LK-CLIENT SEQUENCE-ID

            GOBACK
    END-EVALUATE

    *> TODO The following is a workaround to reset the player's inventory, since we don't implement dropping items yet.
    ADD 1 TO PLAYER-CONTAINER-STATE-ID(PLAYER-ID)
    CALL "SendPacket-SetContainerContent" USING LK-CLIENT PLAYER-CONTAINER-STATE-ID(PLAYER-ID)
        PLAYER-INVENTORY(PLAYER-ID) PLAYER-MOUSE-ITEM(PLAYER-ID)

    GOBACK.

END PROGRAM RecvPacket-PlayerAction.
