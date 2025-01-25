IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-SetCreativeSlot.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    01 PLAYER-ID                BINARY-LONG.
    01 SLOT-NUMBER              BINARY-SHORT.
    01 DECODE-SLOT-COUNT        BINARY-CHAR.
    01 CLIENT-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==CLIENT==.
    01 COMPONENTS-OFFSET        BINARY-LONG UNSIGNED.
    01 COMPONENTS-ADD-COUNT     BINARY-LONG.
    01 COMPONENTS-REMOVE-COUNT  BINARY-LONG.
    01 COMPONENTS-LENGTH        BINARY-LONG UNSIGNED.
    01 COMPONENT-ID             BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    CALL "Decode-Short" USING LK-BUFFER LK-OFFSET SLOT-NUMBER
    PERFORM DecodeSlot

    IF PLAYER-GAMEMODE(PLAYER-ID) NOT = 1
        GOBACK
    END-IF

    EVALUATE SLOT-NUMBER
        WHEN 0 THRU 45
            MOVE CLIENT-SLOT TO PLAYER-INVENTORY-SLOT(PLAYER-ID, SLOT-NUMBER + 1)
        WHEN -1
            *> TODO: spawn item entity
            CONTINUE
        WHEN OTHER
            DISPLAY "Invalid slot number: " SLOT-NUMBER
    END-EVALUATE

    GOBACK.

DecodeSlot.
    *> TODO deduplicate slot decoding with "container click" packet

    *> count
    CALL "Decode-Byte" USING LK-BUFFER LK-OFFSET DECODE-SLOT-COUNT
    MOVE DECODE-SLOT-COUNT TO CLIENT-SLOT-COUNT

    IF DECODE-SLOT-COUNT = 0
        MOVE 0 TO CLIENT-SLOT-ID
    ELSE
        *> id
        CALL "Decode-VarInt" USING LK-BUFFER LK-OFFSET CLIENT-SLOT-ID

        *> components
        MOVE LK-OFFSET TO COMPONENTS-OFFSET
        CALL "Decode-VarInt" USING LK-BUFFER COMPONENTS-OFFSET COMPONENTS-ADD-COUNT
        CALL "Decode-VarInt" USING LK-BUFFER COMPONENTS-OFFSET COMPONENTS-REMOVE-COUNT
        PERFORM COMPONENTS-ADD-COUNT TIMES
            CALL "Components-LengthOf" USING LK-BUFFER COMPONENTS-OFFSET COMPONENTS-LENGTH
            ADD COMPONENTS-LENGTH TO COMPONENTS-OFFSET
        END-PERFORM
        PERFORM COMPONENTS-REMOVE-COUNT TIMES
            CALL "Decode-VarInt" USING LK-BUFFER COMPONENTS-OFFSET COMPONENT-ID
        END-PERFORM

        COMPUTE CLIENT-SLOT-NBT-LENGTH = COMPONENTS-OFFSET - LK-OFFSET
        IF CLIENT-SLOT-NBT-LENGTH <= 1024
            MOVE LK-BUFFER(LK-OFFSET:CLIENT-SLOT-NBT-LENGTH) TO CLIENT-SLOT-NBT-DATA(1:CLIENT-SLOT-NBT-LENGTH)
        ELSE
            MOVE 0 TO CLIENT-SLOT-NBT-LENGTH
            DISPLAY "Item NBT data too long: " CLIENT-SLOT-NBT-LENGTH
        END-IF

        MOVE COMPONENTS-OFFSET TO LK-OFFSET
    END-IF

    EXIT PARAGRAPH.

END PROGRAM RecvPacket-SetCreativeSlot.
