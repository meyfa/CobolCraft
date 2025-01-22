IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-SetContainerContent.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:container_set_content".
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(64000).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 SLOT-INDEX       BINARY-LONG UNSIGNED.
    01 INT32            BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-STATE-ID      BINARY-LONG.
    01 LK-INVENTORY.
        02 LK-SLOT OCCURS 46 TIMES.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
    01 LK-MOUSE-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK-MOUSE==.

PROCEDURE DIVISION USING LK-CLIENT LK-STATE-ID LK-INVENTORY LK-MOUSE-SLOT.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> window ID (0 = player inventory)
    *> TODO: support containers other than the player inventory
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> state ID
    CALL "Encode-VarInt" USING LK-STATE-ID PAYLOAD PAYLOADPOS

    *> count = 46 (https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Inventory)
    MOVE 46 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> slot data
    PERFORM VARYING SLOT-INDEX FROM 1 BY 1 UNTIL SLOT-INDEX > 46
        CALL "EncodeSlot" USING LK-SLOT(SLOT-INDEX) PAYLOAD PAYLOADPOS
    END-PERFORM

    *> carried item (mouse slot)
    CALL "EncodeSlot" USING LK-MOUSE-SLOT PAYLOAD PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

    IDENTIFICATION DIVISION.
    PROGRAM-ID. EncodeSlot.

    DATA DIVISION.
    LINKAGE SECTION.
        01 LK-SLOT.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
        01 LK-BUFFER            PIC X ANY LENGTH.
        01 LK-OFFSET            BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-SLOT LK-BUFFER LK-OFFSET.
        IF LK-SLOT-ID < 0 OR LK-SLOT-COUNT = 0
            *> count = 0
            MOVE X"00" TO LK-BUFFER(LK-OFFSET:1)
            ADD 1 TO LK-OFFSET
        ELSE
            *> count
            CALL "Encode-Byte" USING LK-SLOT-COUNT LK-BUFFER LK-OFFSET
            *> item ID
            CALL "Encode-VarInt" USING LK-SLOT-ID LK-BUFFER LK-OFFSET
            *> NBT data
            MOVE LK-SLOT-NBT-DATA(1:LK-SLOT-NBT-LENGTH) TO LK-BUFFER(LK-OFFSET:LK-SLOT-NBT-LENGTH)
            ADD LK-SLOT-NBT-LENGTH TO LK-OFFSET
        END-IF
        GOBACK.

END PROGRAM SendPacket-SetContainerContent.
