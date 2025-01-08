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
    *> TODO: support containers other than the player inventory
    01 LK-INVENTORY.
        02 LK-SLOT OCCURS 46 TIMES.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.

PROCEDURE DIVISION USING LK-CLIENT LK-INVENTORY.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> window ID (0 = player inventory)
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> state ID = 0
    *> TODO: implement state ID management
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> count = 46 (https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Inventory)
    MOVE 46 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> slot data
    PERFORM VARYING SLOT-INDEX FROM 1 BY 1 UNTIL SLOT-INDEX > 46
        IF LK-SLOT-ID(SLOT-INDEX) < 0 OR LK-SLOT-COUNT(SLOT-INDEX) = 0
            *> count = 0
            MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
            ADD 1 TO PAYLOADPOS
        ELSE
            *> count
            CALL "Encode-Byte" USING LK-SLOT-COUNT(SLOT-INDEX) PAYLOAD PAYLOADPOS

            *> item ID
            CALL "Encode-VarInt" USING LK-SLOT-ID(SLOT-INDEX) PAYLOAD PAYLOADPOS

            *> NBT data
            MOVE LK-SLOT-NBT-LENGTH(SLOT-INDEX) TO INT32
            MOVE LK-SLOT-NBT-DATA(SLOT-INDEX)(1:INT32) TO PAYLOAD(PAYLOADPOS:INT32)
            ADD INT32 TO PAYLOADPOS
        END-IF
    END-PERFORM

    *> carried item (empty)
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-SetContainerContent.
