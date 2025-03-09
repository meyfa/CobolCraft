*> --- BlocksLoot-Drop ---
*> Helper function to be called by the BlocksLootTable generated code.
*> Drops 1 piece of a named item at the given block position.
IDENTIFICATION DIVISION.
PROGRAM-ID. BlocksLoot-Drop.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 DROP-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==DROP==.
LINKAGE SECTION.
    01 LK-POSITION.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.
    01 LK-ITEM-NAME             PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-POSITION LK-ITEM-NAME.
    CALL "Registries-Get-EntryId" USING "minecraft:item" LK-ITEM-NAME DROP-SLOT-ID
    IF DROP-SLOT-ID > 0
        MOVE 1 TO DROP-SLOT-COUNT

        *> TODO item components
        MOVE 2 TO DROP-SLOT-NBT-LENGTH
        MOVE X"0000" TO DROP-SLOT-NBT-DATA(1:2)

        CALL "World-DropItem-FromBlock" USING DROP-SLOT LK-POSITION
    END-IF

    GOBACK.

END PROGRAM BlocksLoot-Drop.
