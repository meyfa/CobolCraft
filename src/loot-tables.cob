*> --- LootTables-RandomChance ---
*> Evaluate a random chance condition.
IDENTIFICATION DIVISION.
PROGRAM-ID. LootTables-RandomChance.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 RANDOM-NUMBER            FLOAT-LONG.
LINKAGE SECTION.
    *> The chance, such as "0.125". Given as a string to avoid a working-storage item in each caller.
    01 LK-CHANCE                PIC X ANY LENGTH.
    *> The result; unchanged if the chance is met, set to 0 otherwise.
    01 LK-COND                  BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-CHANCE LK-COND.
    *> TODO Use a better random number generator - dependent on the loot table's "random_sequence".
    MOVE FUNCTION RANDOM TO RANDOM-NUMBER
    IF RANDOM-NUMBER >= FUNCTION NUMVAL(LK-CHANCE)
        MOVE 0 TO LK-COND
    END-IF.

END PROGRAM LootTables-RandomChance.

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
