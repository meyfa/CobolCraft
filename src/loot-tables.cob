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

*> --- LootTables-TableBonus ---
*> Evaluate a table bonus condition. Depending on an enchantment level, a different random chance is used
*> to determine if the condition is met.
IDENTIFICATION DIVISION.
PROGRAM-ID. LootTables-TableBonus.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 IDX                      BINARY-LONG UNSIGNED.
    01 RANDOM-NUMBER            FLOAT-LONG.
LOCAL-STORAGE SECTION.
    01 CHANCE-STRING            PIC X(64).
LINKAGE SECTION.
    *> The enchantment identifier.
    01 LK-ENCHANTMENT           PIC X ANY LENGTH.
    *> The space-separated list of chances, such as "0.005 0.01 0.02".
    01 LK-CHANCES               PIC X ANY LENGTH.
    *> The result; unchanged if the chance is met, set to 0 otherwise.
    01 LK-COND                  BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-ENCHANTMENT LK-CHANCES LK-COND.
    *> TODO: Obtain the enchantment level from the player's item. For now, assume it's not available (level 0).

    PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FUNCTION LENGTH(LK-CHANCES) OR LK-CHANCES(IDX:1) = SPACE
        MOVE LK-CHANCES(IDX:1) TO CHANCE-STRING(IDX:1)
    END-PERFORM.

    MOVE FUNCTION RANDOM TO RANDOM-NUMBER
    IF RANDOM-NUMBER >= FUNCTION NUMVAL(CHANCE-STRING)
        MOVE 0 TO LK-COND
    END-IF

    GOBACK.

END PROGRAM LootTables-TableBonus.

*> --- BlocksLoot-DropRandom ---
*> Helper function to be called by the BlocksLootTable generated code.
*> Drops random items from a pool at the given block position.
IDENTIFICATION DIVISION.
PROGRAM-ID. BlocksLoot-DropRandom.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 RANDOM-INDEX             BINARY-LONG.
    01 DROP-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==DROP==.
LINKAGE SECTION.
    01 LK-POOL-SIZE             BINARY-LONG UNSIGNED.
    01 LK-POOL-IDS.
        02 LK-POOL-ID           PIC X(64) OCCURS 0 TO 16 TIMES DEPENDING ON LK-POOL-SIZE.
    01 LK-POSITION.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.

PROCEDURE DIVISION USING LK-POOL-SIZE LK-POOL-IDS LK-POSITION.
    IF LK-POOL-SIZE = 0
        GOBACK
    END-IF

    COMPUTE RANDOM-INDEX = (FUNCTION RANDOM * LK-POOL-SIZE) + 1

    CALL "Registries-Get-EntryId" USING "minecraft:item" LK-POOL-ID(RANDOM-INDEX) DROP-SLOT-ID
    IF DROP-SLOT-ID > 0
        MOVE 1 TO DROP-SLOT-COUNT

        *> TODO item components
        MOVE 2 TO DROP-SLOT-NBT-LENGTH
        MOVE X"0000" TO DROP-SLOT-NBT-DATA(1:2)

        CALL "World-DropItem-FromBlock" USING DROP-SLOT LK-POSITION
    END-IF

    GOBACK.

END PROGRAM BlocksLoot-DropRandom.
