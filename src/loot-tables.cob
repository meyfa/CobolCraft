*> --- LootTables-NumberBinomial ---
*> Evaluate a binomial distribution number provider.
IDENTIFICATION DIVISION.
PROGRAM-ID. LootTables-NumberBinomial.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NUM-TRIALS               BINARY-LONG UNSIGNED.
    01 PROBABILITY              FLOAT-LONG.
LINKAGE SECTION.
    01 LK-N-STR                 PIC X ANY LENGTH.
    01 LK-P-STR                 PIC X ANY LENGTH.
    01 LK-RESULT                FLOAT-LONG.

PROCEDURE DIVISION USING LK-N-STR LK-P-STR LK-RESULT.
    MOVE 0 TO LK-RESULT

    MOVE FUNCTION NUMVAL(LK-N-STR) TO NUM-TRIALS
    MOVE FUNCTION NUMVAL(LK-P-STR) TO PROBABILITY

    PERFORM NUM-TRIALS TIMES
        IF FUNCTION RANDOM < PROBABILITY
            ADD 1 TO LK-RESULT
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM LootTables-NumberBinomial.

*> --- LootTables-NumberUniform ---
*> Evaluate a uniform distribution number provider.
IDENTIFICATION DIVISION.
PROGRAM-ID. LootTables-NumberUniform.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 MINVAL                   FLOAT-LONG.
    01 MAXVAL                   FLOAT-LONG.
LINKAGE SECTION.
    01 LK-MIN-STR               PIC X ANY LENGTH.
    01 LK-MAX-STR               PIC X ANY LENGTH.
    01 LK-RESULT                FLOAT-LONG.

PROCEDURE DIVISION USING LK-MIN-STR LK-MAX-STR LK-RESULT.
    MOVE FUNCTION NUMVAL(LK-MIN-STR) TO MINVAL
    MOVE FUNCTION NUMVAL(LK-MAX-STR) TO MAXVAL

    *> random number between min and max, inclusive
    COMPUTE LK-RESULT = MINVAL + FUNCTION RANDOM * (MAXVAL - MINVAL + 1)

    GOBACK.

END PROGRAM LootTables-NumberUniform.

*> --- LootTables-BlockState ---
*> Evaluate a block state condition. This checks whether the mined block has the correct ID, and can additionally
*> check the block's properties, such as whether a slab is a double slab.
IDENTIFICATION DIVISION.
PROGRAM-ID. LootTables-BlockState.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==BLOCK==.
    01 IDX                      BINARY-LONG UNSIGNED.
    01 IDX2                     BINARY-LONG UNSIGNED.
    01 PROPERTY-NAME            PIC X(16).
    01 PROPERTY-VALUE           PIC X(16).
    01 ACTUAL-VALUE             PIC X(16).
LINKAGE SECTION.
    01 LK-BLOCK-ID              BINARY-LONG UNSIGNED.
    *> The expected name of the block, such as "minecraft:stone_slab".
    01 LK-COND-NAME             PIC X ANY LENGTH.
    *> The expected properties of the block, such as "type=double". Multiple properties are separated by spaces.
    01 LK-COND-PROPERTIES       PIC X ANY LENGTH.
    *> The result; unchanged if the chance is met, set to 0 otherwise.
    01 LK-COND                  BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-BLOCK-ID LK-COND-NAME LK-COND-PROPERTIES LK-COND.
    CALL "Blocks-ToDescription" USING LK-BLOCK-ID BLOCK-DESCRIPTION

    IF BLOCK-NAME NOT = LK-COND-NAME
        MOVE 0 TO LK-COND
        GOBACK
    END-IF

    IF LK-COND-PROPERTIES = SPACES
        GOBACK *> nothing to check
    END-IF

    PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > FUNCTION LENGTH(LK-COND-PROPERTIES)
        *> find the next equals sign
        PERFORM VARYING IDX2 FROM IDX BY 1 UNTIL IDX2 > FUNCTION LENGTH(LK-COND-PROPERTIES) OR LK-COND-PROPERTIES(IDX2:1) = "="
            CONTINUE
        END-PERFORM

        MOVE LK-COND-PROPERTIES(IDX:IDX2 - IDX) TO PROPERTY-NAME
        COMPUTE IDX = IDX2 + 1

        *> find the next space
        PERFORM VARYING IDX2 FROM IDX BY 1 UNTIL IDX2 > FUNCTION LENGTH(LK-COND-PROPERTIES) OR LK-COND-PROPERTIES(IDX2:1) = SPACE
            CONTINUE
        END-PERFORM

        MOVE LK-COND-PROPERTIES(IDX:IDX2 - IDX) TO PROPERTY-VALUE

        CALL "Blocks-Description-GetValue" USING BLOCK-DESCRIPTION PROPERTY-NAME ACTUAL-VALUE
        IF ACTUAL-VALUE NOT = PROPERTY-VALUE
            MOVE 0 TO LK-COND
            GOBACK
        END-IF

        MOVE IDX2 TO IDX
    END-PERFORM

    GOBACK.

END PROGRAM LootTables-BlockState.

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

*> --- LootTables-SetCount ---
*> Apply the "minecraft:set_count" function to a loot table item.
IDENTIFICATION DIVISION.
PROGRAM-ID. LootTables-SetCount.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-ITEM-COUNT            BINARY-LONG.
    01 LK-SET-COUNT             FLOAT-LONG.
    01 LK-ADDITIVE              PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-ITEM-COUNT LK-SET-COUNT LK-ADDITIVE.
    IF LK-ADDITIVE = "true"
        ADD LK-SET-COUNT TO LK-ITEM-COUNT
    ELSE
        MOVE LK-SET-COUNT TO LK-ITEM-COUNT
    END-IF.

END PROGRAM LootTables-SetCount.

*> --- LootTables-LimitCount ---
*> Apply the "minecraft:limit_count" function to a loot table item.
IDENTIFICATION DIVISION.
PROGRAM-ID. LootTables-LimitCount.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 MINVAL                   FLOAT-LONG.
    01 MAXVAL                   FLOAT-LONG.
LINKAGE SECTION.
    01 LK-ITEM-COUNT            BINARY-LONG.
    01 LK-MIN-STR               PIC X ANY LENGTH.
    01 LK-MAX-STR               PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-ITEM-COUNT LK-MIN-STR LK-MAX-STR.
    IF LK-ITEM-COUNT < FUNCTION NUMVAL(LK-MIN-STR)
        MOVE FUNCTION NUMVAL(LK-MIN-STR) TO LK-ITEM-COUNT
    ELSE IF LK-ITEM-COUNT > FUNCTION NUMVAL(LK-MAX-STR)
        MOVE FUNCTION NUMVAL(LK-MAX-STR) TO LK-ITEM-COUNT
    END-IF.

END PROGRAM LootTables-LimitCount.

*> --- LootTables-ExplosionDecay ---
*> Apply the "minecraft:explosion_decay" function to a loot table item.
IDENTIFICATION DIVISION.
PROGRAM-ID. LootTables-ExplosionDecay.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-ITEM-COUNT            BINARY-LONG.

PROCEDURE DIVISION USING LK-ITEM-COUNT.
    *> TODO implement
    GOBACK.

END PROGRAM LootTables-ExplosionDecay.

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
    01 LK-POOL-ITEMS.
        02 LK-POOL-ITEM         OCCURS 0 TO 16 TIMES DEPENDING ON LK-POOL-SIZE.
            03 LK-ITEM-ID       PIC X(64).
            03 LK-ITEM-COUNT    BINARY-LONG.
    01 LK-POSITION.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.

PROCEDURE DIVISION USING LK-POOL-SIZE LK-POOL-ITEMS LK-POSITION.
    IF LK-POOL-SIZE = 0
        GOBACK
    END-IF

    COMPUTE RANDOM-INDEX = (FUNCTION RANDOM * LK-POOL-SIZE) + 1

    IF LK-ITEM-COUNT(RANDOM-INDEX) <= 0
        GOBACK
    END-IF

    CALL "Registries-Lookup" USING "minecraft:item" LK-ITEM-ID(RANDOM-INDEX) DROP-SLOT-ID
    IF DROP-SLOT-ID > 0
        MOVE LK-ITEM-COUNT(RANDOM-INDEX) TO DROP-SLOT-COUNT

        *> TODO item components
        MOVE 2 TO DROP-SLOT-NBT-LENGTH
        MOVE X"0000" TO DROP-SLOT-NBT-DATA(1:2)

        CALL "World-DropItem-FromBlock" USING DROP-SLOT LK-POSITION
    END-IF

    GOBACK.

END PROGRAM BlocksLoot-DropRandom.
