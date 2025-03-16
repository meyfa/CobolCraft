MOVE 1 TO COND
$conditions$
IF COND NOT = 0
    MOVE 0 TO POOL-SIZE
$body:indent=4$
    CALL "BlocksLoot-DropRandom" USING POOL-SIZE POOL-ITEMS LK-POS
END-IF
