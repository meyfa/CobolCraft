MOVE 1 TO COND
$conditions$
IF COND NOT = 0
    ADD 1 TO POOL-SIZE
    MOVE "$item-name$" TO ITEM-ID(POOL-SIZE)
    MOVE 1 TO ITEM-COUNT(POOL-SIZE)
$functions:indent=4$
END-IF
