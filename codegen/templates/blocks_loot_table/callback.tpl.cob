
IDENTIFICATION DIVISION.
PROGRAM-ID. $progid$.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 COND                     BINARY-CHAR UNSIGNED.
    01 COND-BACKUP              BINARY-CHAR UNSIGNED.
    01 NUM                      FLOAT-LONG.
    01 POOL-SIZE                BINARY-LONG UNSIGNED.
    01 POOL-ITEMS.
        02 POOL-ITEM            OCCURS 0 TO 16 TIMES DEPENDING ON POOL-SIZE.
            03 ITEM-ID          PIC X(64).
            03 ITEM-COUNT       BINARY-LONG.
LINKAGE SECTION.
    01 LK-POS.
        02 LK-X                 BINARY-LONG.
        02 LK-Y                 BINARY-LONG.
        02 LK-Z                 BINARY-LONG.
    01 LK-BLOCK-ID              BINARY-LONG UNSIGNED.
    01 LK-SURVIVES-EXPLOSION    BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-POS LK-BLOCK-ID LK-SURVIVES-EXPLOSION.
$body:indent=4$
    GOBACK.

END PROGRAM $progid$.
