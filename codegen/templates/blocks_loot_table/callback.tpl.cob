
    IDENTIFICATION DIVISION.
    PROGRAM-ID. $PROGID$.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 COND                     BINARY-CHAR UNSIGNED.
    LINKAGE SECTION.
        01 LK-POS.
            02 LK-X                 BINARY-LONG.
            02 LK-Y                 BINARY-LONG.
            02 LK-Z                 BINARY-LONG.
        01 LK-SURVIVES-EXPLOSION    BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-POS LK-SURVIVES-EXPLOSION.
$BODY$
        GOBACK.

    END PROGRAM $PROGID$.
