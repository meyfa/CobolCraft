*> --- Copybook: data structure for block state descriptions ---
*> Usage: COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==<name>==.
01 PREFIX-DESCRIPTION.
    02 PREFIX-NAME              PIC X(64).
    02 PREFIX-PROPERTY-COUNT    BINARY-LONG UNSIGNED.
    02 PREFIX-PROPERTY          OCCURS 16 TIMES.
        03 PREFIX-PROPERTY-NAME     PIC X(64).
        03 PREFIX-PROPERTY-VALUE    PIC X(64).
