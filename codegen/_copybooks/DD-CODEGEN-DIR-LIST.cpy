*> --- Copybook: structure for directory listings ---

01 PREFIX-DIR.
    02 PREFIX-DIR-SIZE              BINARY-LONG UNSIGNED.
    02 PREFIX-DIR-NAMES.
        03 PREFIX-DIR-NAME          PIC X(128)
            OCCURS 0 TO 4096 TIMES DEPENDING ON PREFIX-DIR-SIZE.
