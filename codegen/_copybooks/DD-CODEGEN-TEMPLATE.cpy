40 PREFIX-TPL-FILENAME          PIC X(1024).
40 PREFIX-TPL-BUFFER            PIC X(1024).
40 PREFIX-TPL-LENGTH            BINARY-LONG UNSIGNED.
*> number of variables in the template
40 PREFIX-TPL-VARS              BINARY-LONG UNSIGNED.
40 PREFIX-TPL-VAR OCCURS 0 TO 16 TIMES DEPENDING ON PREFIX-TPL-VARS.
    *> 1-based index of the beginning $ for the variable
    41 PREFIX-TPL-VAR-START     BINARY-LONG UNSIGNED.
    *> 1-based index of the ending $ for the variable
    41 PREFIX-TPL-VAR-END       BINARY-LONG UNSIGNED.
    *> variable name (excluding the $)
    41 PREFIX-TPL-VAR-NAME      PIC X(32).
