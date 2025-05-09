*> --- Copybook: structure for codegen templates ---

01 PREFIX-TPL.
    02 PREFIX-TPL-FILENAME          PIC X(1024).
    02 PREFIX-TPL-BUFFER            PIC X(1024).
    02 PREFIX-TPL-LENGTH            BINARY-LONG UNSIGNED.
    *> replacement function for the template
    02 PREFIX-TPL-REPLACE           PROGRAM-POINTER.
    *> number of variables in the template
    02 PREFIX-TPL-VARS              BINARY-LONG UNSIGNED.
    02 PREFIX-TPL-VAR OCCURS 0 TO 16 TIMES DEPENDING ON PREFIX-TPL-VARS.
        *> 1-based index of the beginning $ for the variable
        03 PREFIX-TPL-VAR-START     BINARY-LONG UNSIGNED.
        *> 1-based index of the ending $ for the variable
        03 PREFIX-TPL-VAR-END       BINARY-LONG UNSIGNED.
        *> variable name (excluding the $)
        03 PREFIX-TPL-VAR-NAME      PIC X(32).
        *> indentation (number of spaces) to apply to child templates rendered within this variable
        03 PREFIX-TPL-VAR-INDENT    BINARY-LONG UNSIGNED.
        *> whether to add a newline after the variable - especially useful for repeated variables
        03 PREFIX-TPL-VAR-NEWLINE   BINARY-CHAR UNSIGNED.
