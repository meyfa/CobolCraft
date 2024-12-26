*> --- Copybook: callback parameters for command execution ---

*> Client index, 0 for server
01 LK-CLIENT-ID                 BINARY-CHAR.

*> Command name, followed by arguments
01 LK-PARTS.
    02 LK-PART-COUNT            BINARY-LONG UNSIGNED.
    02 LK-PART OCCURS 128 TIMES.
        03 LK-PART-VALUE        PIC X(256).
        03 LK-PART-LENGTH       BINARY-LONG UNSIGNED.

01 LK-PRINT-USAGE               BINARY-CHAR UNSIGNED.
