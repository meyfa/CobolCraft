*> --- Copybook: shared data for the player whitelist ---

*> The maximum number of players that can be whitelisted.
78 MAX-WHITELIST-LENGTH VALUE 100.

01 WHITELIST EXTERNAL.
    02 WHITELIST-LENGTH BINARY-LONG UNSIGNED.
    02 WHITELIST-ENTRY OCCURS MAX-WHITELIST-LENGTH TIMES.
        03 WHITELIST-NAME PIC X(16).
        03 WHITELIST-UUID PIC X(16).
