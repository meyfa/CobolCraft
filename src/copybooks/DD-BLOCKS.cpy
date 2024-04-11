*> --- Copybook: shared data for block states ---
01 BLOCKS EXTERNAL.
    02 BLOCKS-COUNT BINARY-LONG UNSIGNED.
    02 BLOCK-ENTRY OCCURS 2000 TIMES.
        03 BLOCK-ENTRY-NAME PIC X(100).
        03 BLOCK-ENTRY-STATES-COUNT BINARY-LONG UNSIGNED.
        *> This may seem excessive, but there are blocks that really need this much space for block states.
        *> For instance, redstone wire has 1296 states (3 power states per north/east/south/west * 15 power levels).
        *> Even the note block has 1150 states (23 instruments * 25 notes * 2 power states).
        03 BLOCK-ENTRY-STATE OCCURS 1300 TIMES.
            04 BLOCK-ENTRY-STATE-ID BINARY-LONG UNSIGNED.
            04 BLOCK-ENTRY-STATE-IS-DEFAULT BINARY-CHAR UNSIGNED.
        03 BLOCK-ENTRY-DEFAULT-STATE BINARY-LONG.
