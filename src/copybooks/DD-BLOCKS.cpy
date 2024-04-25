*> --- Copybook: shared data for block states ---
01 BLOCKS EXTERNAL.
    02 BLOCKS-COUNT BINARY-LONG UNSIGNED.
    02 BLOCK-ENTRY OCCURS 2000 TIMES.
        03 BLOCK-ENTRY-NAME PIC X(100).
        *> We make use of the fact that each block state corresponds to a combination of properties,
        *> and that they occur in a predictable order. This allows us to use a single array per block
        *> to store the properties instead of one array per state.
        03 BLOCK-ENTRY-PROPERTY-COUNT BINARY-LONG UNSIGNED.
        03 BLOCK-ENTRY-PROPERTY OCCURS 16 TIMES.
            04 BLOCK-ENTRY-PROPERTY-NAME PIC X(64).
            04 BLOCK-ENTRY-PROPERTY-VALUE-COUNT BINARY-LONG UNSIGNED.
            04 BLOCK-ENTRY-PROPERTY-VALUE PIC X(64) OCCURS 32 TIMES.
        03 BLOCK-ENTRY-STATES-COUNT BINARY-LONG UNSIGNED.
        *> This may seem excessive, but there are blocks that really need this much space for block states.
        *> For instance, redstone wire has 1296 states (3 power states per north/east/south/west * 15 power levels).
        *> Even the note block has 1150 states (23 instruments * 25 notes * 2 power states).
        03 BLOCK-ENTRY-STATE OCCURS 1300 TIMES.
            04 BLOCK-ENTRY-STATE-ID BINARY-LONG UNSIGNED.
            04 BLOCK-ENTRY-STATE-IS-DEFAULT BINARY-CHAR UNSIGNED.
        03 BLOCK-ENTRY-DEFAULT-STATE BINARY-LONG.
