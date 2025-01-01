*> --- Copybook: shared data for block states ---

78 BLOCKS-CAPACITY VALUE 2000.

01 BLOCKS EXTERNAL.
    02 BLOCKS-COUNT BINARY-LONG UNSIGNED.
    02 BLOCK-ENTRY OCCURS BLOCKS-CAPACITY TIMES.
        03 BLOCK-ENTRY-NAME PIC X(100).
        *> The value of "definition"."type". For example, "minecraft:slab" for slab blocks.
        03 BLOCK-ENTRY-TYPE PIC X(100).
        *> We make use of the fact that each block state corresponds to a combination of properties,
        *> and that they occur in a predictable order. This allows us to use a single array per block
        *> to store the properties instead of one array per state.
        03 BLOCK-ENTRY-PROPERTY-COUNT BINARY-LONG UNSIGNED.
        03 BLOCK-ENTRY-PROPERTY OCCURS 16 TIMES.
            04 BLOCK-ENTRY-PROPERTY-NAME PIC X(32).
            04 BLOCK-ENTRY-PROPERTY-VALUE-COUNT BINARY-LONG UNSIGNED.
            04 BLOCK-ENTRY-PROPERTY-VALUE PIC X(32) OCCURS 32 TIMES.
        *> Some blocks have a lot of states. For instance, redstone wire has 1296 states (3 power states per
        *> north/east/south/west * 15 power levels).
        *> To save memory, we only store the minimum and maximum IDs instead of the entire list.
        03 BLOCK-ENTRY-MINIMUM-STATE-ID BINARY-LONG UNSIGNED.
        03 BLOCK-ENTRY-MAXIMUM-STATE-ID BINARY-LONG UNSIGNED.
        03 BLOCK-ENTRY-DEFAULT-STATE-ID BINARY-LONG UNSIGNED.
    *> This table is sorted (SORT) by the block name, and is used for binary search (SEARCH ALL) to find the block's
    *> index in the BLOCK-ENTRY table.
    02 BLOCK-NAMES OCCURS BLOCKS-CAPACITY TIMES
            ASCENDING KEY IS BLOCK-NAMES-ENTRY-NAME
            INDEXED BY BLOCK-NAMES-INDEX.
        03 BLOCK-NAMES-ENTRY-NAME PIC X(100).
        03 BLOCK-NAMES-ENTRY-INDEX BINARY-LONG UNSIGNED.
