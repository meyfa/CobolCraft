*> --- Copybook: shared data for item types ---
*> Note: Mappings between item names and IDs are part of the generic registry system; see DD-REGISTRIES. This copybook
*> is used to store specific data only relevant to items.

78 ITEMS-CAPACITY VALUE 2000.

01 ITEMS EXTERNAL.
    *> The number of items loaded. This should equal the registry size, but might not, if the data files diverge.
    *> Since the table is easily indexed by item ID, the count is only used for logging purposes.
    02 ITEMS-COUNT BINARY-LONG UNSIGNED.
    *> Indexed by item ID +1, since item IDs are 0-based.
    02 ITEM-ENTRY OCCURS ITEMS-CAPACITY TIMES.
        *> The "minecraft:max_stack_size" data component
        03 ITEM-ENTRY-MAX-STACK-SIZE BINARY-LONG UNSIGNED.
