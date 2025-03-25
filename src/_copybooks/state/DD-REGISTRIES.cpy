*> --- Copybook: shared data for game registries ---
01 REGISTRIES EXTERNAL.
    *> Number of present registries
    02 REGISTRY-COUNT BINARY-LONG UNSIGNED.

    *> Registries indexed by ID (+1), must be without gaps
    02 REGISTRY OCCURS 100 TIMES.
        *> Resource location of the registry
        03 REGISTRY-NAME PIC X(64).
        03 REGISTRY-REQUIRES-PACKET BINARY-CHAR UNSIGNED.

        *> Number of entries in the registry
        03 REGISTRY-ENTRY-COUNT BINARY-LONG UNSIGNED.

        *> Entries indexed by ID (+1), must be without gaps
        03 REGISTRY-ENTRY OCCURS 2000 TIMES.
            04 REGISTRY-ENTRY-NAME PIC X(64).
