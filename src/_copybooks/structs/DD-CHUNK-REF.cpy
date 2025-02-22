*> --- Copybook: reference to a loaded chunk structure ---

78 CHUNK-SECTION-COUNT              VALUE 24.

01 CHUNK BASED.
    02 CHUNK-DIRTY-BLOCKS           BINARY-CHAR UNSIGNED.
    02 CHUNK-DIRTY-ENTITIES         BINARY-CHAR UNSIGNED.
    02 CHUNK-X                      BINARY-LONG.
    02 CHUNK-Z                      BINARY-LONG.
    02 CHUNK-SECTIONS.
        03 CHUNK-SECTION                OCCURS CHUNK-SECTION-COUNT TIMES.
            04 CHUNK-SECTION-NON-AIR    BINARY-LONG UNSIGNED.
            *> block IDs (16x16x16) - X increases fastest, then Z, then Y
            04 CHUNK-SECTION-BLOCKS.
                05 CHUNK-SECTION-BLOCK  OCCURS 4096 TIMES BINARY-LONG UNSIGNED.
            *> biome IDs (4x4x4)
            04 CHUNK-SECTION-BIOMES.
                05 CHUNK-SECTION-BIOME  OCCURS 64 TIMES BINARY-LONG UNSIGNED.
    02 CHUNK-BLOCK-ENTITIES.
        03 CHUNK-BLOCK-ENTITY-COUNT     BINARY-LONG UNSIGNED.
        *> block entity IDs for each block
        03 CHUNK-BLOCK-ENTITY-IDS.
            *> set to a value < 0 to indicate no entity (since 0 is a valid ID)
            *> TODO: support storing entity data, not just IDs
            04 CHUNK-BLOCK-ENTITY-ID    OCCURS 98304 TIMES BINARY-CHAR.
    02 CHUNK-ENTITIES.
        03 CHUNK-ENTITY-COUNT           BINARY-LONG UNSIGNED.
        *> linked list of entities in the chunk - see DD-CHUNK-ENTITY.cpy
        03 CHUNK-ENTITY-LIST            POINTER.
