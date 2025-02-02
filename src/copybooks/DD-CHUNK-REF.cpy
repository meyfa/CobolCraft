*> --- Copybook: reference to a loaded chunk structure ---

78 CHUNK-SECTION-COUNT              VALUE 24.

01 CHUNK BASED.
    02 CHUNK-DIRTY                  BINARY-CHAR UNSIGNED.
    02 CHUNK-X                      BINARY-LONG.
    02 CHUNK-Z                      BINARY-LONG.
    02 CHUNK-SECTION                OCCURS CHUNK-SECTION-COUNT TIMES.
        03 CHUNK-SECTION-NON-AIR    BINARY-LONG UNSIGNED.
        *> block IDs (16x16x16) - X increases fastest, then Z, then Y
        03 CHUNK-SECTION-BLOCKS.
            04 CHUNK-BLOCK          OCCURS 4096 TIMES.
                05 CHUNK-BLOCK-ID   BINARY-LONG UNSIGNED.
        *> biome IDs (4x4x4)
        03 CHUNK-SECTION-BIOMES.
            04 CHUNK-BIOME          OCCURS 64 TIMES.
                05 CHUNK-BIOME-ID   BINARY-LONG UNSIGNED.
    02 CHUNK-BLOCK-ENTITY-COUNT     BINARY-LONG UNSIGNED.
    *> block entity IDs for each block
    02 CHUNK-BLOCK-ENTITIES.
        *> set to a value < 0 to indicate no entity (since 0 is a valid ID)
        *> TODO: support storing entity data, not just IDs
        03 CHUNK-BLOCK-ENTITY-ID    OCCURS 98304 TIMES BINARY-CHAR.
