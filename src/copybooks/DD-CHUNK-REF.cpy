*> --- Copybook: reference to a loaded chunk structure ---

01 WORLD-CHUNK BASED.
    02 WORLD-CHUNK-DIRTY        BINARY-CHAR UNSIGNED.
    02 WORLD-CHUNK-X            BINARY-LONG.
    02 WORLD-CHUNK-Z            BINARY-LONG.
    02 WORLD-SECTION OCCURS WORLD-SECTION-COUNT TIMES.
        03 WORLD-SECTION-NON-AIR    BINARY-LONG UNSIGNED.
        *> block IDs (16x16x16) - X increases fastest, then Z, then Y
        03 WORLD-SECTION-BLOCKS.
            04 WORLD-BLOCK OCCURS 4096 TIMES.
                05 WORLD-BLOCK-ID           BINARY-LONG UNSIGNED.
        *> biome IDs (4x4x4)
        03 WORLD-SECTION-BIOMES.
            04 WORLD-BIOME OCCURS 64 TIMES.
                05 WORLD-BIOME-ID           BINARY-LONG UNSIGNED.
    02 WORLD-BLOCK-ENTITY-COUNT BINARY-LONG UNSIGNED.
    *> block entity IDs for each block
    02 WORLD-BLOCK-ENTITIES.
        *> set to a value < 0 to indicate no entity (since 0 is a valid ID)
        *> TODO: support storing entity data, not just IDs
        03 WORLD-BLOCK-ENTITY-ID OCCURS 98304 TIMES BINARY-CHAR.
