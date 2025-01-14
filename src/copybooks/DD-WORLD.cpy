*> --- Copybook: shared data for the world state ---

*> Age of the world in ticks.
01 WORLD-AGE                BINARY-LONG-LONG EXTERNAL.
01 WORLD-TIME               BINARY-LONG-LONG EXTERNAL.

*> World spawn point.
01 WORLD-SPAWN-X            BINARY-LONG EXTERNAL.
01 WORLD-SPAWN-Y            BINARY-LONG EXTERNAL.
01 WORLD-SPAWN-Z            BINARY-LONG EXTERNAL.

*> Whether the world is in hardcore mode.
01 WORLD-HARDCORE           BINARY-CHAR UNSIGNED EXTERNAL.

*> Chunk storage
78 WORLD-CHUNK-COUNT            VALUE 255.
78 WORLD-SECTION-COUNT          VALUE 24.
01 WORLD-CHUNKS EXTERNAL.
    02 WORLD-CHUNK OCCURS WORLD-CHUNK-COUNT TIMES.
        03 WORLD-CHUNK-PRESENT      BINARY-CHAR UNSIGNED.
        03 WORLD-CHUNK-DIRTY        BINARY-CHAR UNSIGNED.
        03 WORLD-CHUNK-X            BINARY-LONG.
        03 WORLD-CHUNK-Z            BINARY-LONG.
        03 WORLD-SECTION OCCURS WORLD-SECTION-COUNT TIMES.
            04 WORLD-SECTION-NON-AIR    BINARY-LONG UNSIGNED.
            *> block IDs (16x16x16) - X increases fastest, then Z, then Y
            04 WORLD-SECTION-BLOCKS.
                05 WORLD-BLOCK OCCURS 4096 TIMES.
                    06 WORLD-BLOCK-ID           BINARY-LONG UNSIGNED.
            *> biome IDs (4x4x4)
            04 WORLD-SECTION-BIOMES.
                05 WORLD-BIOME OCCURS 64 TIMES.
                    06 WORLD-BIOME-ID           BINARY-LONG UNSIGNED.
        03 WORLD-BLOCK-ENTITY-COUNT BINARY-LONG UNSIGNED.
        *> block entity IDs for each block
        03 WORLD-BLOCK-ENTITIES.
            *> set to a value < 0 to indicate no entity (since 0 is a valid ID)
            *> TODO: support storing entity data, not just IDs
            04 WORLD-BLOCK-ENTITY-ID OCCURS 98304 TIMES BINARY-CHAR.
