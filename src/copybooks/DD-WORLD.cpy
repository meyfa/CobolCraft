*> --- Copybook: shared data for the world state ---

*> Age of the world in ticks.
01 WORLD-AGE                BINARY-LONG-LONG EXTERNAL.
01 WORLD-TIME               BINARY-LONG-LONG EXTERNAL.

*> Chunk storage
78 WORLD-CHUNK-COUNT        VALUE 255.
*> 16x384x16 blocks per chunk
78 WORLD-CHUNK-BLOCK-COUNT  VALUE 98304.
01 WORLD-CHUNKS EXTERNAL.
    02 WORLD-CHUNK OCCURS WORLD-CHUNK-COUNT TIMES.
        03 WORLD-CHUNK-PRESENT      BINARY-CHAR UNSIGNED.
        03 WORLD-CHUNK-DIRTY        BINARY-CHAR UNSIGNED.
        03 WORLD-CHUNK-X            BINARY-LONG.
        03 WORLD-CHUNK-Z            BINARY-LONG.
        *> block IDs - X increases fastest, then Z, then Y
        03 WORLD-CHUNK-BLOCKS.
            04 WORLD-BLOCK OCCURS WORLD-CHUNK-BLOCK-COUNT TIMES.
                05 WORLD-BLOCK-ID BINARY-LONG UNSIGNED.
