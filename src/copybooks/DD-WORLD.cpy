*> --- Copybook: shared data for the world state ---

*> Age of the world in ticks. This modulo 24000 is the current time of day.
01 WORLD-AGE               BINARY-LONG-LONG EXTERNAL.

*> Chunk storage
01 WORLD-CHUNKS            EXTERNAL.
    02 WORLD-CHUNKS-COUNT-X BINARY-LONG.
    02 WORLD-CHUNKS-COUNT-Z BINARY-LONG.
    02 WORLD-CHUNK OCCURS 49 TIMES.
        03 WORLD-CHUNK-X BINARY-LONG.
        03 WORLD-CHUNK-Z BINARY-LONG.
        *> block IDs (16x384x16) - X increases fastest, then Z, then Y
        03 WORLD-CHUNK-BLOCKS.
            04 WORLD-BLOCK OCCURS 98304 TIMES.
                05 WORLD-BLOCK-ID BINARY-LONG UNSIGNED.
