*> --- Copybook: shared data for the world state ---

78 WORLD-CHUNK-CAPACITY     VALUE 65536.

*> Age of the world in ticks.
01 WORLD-AGE                BINARY-LONG-LONG EXTERNAL.
01 WORLD-TIME               BINARY-LONG-LONG EXTERNAL.

*> World spawn point.
01 WORLD-SPAWN-X            BINARY-LONG EXTERNAL.
01 WORLD-SPAWN-Y            BINARY-LONG EXTERNAL.
01 WORLD-SPAWN-Z            BINARY-LONG EXTERNAL.

*> Whether the world is in hardcore mode.
01 WORLD-HARDCORE           BINARY-CHAR UNSIGNED EXTERNAL.

*> Pointers to loaded chunks. See DD-CHUNK-REF.cpy for the structure pointed to.
01 WORLD-CHUNK-POINTERS EXTERNAL.
    *> Number of loaded chunks
    02 WORLD-CHUNK-COUNT        BINARY-LONG UNSIGNED.
    02 WORLD-CHUNK-POINTER      USAGE POINTER OCCURS WORLD-CHUNK-CAPACITY TIMES.
