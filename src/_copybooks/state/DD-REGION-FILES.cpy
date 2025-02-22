*> --- Copybook: shared data for region files ---
*> A region file contains up to 32x32 chunks. It has a header storing offsets into the file for each chunk.
*> To make saving/loading chunks from region files efficient, we need to cache some data about region files.

*> Region file constants
78 REGION-SECTOR-BYTES          VALUE 4096.
78 REGION-SECTOR-INTS           VALUE 1024.
78 REGION-SECTOR-COUNT          VALUE 1048576.
78 REGION-CHUNK-WIDTH           VALUE 32.

*> The maximum number of region files that can be open at once.
78 MAX-REGION-FILES             VALUE 1024.

*> The region file cache.
01 REGION-FILE-POINTERS EXTERNAL.
    *> Number of loaded region files
    02 REGION-FILE-COUNT        BINARY-LONG UNSIGNED.
    *> Pointers to dynamically allocated structures, see DD-REGION-FILE-REF.cpy.
    *> The items will be reordered such that all open files are at the beginning of the table.
    02 REGION-FILE-POINTER      USAGE POINTER OCCURS MAX-REGION-FILES TIMES.
