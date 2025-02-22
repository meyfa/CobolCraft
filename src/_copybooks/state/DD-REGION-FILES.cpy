*> --- Copybook: shared data for region and entity data files (collectively: Anvil file format) ---
*> A region/entity data file contains up to 32x32 chunks. It has a header storing offsets into the file for each chunk.
*> To make saving/loading chunks from region/entity data files efficient, we need to cache some data about each file.

*> Region file constants
78 REGION-SECTOR-BYTES          VALUE 4096.
78 REGION-SECTOR-INTS           VALUE 1024.
78 REGION-SECTOR-COUNT          VALUE 1048576.
78 REGION-CHUNK-WIDTH           VALUE 32.

*> File types
78 FILE-TYPE-REGION             VALUE 0.
78 FILE-TYPE-ENTITY             VALUE 1.

*> The maximum number of region/entity data files that can be open at once.
78 MAX-REGION-FILES             VALUE 1024.

*> The region file cache.
01 REGION-FILE-POINTERS EXTERNAL.
    *> Number of loaded region files
    02 REGION-FILE-COUNT        BINARY-LONG UNSIGNED.
    *> Pointers to dynamically allocated structures, see DD-REGION-FILE-REF.cpy.
    *> The items will be reordered such that all open files are at the beginning of the table.
    02 REGION-FILE-ITEM         OCCURS MAX-REGION-FILES TIMES.
        *> Either FILE-TYPE-REGION or FILE-TYPE-ENTITY
        03 REGION-FILE-TYPE     BINARY-CHAR UNSIGNED.
        03 REGION-FILE-POINTER  USAGE POINTER.
