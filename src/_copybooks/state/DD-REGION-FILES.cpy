*> --- Copybook: shared data for region files ---
*> A region file contains up to 32x32 chunks. It has a header storing offsets into the file for each chunk.
*> To make saving/loading chunks from region files efficient, we need to cache some data about region files.

*> Region file constants
78 REGION-SECTOR-BYTES          VALUE 4096.
78 REGION-SECTOR-INTS           VALUE 1024.
78 REGION-SECTOR-COUNT          VALUE 1048576.
78 REGION-CHUNK-WIDTH           VALUE 32.

*> The maximum number of region files that can be open at once.
*> Should always be greater than the number of players or thrashing will occur if players are spread out.
78 MAX-REGION-FILES             VALUE 16.

*> The region file cache.
01 REGION-FILES EXTERNAL.
    02 REGION-FILE OCCURS MAX-REGION-FILES TIMES.
        *> Whether the region file is currently open.
        03 REGION-FILE-OPEN             BINARY-CHAR UNSIGNED.
        *> When the region file was last accessed.
        03 REGION-FILE-ACCESS-TIME      BINARY-LONG-LONG.
        *> Region coordinates.
        03 REGION-FILE-X                BINARY-LONG.
        03 REGION-FILE-Z                BINARY-LONG.
        *> The file name.
        03 REGION-FILE-NAME             PIC X(1024).
        *> The open file handle (input/output).
        03 REGION-FILE-HANDLE           PIC X(4) USAGE COMP-X.
        *> Chunk locations within the file, consisting of the offset (3 big-endian bytes) and length (1 byte).
        *> Offset and length are measured in 4 kiB sectors.
        03 REGION-FILE-OFFSET-BYTES     PIC X(REGION-SECTOR-BYTES).
        03 REGION-FILE-OFFSETS          REDEFINES REGION-FILE-OFFSET-BYTES.
            04 REGION-FILE-OFFSET OCCURS REGION-SECTOR-INTS TIMES PIC X(4).
        *> TODO: Last-modified timestamps for each chunk
        *> Boolean array; one entry per file sector, indicating whether the sector is used.
        *> TODO: Make this use less memory (bitset?)
        03 REGION-FILE-USED OCCURS REGION-SECTOR-COUNT TIMES BINARY-CHAR UNSIGNED.
