*> --- Copybook: reference to a loaded region or entity data file ---
*> This is a BASED item, i.e., a view into a data structure referenced by a pointer. DD-REGION-FILES.cpy contains a
*> table of pointers to these structures. The same structure is used for both region and entity files.

*> The region file cache.
01 REGION-FILE BASED.
    *> When the region file was last accessed.
    02 REGION-FILE-ACCESS-TIME      BINARY-LONG-LONG.
    *> Region coordinates.
    02 REGION-FILE-X                BINARY-LONG.
    02 REGION-FILE-Z                BINARY-LONG.
    *> The file name.
    02 REGION-FILE-NAME             PIC X(1024).
    *> The open file handle (input/output).
    02 REGION-FILE-HANDLE           PIC X(4) USAGE COMP-X.
    *> Chunk locations within the file, consisting of the offset (3 big-endian bytes) and length (1 byte).
    *> Offset and length are measured in 4 kiB sectors.
    02 REGION-FILE-OFFSET-BYTES     PIC X(REGION-SECTOR-BYTES).
    02 REGION-FILE-OFFSETS          REDEFINES REGION-FILE-OFFSET-BYTES.
        03 REGION-FILE-OFFSET OCCURS REGION-SECTOR-INTS TIMES PIC X(4).
    *> TODO: Last-modified timestamps for each chunk
    *> Boolean array; one entry per file sector, indicating whether the sector is used.
    *> TODO: Make this use less memory (bitset?)
    02 REGION-FILE-USED OCCURS REGION-SECTOR-COUNT TIMES BINARY-CHAR UNSIGNED.
