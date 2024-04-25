*> --- Copybook: chunk file format ---

*> Each record in the file is a chunk section. This is done because GnuCOBOL has a very restrictive maximum
*> record length. Properties that apply to the chunk as a whole (e.g., the chunk position) are stored in the
*> first record.
01 CHUNK-FILE-RECORD.

    *> --- Data for the chunk as a whole ---

    *> Position of the chunk
    02 CHUNK-X                  BINARY-LONG.
    02 CHUNK-Z                  BINARY-LONG.
    *> The lowest section number (i.e., -4 if the lowest block is at Y=-64)
    02 CHUNK-Y                  BINARY-LONG.
    *> The number of populated sections; other sections are assumed to be air
    02 CHUNK-SECTION-COUNT      BINARY-LONG UNSIGNED.

    *> --- Data for a single chunk section ---

    *> Y position of the section (CHUNK-Y for the first section, CHUNK-Y+1 for the second, etc.)
    02 CHUNK-SECTION-Y                BINARY-LONG.
    *> Blocks; these are indices into the block palette
    02 CHUNK-SECTION-DATA BINARY-SHORT UNSIGNED OCCURS 4096 TIMES.
    *> Block palette for the section. This needs to be last since it is variable-length.
    02 CHUNK-PALETTE-LENGTH     BINARY-LONG UNSIGNED.
    02 CHUNK-PALETTE-ENTRY OCCURS 0 TO 4096 TIMES DEPENDING ON CHUNK-PALETTE-LENGTH.
        *> Name of the block, e.g., "minecraft:bamboo_stairs".
        03 PALETTE-NAME             PIC X(64).
        *> Properties of the block, e.g., "facing": "north", "half": "top", "shape": "straight", "waterlogged": "false".
        03 PALETTE-PROPERTY-COUNT   BINARY-LONG UNSIGNED.
        03 PALETTE-PROPERTY OCCURS 16 TIMES.
            04 PALETTE-PROPERTY-NAME    PIC X(64).
            04 PALETTE-PROPERTY-VALUE   PIC X(64).
