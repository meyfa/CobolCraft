*> --- Copybook: NBT decoder state ---

01 NBT-DECODER-STATE.
    *> The number of items on the stack, indicating the descension level in the NBT tree.
    02 NBT-DECODER-LEVEL BINARY-LONG UNSIGNED.
    *> Each stack item stores the type of tag at that level (compound or list).
    *> This is used to determine whether values contain names (for compound tags) or not (for list tags).
    02 NBT-DECODER-STACK OCCURS 512 TIMES.
        03 NBT-DECODER-STACK-TYPE PIC X.
