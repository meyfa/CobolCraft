*> --- Copybook: NBT encoder state ---

01 NBT-ENCODER-STATE.
    *> The number of items on the stack, indicating the descension level in the NBT tree.
    02 NBT-ENCODER-LEVEL BINARY-LONG UNSIGNED.
    *> Each stack item stores the type of tag at that level (compound or list).
    *> This is used to determine whether values should contain names (for compound tags) or not (for list tags).
    02 NBT-ENCODER-STACK OCCURS 512 TIMES.
        03 NBT-ENCODER-STACK-TYPE PIC X.
