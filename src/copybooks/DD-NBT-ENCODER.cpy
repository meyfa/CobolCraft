*> --- Copybook: NBT encoder state ---

01 NBT-ENCODER-STATE.
    *> The number of items on the stack, indicating the descension level in the NBT tree.
    02 NBT-ENCODER-LEVEL BINARY-LONG UNSIGNED.
    *> Each stack item stores the type of tag at that level (compound or list).
    *> This is used to determine whether values should contain names (for compound tags) or not (for list tags).
    02 NBT-ENCODER-STACK OCCURS 512 TIMES.
        03 NBT-ENCODER-STACK-TYPE PIC X.
        *> The index of the data structure in the output buffer.
        03 NBT-ENCODER-STACK-INDEX BINARY-LONG UNSIGNED.
        *> Number and type of items in the list. This is written once the list is ended.
        03 NBT-ENCODER-STACK-LIST-TYPE PIC X.
        03 NBT-ENCODER-STACK-LIST-COUNT BINARY-LONG.
