*> --- Copybook: NBT decoder state ---

01 NBT-DECODER-STATE.
    *> The position within the input buffer (starting at 1).
    02 NBT-DECODER-OFFSET BINARY-LONG UNSIGNED.
    *> The number of items on the stack, indicating the descension level in the NBT tree.
    02 NBT-DECODER-LEVEL BINARY-LONG UNSIGNED.
    *> Each stack item stores the type of container at that level (compound/list/byte array/int array/long array).
    *> This is used to determine whether values are prefixed with tag IDs (for compounds and lists, but not arrays),
    *> and whether they are named (only for compounds).
    02 NBT-DECODER-STACK OCCURS 512 TIMES.
        03 NBT-DECODER-STACK-TYPE PIC X.
        *> Type and number of items in the list.
        03 NBT-DECODER-STACK-LIST-TYPE PIC X.
        03 NBT-DECODER-STACK-LIST-COUNT BINARY-LONG.
