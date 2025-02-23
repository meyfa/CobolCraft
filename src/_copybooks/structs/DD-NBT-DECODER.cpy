*> --- Copybook: NBT decoder state ---

01 NBTDEC.
    *> The position within the input buffer (starting at 1).
    02 NBTDEC-OFFSET BINARY-LONG UNSIGNED.
    *> The number of items on the stack, indicating the descension level in the NBT tree.
    02 NBTDEC-LEVEL BINARY-LONG UNSIGNED.
    *> Each stack item stores the type of container at that level (compound/list/byte array/int array/long array).
    *> This is used to determine whether values are prefixed with tag IDs (for compounds and lists, but not arrays),
    *> and whether they are named (only for compounds).
    02 NBTDEC-STACK OCCURS 512 TIMES.
        03 NBTDEC-STACK-TYPE PIC X.
        *> Type and number of items in the list.
        03 NBTDEC-STACK-LIST-TYPE PIC X.
        03 NBTDEC-STACK-LIST-COUNT BINARY-LONG.
