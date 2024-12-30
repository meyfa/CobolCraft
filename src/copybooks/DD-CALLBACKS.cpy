*> --- Copybook: shared data for callbacks ---

*> One set of callbacks per item protocol ID.
01 ITEM-CALLBACKS EXTERNAL.
    02 CALLBACK OCCURS 2000 TIMES.
        03 CB-PTR-USE USAGE PROGRAM-POINTER.

*> One set of callbacks per block protocol ID.
01 BLOCK-CALLBACKS EXTERNAL.
    02 CALLBACK OCCURS 30000 TIMES.
        *> Called when a player breaks a block.
        03 CB-PTR-DESTROY USAGE PROGRAM-POINTER.
        *> Called when a player interacts with a block (by right-clicking).
        03 CB-PTR-INTERACT USAGE PROGRAM-POINTER.
        *> Called for determining the shape of a block face, such as for checking if a block face is solid.
        03 CB-PTR-FACE USAGE PROGRAM-POINTER.
        *> Called for determining whether a block state can be replaced by another block state.
        03 CB-PTR-REPLACEABLE USAGE PROGRAM-POINTER.
        *> Called to determine the item form of a block, such as for the "pick block" action (middle-click).
        03 CB-PTR-BLOCK-ITEM USAGE PROGRAM-POINTER.
