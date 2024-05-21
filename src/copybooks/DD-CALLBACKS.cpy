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
