*> --- Copybook: shared data for callbacks ---

*> One set of callbacks per item protocol ID.
01 ITEM-CALLBACKS EXTERNAL.
    02 CALLBACK OCCURS 2000 TIMES.
        03 CB-PTR-USE USAGE PROGRAM-POINTER.

*> One set of callbacks per block protocol ID.
01 BLOCK-CALLBACKS EXTERNAL.
    02 CALLBACK OCCURS 30000 TIMES.
        03 CB-PTR-DESTROY USAGE PROGRAM-POINTER.
        03 CB-PTR-INTERACT USAGE PROGRAM-POINTER.
