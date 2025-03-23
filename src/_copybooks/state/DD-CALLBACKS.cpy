*> --- Copybook: shared data for callbacks ---

*> One set of callbacks per item registry ID.
01 ITEM-CALLBACKS EXTERNAL.
    02 CALLBACK OCCURS 2000 TIMES.
        03 CB-PTR-USE USAGE PROGRAM-POINTER.

*> One set of callbacks per block registry ID.
01 BLOCK-CALLBACKS EXTERNAL.
    02 CALLBACK OCCURS 2000 TIMES.
        *> Called when a block is destroyed by a player.
        03 CB-PTR-BLOCK-LOOT USAGE PROGRAM-POINTER.

*> One set of callbacks per block state ID.
01 BLOCKSTATE-CALLBACKS EXTERNAL.
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

*> One set of callbacks per block entity type.
01 BLOCK-ENTITY-CALLBACKS EXTERNAL.
    02 CALLBACK OCCURS 200 TIMES.
        *> Called to allocate memory for block entity data.
        03 CB-PTR-BLOCK-ENTITY-ALLOCATE USAGE PROGRAM-POINTER.
        *> Called to save block entity data into an NBT compound tag for storage.
        03 CB-PTR-BLOCK-ENTITY-SERIALIZE USAGE PROGRAM-POINTER.
        *> Called to load block entity data from an NBT compound tag.
        03 CB-PTR-BLOCK-ENTITY-DESERIALIZE USAGE PROGRAM-POINTER.

*> One set of callbacks per entity type.
01 ENTITY-CALLBACKS EXTERNAL.
    02 CALLBACK OCCURS 200 TIMES.
        *> Called when an entity is updated during the game tick.
        03 CB-PTR-TICK USAGE PROGRAM-POINTER.
        *> Called to save entity data into an NBT compound tag for storage.
        03 CB-PTR-SERIALIZE USAGE PROGRAM-POINTER.
        *> Called to load entity data from an NBT compound tag.
        03 CB-PTR-DESERIALIZE USAGE PROGRAM-POINTER.

*> One set of callbacks per window type (inventories, enchanting tables, etc.).
*> The player inventory does not have a window type, and hence must be stored separately.
01 WINDOW-CALLBACKS EXTERNAL.
    02 CALLBACK OCCURS 100 TIMES.
        03 CB-PTR-SYNC USAGE PROGRAM-POINTER.
        03 CB-PTR-CLOSE USAGE PROGRAM-POINTER.
        03 CB-PTR-SET-SLOT USAGE PROGRAM-POINTER.
        03 CB-PTR-DROP USAGE PROGRAM-POINTER.
    02 PLAYER-INVENTORY-CALLBACKS.
        03 CB-PTR-SYNC-PLAYER USAGE PROGRAM-POINTER.
        03 CB-PTR-CLOSE-PLAYER USAGE PROGRAM-POINTER.
        03 CB-PTR-SET-SLOT-PLAYER USAGE PROGRAM-POINTER.
        03 CB-PTR-DROP-PLAYER USAGE PROGRAM-POINTER.
