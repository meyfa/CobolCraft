*> --- Copybook: callback parameters for dropping the item in a window-specific slot ---

01 LK-PLAYER                BINARY-LONG.
01 LK-INDEX                 BINARY-SHORT.
*> whether to drop the entire stack, vs. just one item
01 LK-STACK                 BINARY-CHAR UNSIGNED.
*> whether dropping from the slot affected other slots (e.g. crafting output)
01 LK-SYNC-REQUIRED         BINARY-CHAR UNSIGNED.
