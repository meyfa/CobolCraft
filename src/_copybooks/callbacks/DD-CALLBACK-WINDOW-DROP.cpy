*> --- Copybook: callback parameters for dropping the item in a window-specific slot ---

01 LK-PLAYER                BINARY-LONG.
01 LK-INDEX                 BINARY-SHORT.
*> whether to drop the entire stack, vs. just one item
01 LK-STACK                 BINARY-CHAR UNSIGNED.
*> the total number of slots that have changed
01 LK-CHANGES               BINARY-LONG UNSIGNED.
