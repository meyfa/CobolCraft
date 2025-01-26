*> --- Copybook: callback parameters for setting the item in a window-specific slot ---

01 LK-PLAYER                BINARY-LONG.
01 LK-INDEX                 BINARY-SHORT.
01 LK-SLOT.
    COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
