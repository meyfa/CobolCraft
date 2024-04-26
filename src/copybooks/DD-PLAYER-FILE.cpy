*> --- Copybook: player file format ---

01 FILE-PLAYER.
    02 FILE-PLAYER-UUID                     PIC X(16).
    02 FILE-PLAYER-NAME                     PIC X(16).
    02 FILE-PLAYER-POSITION.
        03 FILE-PLAYER-X                            FLOAT-LONG.
        03 FILE-PLAYER-Y                            FLOAT-LONG.
        03 FILE-PLAYER-Z                            FLOAT-LONG.
    02 FILE-PLAYER-ROTATION.
        03 FILE-PLAYER-YAW                          FLOAT-SHORT.
        03 FILE-PLAYER-PITCH                        FLOAT-SHORT.
    02 FILE-PLAYER-HOTBAR                       BINARY-CHAR UNSIGNED.
    02 FILE-PLAYER-INVENTORY.
        03 FILE-PLAYER-INVENTORY-SLOT OCCURS 46 TIMES.
            *> If no item is present, the count is 0 and the ID is 0.
            04 FILE-PLAYER-INVENTORY-SLOT-ID            PIC X(255).
            04 FILE-PLAYER-INVENTORY-SLOT-COUNT         BINARY-CHAR UNSIGNED.
            04 FILE-PLAYER-INVENTORY-SLOT-NBT-LENGTH    BINARY-SHORT UNSIGNED.
            04 FILE-PLAYER-INVENTORY-SLOT-NBT-DATA      PIC X(1024).
