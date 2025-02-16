*> --- Copybook: shared data for player state ---

*> The maximum size of the PLAYERS table. The actual limit may be less, depending on the value set in server.properties.
78 PLAYER-CAPACITY VALUE 100.

*> Player data. Once a new player is connected, their data is stored here. When they disconnect, the client is
*> set to 0, but the player data remains to be reclaimed if the same player connects again.
01 PLAYERS EXTERNAL.
    02 PLAYER OCCURS PLAYER-CAPACITY TIMES.
        03 PLAYER-CLIENT                BINARY-LONG UNSIGNED.
        03 PLAYER-UUID                  PIC X(16).
        03 PLAYER-NAME                  PIC X(16).
        *> Survival: 0, Creative: 1, Adventure: 2, Spectator: 3
        03 PLAYER-GAMEMODE              BINARY-CHAR UNSIGNED.
        03 PLAYER-POSITION.
            04 PLAYER-X                     FLOAT-LONG.
            04 PLAYER-Y                     FLOAT-LONG.
            04 PLAYER-Z                     FLOAT-LONG.
        03 PLAYER-ROTATION.
            04 PLAYER-YAW                   FLOAT-SHORT.
            04 PLAYER-PITCH                 FLOAT-SHORT.
        03 PLAYER-HEALTH                FLOAT-SHORT.
        03 PLAYER-FOOD-LEVEL            BINARY-LONG UNSIGNED.
        03 PLAYER-SATURATION            FLOAT-SHORT.
        *> Progress on the experience bar (0.0 - 1.0); level; total experience
        03 PLAYER-XP-PROGRESS           FLOAT-SHORT.
        03 PLAYER-XP-LEVEL              BINARY-LONG.
        03 PLAYER-XP-TOTAL              BINARY-LONG.
        03 PLAYER-SNEAKING              BINARY-CHAR UNSIGNED.
        03 PLAYER-FLYING                BINARY-CHAR UNSIGNED.
        03 PLAYER-HOTBAR                BINARY-CHAR UNSIGNED.
        03 PLAYER-INVENTORY.
            04 PLAYER-INVENTORY-SLOT OCCURS 46 TIMES.
                COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==PLAYER-INVENTORY==.
        03 PLAYER-INVENTORY-STATE       BINARY-LONG.
        03 PLAYER-WINDOW.
            04 PLAYER-WINDOW-ID             BINARY-LONG.
            04 PLAYER-WINDOW-TYPE           BINARY-LONG.
            *> different windows have different states - in particular, this is different from PLAYER-INVENTORY-STATE.
            04 PLAYER-WINDOW-STATE          BINARY-LONG.
            *> window-specific slot contents; not including the regular player inventory
            04 PLAYER-WINDOW-SLOTS.
                05 PLAYER-WINDOW-SLOT OCCURS 54 TIMES.
                    COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==PLAYER-WINDOW==.
        03 PLAYER-MOUSE-ITEM.
            COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==PLAYER-MOUSE==.
        03 PLAYER-BLOCK-BREAKING.
            *> 0-9: player is breaking a block
            04 PLAYER-BLOCK-BREAKING-STAGE  BINARY-CHAR.
            04 PLAYER-BLOCK-BREAKING-POSITION.
                05 PLAYER-BLOCK-BREAKING-X      BINARY-LONG.
                05 PLAYER-BLOCK-BREAKING-Y      BINARY-LONG.
                05 PLAYER-BLOCK-BREAKING-Z      BINARY-LONG.
