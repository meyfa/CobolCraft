*> --- Copybook: shared data for player state ---

*> The number of player slots available for concurrent players.
78 MAX-PLAYERS VALUE 10.

*> Player data. Once a new player is connected, their data is stored here. When they disconnect, the client is
*> set to 0, but the player data remains to be reclaimed if the same player connects again.
01 PLAYERS EXTERNAL.
    02 PLAYER OCCURS MAX-PLAYERS TIMES.
        03 PLAYER-CLIENT        BINARY-LONG UNSIGNED.
        03 PLAYER-UUID          PIC X(16).
        03 PLAYER-NAME          PIC X(16).
        03 PLAYER-NAME-LENGTH   BINARY-LONG UNSIGNED.
        03 PLAYER-POSITION.
            04 PLAYER-X             FLOAT-LONG.
            04 PLAYER-Y             FLOAT-LONG.
            04 PLAYER-Z             FLOAT-LONG.
        03 PLAYER-ROTATION.
            04 PLAYER-YAW           FLOAT-SHORT.
            04 PLAYER-PITCH         FLOAT-SHORT.
        03 PLAYER-SNEAKING      BINARY-CHAR UNSIGNED.
        03 PLAYER-FLYING        BINARY-CHAR UNSIGNED.
        03 PLAYER-HOTBAR        BINARY-CHAR UNSIGNED.
        03 PLAYER-INVENTORY.
            04 PLAYER-INVENTORY-SLOT OCCURS 46 TIMES.
                *> If no item is present, the count is 0 and the ID is 0.
                05 PLAYER-INVENTORY-SLOT-ID         BINARY-LONG.
                05 PLAYER-INVENTORY-SLOT-COUNT      BINARY-CHAR UNSIGNED.
                05 PLAYER-INVENTORY-SLOT-NBT-LENGTH BINARY-SHORT UNSIGNED.
                05 PLAYER-INVENTORY-SLOT-NBT-DATA   PIC X(1024).
