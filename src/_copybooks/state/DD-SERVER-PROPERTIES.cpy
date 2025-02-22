*> --- Copybook: shared data for server.properties ---

01 SERVER-PROPERTIES EXTERNAL.
    02 SP-PORT                  BINARY-SHORT UNSIGNED.
    02 SP-LEVEL-NAME            PIC X(255).
    02 SP-WHITELIST-ENABLE      BINARY-CHAR UNSIGNED.
    02 SP-MOTD                  PIC X(64).

*> The number of player slots available for concurrent players.
01 MAX-PLAYERS                  BINARY-LONG UNSIGNED EXTERNAL.

*> The number of client slots available.
01 MAX-CLIENTS                  BINARY-LONG UNSIGNED EXTERNAL.

*> The server sends (2 * VIEW-DISTANCE + 1) * (2 * VIEW-DISTANCE + 1) chunks around the player.
*> TODO: Improve performance so this can be increased to a reasonable value.
01 VIEW-DISTANCE                BINARY-LONG             VALUE 3.
