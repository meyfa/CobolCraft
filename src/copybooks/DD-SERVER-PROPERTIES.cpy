*> --- Copybook: shared data for server.properties ---

01 SERVER-PROPERTIES EXTERNAL.
    02 SP-PORT                  BINARY-SHORT UNSIGNED.
    02 SP-WHITELIST-ENABLE      BINARY-CHAR UNSIGNED.
    02 SP-MOTD                  PIC X(64).

*> The number of player slots available for concurrent players.
01 MAX-PLAYERS                  BINARY-LONG UNSIGNED EXTERNAL.

*> The number of client slots available.
01 MAX-CLIENTS                  BINARY-LONG UNSIGNED EXTERNAL.
