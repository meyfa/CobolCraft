*> --- Copybook: shared data for server.properties ---

01 SERVER-PROPERTIES EXTERNAL.
    02 SP-PORT                  PIC X(5).
    02 SP-WHITELIST-ENABLE      BINARY-CHAR UNSIGNED.
    02 SP-MOTD                  PIC X(64).

*> The number of player slots available for concurrent players.
01 MAX-PLAYERS                  BINARY-CHAR EXTERNAL.

*> The number of client slots available.
01 MAX-CLIENTS                  BINARY-CHAR EXTERNAL.
