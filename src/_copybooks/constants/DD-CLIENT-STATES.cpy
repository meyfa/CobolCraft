*> --- Copybook: constants for client connection states ---
*> These constants are used by the official server and client. For instance, during handshake, the client informs
*> the server whether to move into state 1 (= status) or state 2 (= login).

78 CLIENT-STATE-DISCONNECTED        VALUE -1.
78 CLIENT-STATE-HANDSHAKE           VALUE 0.
78 CLIENT-STATE-STATUS              VALUE 1.
78 CLIENT-STATE-LOGIN               VALUE 2.
78 CLIENT-STATE-CONFIGURATION       VALUE 3.
78 CLIENT-STATE-PLAY                VALUE 4.
