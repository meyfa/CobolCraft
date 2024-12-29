*> --- Copybook: shared data for client state ---

*> Maximum packet length is 2^21-1 bytes ~= 2.1 MiB,
*> see: https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Protocol#Packet_format
78 RECEIVE-BUFFER-LENGTH        VALUE 2100000.

*> The maximum number of chunks that can be queued for a client.
78 CHUNK-QUEUE-LENGTH           VALUE 100.

*> Client data
01 CLIENTS EXTERNAL.
    02 CLIENT OCCURS 100 TIMES.
        03 CLIENT-PRESENT       BINARY-CHAR.
        03 CLIENT-HNDL          PIC X(4).
        *> Any error that occured during a send operation. If non-zero, the client is disconnected on the next tick.
        03 CLIENT-ERRNO-SEND    PIC 9(3).
        *> State of the player (see DD-CLIENT-STATES for possible values)
        03 CLIENT-STATE         BINARY-CHAR.
        03 CONFIG-FINISH        BINARY-CHAR.
        *> The index of the associated player, or 0 if login has not been started
        03 CLIENT-PLAYER        BINARY-CHAR.
        *> Last keepalive ID sent and received
        03 KEEPALIVE-SENT       BINARY-LONG-LONG.
        03 KEEPALIVE-RECV       BINARY-LONG-LONG.
        *> Last teleport ID sent and received. Until the client acknowledges the teleport, any movement packets it sends
        *> are ignored.
        03 TELEPORT-SENT        BINARY-LONG-LONG.
        03 TELEPORT-RECV        BINARY-LONG-LONG.
        *> Last sent center of the loaded chunk area. Once the player moves across a chunk border, the server sends new
        *> chunks around the player.
        03 CENTER-CHUNK-X       BINARY-LONG.
        03 CENTER-CHUNK-Z       BINARY-LONG.
        *> Queue of chunks that need to be sent. Implemented as a ring buffer.
        *> Note: To simplify the implementation, these indices are zero-based!
        03 CHUNK-QUEUE-BEGIN    BINARY-LONG.
        03 CHUNK-QUEUE-END      BINARY-LONG.
        03 CHUNK-QUEUE OCCURS CHUNK-QUEUE-LENGTH TIMES.
            04 CHUNK-QUEUE-X        BINARY-LONG.
            04 CHUNK-QUEUE-Z        BINARY-LONG.
        *> Packet reading: expected packet length (0 if not yet known), packet buffer, amount of received bytes
        03 PACKET-LENGTH        BINARY-LONG.
        *> Pointer to the client's receive buffer. Must be ALLOCATE'd when the client connects and FREE'd on disconnect.
        03 PACKET-BUFFER        POINTER.
        03 PACKET-BUFFERLEN     BINARY-LONG.

*> The receive buffer for the currently selected client (its PACKET-BUFFER).
01 CLIENT-RECEIVE-BUFFER PIC X(RECEIVE-BUFFER-LENGTH) BASED.
