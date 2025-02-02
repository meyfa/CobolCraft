*> --- SetCenterChunk ---
*> Set up the center chunk for a client.
IDENTIFICATION DIVISION.
PROGRAM-ID. SetCenterChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    01 PLAYER-ID                BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT.
    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID
    DIVIDE PLAYER-X(PLAYER-ID) BY 16 GIVING CENTER-CHUNK-X(LK-CLIENT) ROUNDED MODE IS TOWARD-LESSER
    DIVIDE PLAYER-Z(PLAYER-ID) BY 16 GIVING CENTER-CHUNK-Z(LK-CLIENT) ROUNDED MODE IS TOWARD-LESSER
    CALL "SendPacket-SetCenterChunk" USING LK-CLIENT CENTER-CHUNK-X(LK-CLIENT) CENTER-CHUNK-Z(LK-CLIENT)
    GOBACK.

END PROGRAM SetCenterChunk.

*> --- EnqueueSurroundingChunks ---
*> Enqueue chunks surrounding the player. This is done in such a way that pre-chunks are sent first.
IDENTIFICATION DIVISION.
PROGRAM-ID. EnqueueSurroundingChunks.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-SERVER-PROPERTIES.
    01 X-START                  BINARY-LONG.
    01 X-END                    BINARY-LONG.
    01 Z-START                  BINARY-LONG.
    01 Z-END                    BINARY-LONG.
    01 X-POS                    BINARY-LONG.
    01 Z-POS                    BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT.
    *> enqueue 3x3 chunks around the player first
    MOVE 0 TO CHUNK-QUEUE-BEGIN(LK-CLIENT)
    MOVE 0 TO CHUNK-QUEUE-END(LK-CLIENT)
    COMPUTE X-START = CENTER-CHUNK-X(LK-CLIENT) - 1
    COMPUTE X-END = CENTER-CHUNK-X(LK-CLIENT) + 1
    COMPUTE Z-START = CENTER-CHUNK-Z(LK-CLIENT) - 1
    COMPUTE Z-END = CENTER-CHUNK-Z(LK-CLIENT) + 1
    PERFORM VARYING X-POS FROM X-START BY 1 UNTIL X-POS > X-END
        PERFORM VARYING Z-POS FROM Z-START BY 1 UNTIL Z-POS > Z-END
            CALL "EnqueueChunk" USING LK-CLIENT X-POS Z-POS
        END-PERFORM
    END-PERFORM

    *> now enqueue all chunks in the view distance
    COMPUTE X-START = CENTER-CHUNK-X(LK-CLIENT) - VIEW-DISTANCE
    COMPUTE X-END = CENTER-CHUNK-X(LK-CLIENT) + VIEW-DISTANCE
    COMPUTE Z-START = CENTER-CHUNK-Z(LK-CLIENT) - VIEW-DISTANCE
    COMPUTE Z-END = CENTER-CHUNK-Z(LK-CLIENT) + VIEW-DISTANCE
    PERFORM VARYING X-POS FROM X-START BY 1 UNTIL X-POS > X-END
        PERFORM VARYING Z-POS FROM Z-START BY 1 UNTIL Z-POS > Z-END
            *> Note: EnqueueChunk will automatically skip duplicates
            CALL "EnqueueChunk" USING LK-CLIENT X-POS Z-POS
        END-PERFORM
    END-PERFORM

    GOBACK.

END PROGRAM EnqueueSurroundingChunks.

*> --- SendPreChunks ---
*> Send pre-chunks to a client. This is the area immediately around the player and is required to complete the login.
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPreChunks.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT.
    PERFORM 9 TIMES
        CALL "ProcessChunkQueue" USING LK-CLIENT
    END-PERFORM
    GOBACK.

END PROGRAM SendPreChunks.

*> --- ProcessClientChunks ---
*> Send chunks to clients, update chunk queues, and unload chunks.
IDENTIFICATION DIVISION.
PROGRAM-ID. ProcessClientChunks.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-CLIENT-STATES.
    COPY DD-SERVER-PROPERTIES.
    01 CLIENT-ID                BINARY-LONG UNSIGNED.
    01 UNLOAD-FAILURE           BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION.
    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
            CALL "SendChunks" USING CLIENT-ID
        END-IF
    END-PERFORM
    CALL "World-UnloadChunks" USING VIEW-DISTANCE UNLOAD-FAILURE
    IF UNLOAD-FAILURE NOT = 0
        DISPLAY "Failure unloading chunks"
        STOP RUN RETURNING 1
    END-IF
    GOBACK.

END PROGRAM ProcessClientChunks.

*> --- SendChunks ---
*> Send chunks to a client.
IDENTIFICATION DIVISION.
PROGRAM-ID. SendChunks.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    COPY DD-SERVER-PROPERTIES.
    01 PLAYER-ID                BINARY-LONG UNSIGNED.
    01 X-START                  BINARY-LONG.
    01 X-END                    BINARY-LONG.
    01 Z-START                  BINARY-LONG.
    01 Z-END                    BINARY-LONG.
    01 X-POS                    BINARY-LONG.
    01 Z-POS                    BINARY-LONG.
    01 PREV-CENTER-CHUNK-X      BINARY-LONG.
    01 PREV-CENTER-CHUNK-Z      BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT.
    *> send up to 1 chunk to the client per tick
    CALL "ProcessChunkQueue" USING LK-CLIENT
    IF CLIENT-PRESENT(LK-CLIENT) = 0
        *> The client disconnected while processing the queue
        GOBACK
    END-IF

    MOVE CLIENT-PLAYER(LK-CLIENT) TO PLAYER-ID

    *> compute the new center chunk position
    MOVE CENTER-CHUNK-X(LK-CLIENT) TO PREV-CENTER-CHUNK-X
    MOVE CENTER-CHUNK-Z(LK-CLIENT) TO PREV-CENTER-CHUNK-Z
    DIVIDE PLAYER-X(PLAYER-ID) BY 16 GIVING CENTER-CHUNK-X(LK-CLIENT) ROUNDED MODE IS TOWARD-LESSER
    DIVIDE PLAYER-Z(PLAYER-ID) BY 16 GIVING CENTER-CHUNK-Z(LK-CLIENT) ROUNDED MODE IS TOWARD-LESSER

    IF CENTER-CHUNK-X(LK-CLIENT) NOT = PREV-CENTER-CHUNK-X OR CENTER-CHUNK-Z(LK-CLIENT) NOT = PREV-CENTER-CHUNK-Z
        *> send center chunk position
        CALL "SendPacket-SetCenterChunk" USING LK-CLIENT CENTER-CHUNK-X(LK-CLIENT) CENTER-CHUNK-Z(LK-CLIENT)

        *> compute chunk area that came into view
        *> TODO: make this code look better

        *> first: parallel to the X axis
        COMPUTE X-START = CENTER-CHUNK-X(LK-CLIENT) - VIEW-DISTANCE
        COMPUTE X-END = CENTER-CHUNK-X(LK-CLIENT) + VIEW-DISTANCE
        IF CENTER-CHUNK-Z(LK-CLIENT) < PREV-CENTER-CHUNK-Z
            COMPUTE Z-START = CENTER-CHUNK-Z(LK-CLIENT) - VIEW-DISTANCE
            COMPUTE Z-END = FUNCTION MIN(PREV-CENTER-CHUNK-Z - VIEW-DISTANCE, CENTER-CHUNK-Z(LK-CLIENT) + VIEW-DISTANCE)
        ELSE
            COMPUTE Z-START = FUNCTION MAX(PREV-CENTER-CHUNK-Z + VIEW-DISTANCE, CENTER-CHUNK-Z(LK-CLIENT) - VIEW-DISTANCE)
            COMPUTE Z-END = CENTER-CHUNK-Z(LK-CLIENT) + VIEW-DISTANCE
        END-IF
        PERFORM VARYING X-POS FROM X-START BY 1 UNTIL X-POS > X-END
            PERFORM VARYING Z-POS FROM Z-START BY 1 UNTIL Z-POS > Z-END
                CALL "EnqueueChunk" USING LK-CLIENT X-POS Z-POS
            END-PERFORM
        END-PERFORM

        *> second: parallel to the Z axis
        COMPUTE Z-START = CENTER-CHUNK-Z(LK-CLIENT) - VIEW-DISTANCE
        COMPUTE Z-END = CENTER-CHUNK-Z(LK-CLIENT) + VIEW-DISTANCE
        IF CENTER-CHUNK-X(LK-CLIENT) < PREV-CENTER-CHUNK-X
            COMPUTE X-START = CENTER-CHUNK-X(LK-CLIENT) - VIEW-DISTANCE
            COMPUTE X-END = FUNCTION MIN(PREV-CENTER-CHUNK-X - VIEW-DISTANCE, CENTER-CHUNK-X(LK-CLIENT) + VIEW-DISTANCE)
        ELSE
            COMPUTE X-START = FUNCTION MAX(PREV-CENTER-CHUNK-X + VIEW-DISTANCE, CENTER-CHUNK-X(LK-CLIENT) - VIEW-DISTANCE)
            COMPUTE X-END = CENTER-CHUNK-X(LK-CLIENT) + VIEW-DISTANCE
        END-IF
        PERFORM VARYING X-POS FROM X-START BY 1 UNTIL X-POS > X-END
            PERFORM VARYING Z-POS FROM Z-START BY 1 UNTIL Z-POS > Z-END
                CALL "EnqueueChunk" USING LK-CLIENT X-POS Z-POS
            END-PERFORM
        END-PERFORM
    END-IF

    GOBACK.

END PROGRAM SendChunks.

*> --- EnqueueChunk ---
*> Enqueue a chunk to be sent to a client.
IDENTIFICATION DIVISION.
PROGRAM-ID. EnqueueChunk.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    01 QUEUE-INDEX              BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-CHUNK-X               BINARY-LONG UNSIGNED.
    01 LK-CHUNK-Z               BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-CHUNK-X LK-CHUNK-Z.
    *> Overflow would occur if (end + 1) % length == begin
    COMPUTE QUEUE-INDEX = CHUNK-QUEUE-END(LK-CLIENT) + 1
    COMPUTE QUEUE-INDEX = FUNCTION MOD(QUEUE-INDEX, CHUNK-QUEUE-LENGTH)
    IF QUEUE-INDEX = CHUNK-QUEUE-BEGIN(LK-CLIENT)
        DISPLAY "[client=" LK-CLIENT "] Chunk queue overflow!"
        GOBACK
    END-IF

    *> Check for duplicates
    MOVE CHUNK-QUEUE-BEGIN(LK-CLIENT) TO QUEUE-INDEX
    PERFORM UNTIL QUEUE-INDEX = CHUNK-QUEUE-END(LK-CLIENT)
        IF CHUNK-QUEUE-X(LK-CLIENT, QUEUE-INDEX + 1) = LK-CHUNK-X AND CHUNK-QUEUE-Z(LK-CLIENT, QUEUE-INDEX + 1) = LK-CHUNK-Z
            GOBACK
        END-IF
        ADD 1 TO QUEUE-INDEX
        IF QUEUE-INDEX >= CHUNK-QUEUE-LENGTH
            MOVE 0 TO QUEUE-INDEX
        END-IF
    END-PERFORM

    *> Insert the chunk at the current end position
    MOVE LK-CHUNK-X TO CHUNK-QUEUE-X(LK-CLIENT, CHUNK-QUEUE-END(LK-CLIENT) + 1)
    MOVE LK-CHUNK-Z TO CHUNK-QUEUE-Z(LK-CLIENT, CHUNK-QUEUE-END(LK-CLIENT) + 1)

    *> Move the end pointer one beyond the new item
    ADD 1 TO CHUNK-QUEUE-END(LK-CLIENT)
    IF CHUNK-QUEUE-END(LK-CLIENT) >= CHUNK-QUEUE-LENGTH
        MOVE 0 TO CHUNK-QUEUE-END(LK-CLIENT)
    END-IF

    GOBACK.

END PROGRAM EnqueueChunk.

*> --- ProcessChunkQueue ---
*> Process the chunk queue and send chunks to a client.
IDENTIFICATION DIVISION.
PROGRAM-ID. ProcessChunkQueue.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
    COPY DD-CLIENTS.
    COPY DD-SERVER-PROPERTIES.
    01 CHUNK-INDEX              BINARY-LONG UNSIGNED.
    01 X-START                  BINARY-LONG.
    01 X-END                    BINARY-LONG.
    01 Z-START                  BINARY-LONG.
    01 Z-END                    BINARY-LONG.
    01 X-POS                    BINARY-LONG.
    01 Z-POS                    BINARY-LONG.
    COPY DD-CHUNK-REF.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT.
    *> Determine the client's view area to avoid sending chunks outside of it
    COMPUTE X-START = CENTER-CHUNK-X(LK-CLIENT) - VIEW-DISTANCE
    COMPUTE X-END = CENTER-CHUNK-X(LK-CLIENT) + VIEW-DISTANCE
    COMPUTE Z-START = CENTER-CHUNK-Z(LK-CLIENT) - VIEW-DISTANCE
    COMPUTE Z-END = CENTER-CHUNK-Z(LK-CLIENT) + VIEW-DISTANCE

    *> Since end points one beyond the last item, the queue is empty once begin = end.
    PERFORM UNTIL CHUNK-QUEUE-BEGIN(LK-CLIENT) = CHUNK-QUEUE-END(LK-CLIENT)
        *> Dequeue the next chunk
        MOVE CHUNK-QUEUE-X(LK-CLIENT, CHUNK-QUEUE-BEGIN(LK-CLIENT) + 1) TO X-POS
        MOVE CHUNK-QUEUE-Z(LK-CLIENT, CHUNK-QUEUE-BEGIN(LK-CLIENT) + 1) TO Z-POS
        ADD 1 TO CHUNK-QUEUE-BEGIN(LK-CLIENT)
        IF CHUNK-QUEUE-BEGIN(LK-CLIENT) >= CHUNK-QUEUE-LENGTH
            MOVE 0 TO CHUNK-QUEUE-BEGIN(LK-CLIENT)
        END-IF
        *> Check if the chunk is within the client's view area
        IF X-POS >= X-START AND X-POS <= X-END AND Z-POS >= Z-START AND Z-POS <= Z-END
            CALL "World-EnsureChunk" USING X-POS Z-POS CHUNK-INDEX
            IF CHUNK-INDEX > 0
                SET ADDRESS OF CHUNK TO WORLD-CHUNK-POINTER(CHUNK-INDEX)
                CALL "SendPacket-ChunkData" USING LK-CLIENT CHUNK-X CHUNK-Z CHUNK-SECTIONS CHUNK-BLOCK-ENTITIES
            END-IF
            *> Stop once a chunk has been sent
            EXIT PERFORM
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM ProcessChunkQueue.
