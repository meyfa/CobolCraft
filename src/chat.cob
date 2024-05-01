*> --- BroadcastChatMessage ---
*> Send a chat message to all clients in the "play" state, and log it to the server console.
IDENTIFICATION DIVISION.
PROGRAM-ID. BroadcastChatMessage.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CLIENT-ID                BINARY-LONG UNSIGNED        VALUE 0.
LINKAGE SECTION.
    01 LK-MESSAGE               PIC X ANY LENGTH.
    01 LK-MESSAGE-LENGTH        BINARY-LONG UNSIGNED.
    01 LK-COLOR                 PIC X(16).

PROCEDURE DIVISION USING LK-MESSAGE LK-MESSAGE-LENGTH LK-COLOR.
    CALL "BroadcastChatMessageExcept" USING CLIENT-ID LK-MESSAGE LK-MESSAGE-LENGTH LK-COLOR
    GOBACK.

END PROGRAM BroadcastChatMessage.

*> --- BroadcastChatMessageExcept ---
*> Send a chat message to all clients in the "play" state, except for a specific client, and log the message to the
*> server console. A client ID of 0 means to broadcast to all clients.
IDENTIFICATION DIVISION.
PROGRAM-ID. BroadcastChatMessageExcept.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENT-STATES.
    COPY DD-CLIENTS.
    01 CLIENT-ID                BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-EXCEPT-CLIENT-ID      BINARY-LONG UNSIGNED.
    01 LK-MESSAGE               PIC X ANY LENGTH.
    01 LK-MESSAGE-LENGTH        BINARY-LONG UNSIGNED.
    01 LK-COLOR                 PIC X(16).

PROCEDURE DIVISION USING LK-EXCEPT-CLIENT-ID LK-MESSAGE LK-MESSAGE-LENGTH LK-COLOR.
    DISPLAY LK-MESSAGE(1:LK-MESSAGE-LENGTH)

    PERFORM VARYING CLIENT-ID FROM 1 BY 1 UNTIL CLIENT-ID > MAX-CLIENTS
        IF CLIENT-ID NOT = LK-EXCEPT-CLIENT-ID AND CLIENT-PRESENT(CLIENT-ID) = 1 AND CLIENT-STATE(CLIENT-ID) = CLIENT-STATE-PLAY
            CALL "SendPacket-SystemChat" USING CLIENT-ID LK-MESSAGE LK-MESSAGE-LENGTH LK-COLOR
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM BroadcastChatMessageExcept.

