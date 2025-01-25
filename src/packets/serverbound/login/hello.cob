IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-Hello.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    COPY DD-PLAYERS.
    COPY DD-CLIENT-STATES.
    COPY DD-SERVER-PROPERTIES.
    *> payload
    01 TEMP-PLAYER-NAME         PIC X(16).
    01 TEMP-PLAYER-NAME-LEN     BINARY-LONG UNSIGNED.
    01 TEMP-UUID                PIC X(16).
    *> processing
    01 WHITELISTED              BINARY-CHAR UNSIGNED.
    01 EXISTING-PLAYER-ID       BINARY-LONG.
    01 EXISTING-CLIENT          BINARY-LONG.
    01 PLAYER-ID                BINARY-LONG.
    01 BUFFER                   PIC X(255).
    01 BUFFERLEN                BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    *> Decode username
    MOVE SPACES TO TEMP-PLAYER-NAME
    CALL "Decode-String" USING LK-BUFFER LK-OFFSET TEMP-PLAYER-NAME-LEN TEMP-PLAYER-NAME

    *> Ignore UUID and generate our own
    ADD 16 TO LK-OFFSET
    CALL "Players-NameToUUID" USING TEMP-PLAYER-NAME TEMP-UUID

    *> Check username against the whitelist
    IF SP-WHITELIST-ENABLE NOT = 0
        CALL "Whitelist-Check" USING TEMP-UUID TEMP-PLAYER-NAME WHITELISTED
        IF WHITELISTED = 0
            MOVE "You are not white-listed on this server!" TO BUFFER
            MOVE 40 TO BUFFERLEN
            DISPLAY "Disconnecting " FUNCTION TRIM(TEMP-PLAYER-NAME) ": " BUFFER(1:BUFFERLEN)
            CALL "SendPacket-Disconnect" USING LK-CLIENT CLIENT-STATE(LK-CLIENT) BUFFER BUFFERLEN
            CALL "Server-DisconnectClient" USING LK-CLIENT
            GOBACK
        END-IF
    END-IF

    *> If the player is already connected, disconnect them first
    CALL "Players-FindConnectedByUUID" USING TEMP-UUID EXISTING-PLAYER-ID
    IF EXISTING-PLAYER-ID > 0
        MOVE PLAYER-CLIENT(EXISTING-PLAYER-ID) TO EXISTING-CLIENT
        MOVE "You logged in from another location" TO BUFFER
        MOVE 35 TO BUFFERLEN
        DISPLAY "Disconnecting " FUNCTION TRIM(PLAYER-NAME(EXISTING-PLAYER-ID)) ": " BUFFER(1:BUFFERLEN)
        CALL "SendPacket-Disconnect" USING EXISTING-CLIENT CLIENT-STATE(EXISTING-CLIENT) BUFFER BUFFERLEN
        CALL "Server-DisconnectClient" USING EXISTING-CLIENT
    END-IF

    *> Add/replace the player in the player list
    CALL "Players-Connect" USING LK-CLIENT TEMP-UUID TEMP-PLAYER-NAME PLAYER-ID
    IF PLAYER-ID = 0
        MOVE "The server is full" TO BUFFER
        MOVE 18 TO BUFFERLEN
        DISPLAY "Disconnecting " FUNCTION TRIM(TEMP-PLAYER-NAME) ": " BUFFER(1:BUFFERLEN)
        CALL "SendPacket-Disconnect" USING LK-CLIENT CLIENT-STATE(LK-CLIENT) BUFFER BUFFERLEN
        CALL "Server-DisconnectClient" USING LK-CLIENT
        GOBACK
    END-IF

    MOVE PLAYER-ID TO CLIENT-PLAYER(LK-CLIENT)

    *> Send login success. This should result in a "login acknowledged" packet by the client.
    CALL "SendPacket-LoginSuccess" USING LK-CLIENT PLAYER-UUID(PLAYER-ID) PLAYER-NAME(PLAYER-ID)

    GOBACK.

END PROGRAM RecvPacket-Hello.
