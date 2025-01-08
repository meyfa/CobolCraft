IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-PluginMessage.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> packet id depending on connection state
    COPY DD-PACKET REPLACING LEADING ==PACKET-== BY ==CONFIGURATION-PACKET-==,
        IDENTIFIER BY "configuration/clientbound/minecraft:custom_payload".
    COPY DD-PACKET REPLACING LEADING ==PACKET-== BY ==PLAY-PACKET-==,
        IDENTIFIER BY "play/clientbound/minecraft:custom_payload".
    01 PACKET-ID                BINARY-LONG.
    *> shared state
    COPY DD-CLIENT-STATES.
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(64000).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
    *> temporary
    01 CHANNEL-LEN              BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-STATE                 BINARY-CHAR.
    01 LK-CHANNEL               PIC X ANY LENGTH.
    01 LK-DATA-LEN              BINARY-LONG UNSIGNED.
    01 LK-DATA                  PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-CLIENT LK-STATE LK-CHANNEL LK-DATA-LEN LK-DATA.
    COPY PROC-PACKET-INIT REPLACING LEADING ==PACKET-== BY ==CONFIGURATION-PACKET-==.
    COPY PROC-PACKET-INIT REPLACING LEADING ==PACKET-== BY ==PLAY-PACKET-==.

    MOVE 1 TO PAYLOADPOS

    EVALUATE LK-STATE
        WHEN CLIENT-STATE-CONFIGURATION
            MOVE CONFIGURATION-PACKET-ID TO PACKET-ID
        WHEN CLIENT-STATE-PLAY
            MOVE PLAY-PACKET-ID TO PACKET-ID
        WHEN OTHER
            DISPLAY "Invalid state for PluginMessage packet: " LK-STATE
            GOBACK
    END-EVALUATE

    *> channel identifier
    MOVE FUNCTION STORED-CHAR-LENGTH(LK-CHANNEL) TO CHANNEL-LEN
    CALL "Encode-String" USING LK-CHANNEL CHANNEL-LEN PAYLOAD PAYLOADPOS

    *> data
    MOVE LK-DATA(1:LK-DATA-LEN) TO PAYLOAD(PAYLOADPOS:LK-DATA-LEN)
    ADD LK-DATA-LEN TO PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-PluginMessage.
