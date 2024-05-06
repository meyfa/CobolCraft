IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-PluginMessage.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENT-STATES.
    *> packet id depending on connection state
    78 PACKET-ID-CONFIGURATION  VALUE H'01'.
    78 PACKET-ID-PLAY           VALUE H'19'.
    01 PACKET-ID                BINARY-LONG.
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(64000).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
    *> temporary
    01 CHANNEL-LEN              BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-STATE                 BINARY-CHAR.
    01 LK-CHANNEL               PIC X ANY LENGTH.
    01 LK-DATA-LEN              BINARY-LONG UNSIGNED.
    01 LK-DATA                  PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-CLIENT LK-STATE LK-CHANNEL LK-DATA-LEN LK-DATA.
    EVALUATE LK-STATE
        WHEN CLIENT-STATE-CONFIGURATION
            MOVE PACKET-ID-CONFIGURATION TO PACKET-ID
        WHEN CLIENT-STATE-PLAY
            MOVE PACKET-ID-PLAY TO PACKET-ID
        WHEN OTHER
            DISPLAY "Invalid state for PluginMessage packet: " LK-STATE
            GOBACK
    END-EVALUATE

    MOVE 1 TO PAYLOADPOS

    *> channel length
    MOVE FUNCTION STORED-CHAR-LENGTH(LK-CHANNEL) TO CHANNEL-LEN
    CALL "Encode-VarInt" USING CHANNEL-LEN PAYLOAD PAYLOADPOS

    *> channel
    MOVE LK-CHANNEL(1:CHANNEL-LEN) TO PAYLOAD(PAYLOADPOS:CHANNEL-LEN)
    ADD CHANNEL-LEN TO PAYLOADPOS

    *> data
    MOVE LK-DATA(1:LK-DATA-LEN) TO PAYLOAD(PAYLOADPOS:LK-DATA-LEN)
    ADD LK-DATA-LEN TO PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-PluginMessage.
