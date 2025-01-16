IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-AddPlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET REPLACING IDENTIFIER BY "play/clientbound/minecraft:player_info_update".
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(1024).
    01 PAYLOADPOS       BINARY-LONG UNSIGNED.
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 INT32            BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT        BINARY-LONG UNSIGNED.
    01 LK-UUID          PIC X(16).
    01 LK-USERNAME      PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-CLIENT LK-UUID LK-USERNAME.
    COPY PROC-PACKET-INIT.

    MOVE 1 TO PAYLOADPOS

    *> TODO: game mode, ping, display name(, skin?)

    *> actions mask=(add player + initialize chat + update listed)
    MOVE X"0B" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> number of players
    *> TODO: support sending multiple players
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> player UUID
    MOVE LK-UUID(1:16) TO PAYLOAD(PAYLOADPOS:16)
    ADD 16 TO PAYLOADPOS

    *> --- action: add player ---

    *> player name
    MOVE FUNCTION STORED-CHAR-LENGTH(LK-USERNAME) TO INT32
    CALL "Encode-String" USING LK-USERNAME INT32 PAYLOAD PAYLOADPOS

    *> number of properties
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 PAYLOAD PAYLOADPOS

    *> properties (omitted)

    *> --- action: initialize chat ---

    *> has signature data: false - hence no other fields in this section
    MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> --- action: update listed ---

    *> listed=true
    MOVE X"01" TO PAYLOAD(PAYLOADPOS:1)
    ADD 1 TO PAYLOADPOS

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-AddPlayer.
