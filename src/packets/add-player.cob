IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-AddPlayer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID        BINARY-LONG             VALUE 60.
    *> buffer used to store the packet data
    01 PAYLOAD          PIC X(1024).
    01 PAYLOADLEN       BINARY-LONG UNSIGNED.
    *> temporary data
    01 INT32            BINARY-LONG.
    01 BUFFER           PIC X(16).
    01 BUFFERLEN        BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-HNDL          PIC X(4).
    01 LK-ERRNO         PIC 9(3).
    01 LK-UUID          PIC X(16).
    01 LK-USERNAME      PIC X(16).
    01 LK-USERNAME-LEN  BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO LK-UUID LK-USERNAME LK-USERNAME-LEN.
    MOVE 0 TO PAYLOADLEN

    *> TODO: game mode, ping, display name(, skin?)

    *> actions mask=(add player + initialize chat + update listed)
    MOVE X"0B" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> number of players
    *> TODO: support sending multiple players
    MOVE 1 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> player UUID
    MOVE LK-UUID(1:16) TO PAYLOAD(PAYLOADLEN + 1:16)
    ADD 16 TO PAYLOADLEN

    *> --- action: add player ---

    *> player name
    CALL "Encode-VarInt" USING LK-USERNAME-LEN BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN
    MOVE LK-USERNAME TO PAYLOAD(PAYLOADLEN + 1:LK-USERNAME-LEN)
    ADD LK-USERNAME-LEN TO PAYLOADLEN

    *> number of properties
    MOVE 0 TO INT32
    CALL "Encode-VarInt" USING INT32 BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> properties (omitted)

    *> --- action: initialize chat ---

    *> has signature data: false - hence no other fields in this section
    MOVE X"00" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> --- action: update listed ---

    *> listed=true
    MOVE X"01" TO PAYLOAD(PAYLOADLEN + 1:1)
    ADD 1 TO PAYLOADLEN

    *> send packet
    CALL "SendPacket" USING LK-HNDL PACKET-ID PAYLOAD PAYLOADLEN LK-ERRNO
    GOBACK.

END PROGRAM SendPacket-AddPlayer.
