IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-SetEntityMetadata.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID    BINARY-LONG             VALUE H'58'.
    *> buffer used to store the packet data
    01 PAYLOAD      PIC X(1024).
    01 PAYLOADLEN   BINARY-LONG UNSIGNED.
    *> temporary data
    01 BUFFER           PIC X(8).
    01 BUFFERLEN        BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-HNDL          PIC X(4).
    01 LK-ERRNO         PIC 9(3).
    01 LK-ENTITY-ID     BINARY-LONG.
    *> Metadata is very much dependent on the entity implementation in Java, so we don't make an attempt at
    *> giving it a meaningful structure here.
    01 LK-METADATA-LEN  BINARY-LONG UNSIGNED.
    01 LK-METADATA-DATA PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO LK-ENTITY-ID LK-METADATA-LEN LK-METADATA-DATA.
    MOVE 0 TO PAYLOADLEN

    *> entity ID
    CALL "Encode-VarInt" USING LK-ENTITY-ID BUFFER BUFFERLEN
    MOVE BUFFER(1:BUFFERLEN) TO PAYLOAD(PAYLOADLEN + 1:BUFFERLEN)
    ADD BUFFERLEN TO PAYLOADLEN

    *> metadata
    MOVE LK-METADATA-DATA(1:LK-METADATA-LEN) TO PAYLOAD(PAYLOADLEN + 1:LK-METADATA-LEN)
    ADD LK-METADATA-LEN TO PAYLOADLEN

    *> send packet
    CALL "SendPacket" USING LK-HNDL PACKET-ID PAYLOAD PAYLOADLEN LK-ERRNO
    GOBACK.

END PROGRAM SendPacket-SetEntityMetadata.
