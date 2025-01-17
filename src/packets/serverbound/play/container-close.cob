IDENTIFICATION DIVISION.
PROGRAM-ID. RecvPacket-ContainerClose.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-BUFFER LK-OFFSET.
    *> TODO implement containers
    *> TODO handle carried item (mouse item) - transfer to inventory when closing
    GOBACK.

END PROGRAM RecvPacket-ContainerClose.
