IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket-Registry.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 PACKET-ID                BINARY-LONG             VALUE H'07'.
    *> buffer used to store the packet data
    01 PAYLOAD                  PIC X(60000).
    01 PAYLOADPOS               BINARY-LONG UNSIGNED.
    01 PAYLOADLEN               BINARY-LONG UNSIGNED.
    *> temporary data
    01 REGISTRY-NAME            PIC X(255).
    01 REGISTRY-ENTRY-COUNT     BINARY-LONG UNSIGNED.
    01 REGISTRY-ENTRY-INDEX     BINARY-LONG UNSIGNED.
    01 REGISTRY-ENTRY-NAME      PIC X(255).
    01 INT32                    BINARY-LONG.
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-REGISTRY-INDEX        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-REGISTRY-INDEX.
    MOVE 1 TO PAYLOADPOS

    *> registry name
    CALL "Registries-Iterate-Name" USING LK-REGISTRY-INDEX REGISTRY-NAME
    MOVE FUNCTION STORED-CHAR-LENGTH(REGISTRY-NAME) TO INT32
    CALL "Encode-String" USING REGISTRY-NAME INT32 PAYLOAD PAYLOADPOS

    *> entry count
    CALL "Registries-GetRegistryLength" USING LK-REGISTRY-INDEX REGISTRY-ENTRY-COUNT
    CALL "Encode-VarInt" USING REGISTRY-ENTRY-COUNT PAYLOAD PAYLOADPOS

    *> entries
    PERFORM VARYING REGISTRY-ENTRY-INDEX FROM 1 BY 1 UNTIL REGISTRY-ENTRY-INDEX > REGISTRY-ENTRY-COUNT
        *> TODO: This assumes that entries are sorted by protocol ID. This is the case for all registries that we care
        *> about with this packet, but shouldn't be relied upon.
        CALL "Registries-Iterate-EntryName" USING LK-REGISTRY-INDEX REGISTRY-ENTRY-INDEX REGISTRY-ENTRY-NAME
        MOVE FUNCTION STORED-CHAR-LENGTH(REGISTRY-ENTRY-NAME) TO INT32
        CALL "Encode-String" USING REGISTRY-ENTRY-NAME INT32 PAYLOAD PAYLOADPOS

        *> has data (always false)
        MOVE X"00" TO PAYLOAD(PAYLOADPOS:1)
        ADD 1 TO PAYLOADPOS
    END-PERFORM

    *> send packet
    COMPUTE PAYLOADLEN = PAYLOADPOS - 1
    CALL "SendPacket" USING LK-CLIENT PACKET-ID PAYLOAD PAYLOADLEN
    GOBACK.

END PROGRAM SendPacket-Registry.
