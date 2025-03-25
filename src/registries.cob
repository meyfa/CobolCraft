*> --- Registries-Parse ---
*> Parse the registries.json file. Afterwards, the parsed contents are stored in the REGISTRIES shared data structure.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Parse.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
    *> parsing
    01 OFFSET                   BINARY-LONG UNSIGNED.
    01 EXIT-LOOP                BINARY-CHAR UNSIGNED.
    01 OBJECT-KEY               PIC X(64).
    01 PROTOCOL-ID-OFFSET       BINARY-LONG UNSIGNED.
    01 CURRENT-REGISTRY-NAME    PIC X(64).
    01 CURRENT-REGISTRY-ID      BINARY-LONG.
    01 CURRENT-ENTRY-NAME       PIC X(64).
    01 CURRENT-ENTRY-ID         BINARY-LONG.
    *> validation
    01 REGISTRY-IDX             BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-JSON                  PIC X ANY LENGTH.
    01 LK-JSON-LEN              BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-JSON LK-JSON-LEN LK-FAILURE.
    INITIALIZE REGISTRIES

    MOVE 1 TO OFFSET

    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    PERFORM CheckFailure

    *> Loop over each key in the root object, each representing a registry.
    PERFORM UNTIL EXIT
        PERFORM ParseRegistry
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
        IF EXIT-LOOP NOT = 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE
    PERFORM CheckFailure

    *> There should be no gaps in the registry IDs.
    PERFORM VARYING REGISTRY-IDX FROM 1 BY 1 UNTIL REGISTRY-IDX > REGISTRY-COUNT
        IF REGISTRY-NAME(REGISTRY-IDX) = SPACES
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
    END-PERFORM

    GOBACK.

ParseRegistry.
    ADD 1 TO REGISTRY-COUNT

    CALL "JsonParse-ObjectKey" USING LK-JSON OFFSET LK-FAILURE CURRENT-REGISTRY-NAME
    PERFORM CheckFailure

    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    PERFORM CheckFailure

    *> Find the protocol_id first, as we use it to identify the registry.
    MOVE OFFSET TO PROTOCOL-ID-OFFSET
    CALL "JsonParse-FindValue" USING LK-JSON PROTOCOL-ID-OFFSET "protocol_id" LK-FAILURE
    PERFORM CheckFailure
    CALL "JsonParse-Integer" USING LK-JSON PROTOCOL-ID-OFFSET LK-FAILURE CURRENT-REGISTRY-ID
    PERFORM CheckFailure

    *> Now we have the ID, we can store the name.
    MOVE CURRENT-REGISTRY-NAME TO REGISTRY-NAME(CURRENT-REGISTRY-ID + 1)

    *> Parse the object to find the entries.
    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING LK-JSON OFFSET LK-FAILURE OBJECT-KEY
        PERFORM CheckFailure

        EVALUATE OBJECT-KEY
            WHEN "entries"
                PERFORM ParseRegistryEntries
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING LK-JSON OFFSET LK-FAILURE
                PERFORM CheckFailure
        END-EVALUATE

        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
        IF EXIT-LOOP NOT = 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE
    PERFORM CheckFailure
    .

ParseRegistryEntries.
    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    PERFORM CheckFailure

    *> Parse each entry.
    PERFORM UNTIL EXIT
        PERFORM ParseRegistryEntry
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
        IF EXIT-LOOP NOT = 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE
    PERFORM CheckFailure
    .

ParseRegistryEntry.
    ADD 1 TO REGISTRY-ENTRY-COUNT(CURRENT-REGISTRY-ID + 1)

    CALL "JsonParse-ObjectKey" USING LK-JSON OFFSET LK-FAILURE CURRENT-ENTRY-NAME
    PERFORM CheckFailure

    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    PERFORM CheckFailure

    MOVE -1 TO CURRENT-ENTRY-ID

    *> Loop over each key looking for "protocol_id", which is all we care about.
    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING LK-JSON OFFSET LK-FAILURE OBJECT-KEY
        PERFORM CheckFailure

        EVALUATE OBJECT-KEY
            WHEN "protocol_id"
                CALL "JsonParse-Integer" USING LK-JSON OFFSET LK-FAILURE CURRENT-ENTRY-ID
                PERFORM CheckFailure
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING LK-JSON OFFSET LK-FAILURE
                PERFORM CheckFailure
        END-EVALUATE

        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
        IF EXIT-LOOP NOT = 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE
    PERFORM CheckFailure

    IF CURRENT-ENTRY-ID < 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> Store the name
    MOVE CURRENT-ENTRY-NAME TO REGISTRY-ENTRY-NAME(CURRENT-REGISTRY-ID + 1, CURRENT-ENTRY-ID + 1)
    .

CheckFailure.
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF
    .

END PROGRAM Registries-Parse.

*> --- Registries-Create ---
*> Create a new, empty registry. The registry will be assigned the next available ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Create.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-NAME         PIC X ANY LENGTH.
    01 LK-PACKET-REQUIRED       BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-REGISTRY-NAME LK-PACKET-REQUIRED.
    *> We can presume that (count = max_id + 1) is always true, hence using the current count as the next ID.
    ADD 1 TO REGISTRY-COUNT
    MOVE LK-REGISTRY-NAME TO REGISTRY-NAME(REGISTRY-COUNT)
    MOVE LK-PACKET-REQUIRED TO REGISTRY-REQUIRES-PACKET(REGISTRY-COUNT)
    GOBACK.

END PROGRAM Registries-Create.

*> --- Registries-CreateEntry ---
*> Create a new entry in a registry. The entry will be assigned the next available ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-CreateEntry.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
    01 IDX                      BINARY-LONG.
LINKAGE SECTION.
    01 LK-REGISTRY-ID           BINARY-LONG.
    01 LK-ENTRY-NAME            PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-REGISTRY-ID LK-ENTRY-NAME.
    COMPUTE IDX = LK-REGISTRY-ID + 1
    ADD 1 TO REGISTRY-ENTRY-COUNT(IDX)
    MOVE LK-ENTRY-NAME TO REGISTRY-ENTRY-NAME(IDX, REGISTRY-ENTRY-COUNT(IDX))
    GOBACK.

END PROGRAM Registries-CreateEntry.

*> --- Registries-Count ---
*> Get the number of registries. Incidentally, this is also one greater than the highest registry ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Count.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-COUNT        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-REGISTRY-COUNT.
    MOVE REGISTRY-COUNT TO LK-REGISTRY-COUNT
    GOBACK.

END PROGRAM Registries-Count.

*> --- Registries-EntryCount ---
*> Get the number of entries in a registry by its ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-EntryCount.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-ID           BINARY-LONG.
    01 LK-ENTRY-COUNT           BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-REGISTRY-ID LK-ENTRY-COUNT.
    MOVE REGISTRY-ENTRY-COUNT(LK-REGISTRY-ID + 1) TO LK-ENTRY-COUNT
    GOBACK.

END PROGRAM Registries-EntryCount.

*> --- Registries-Name ---
*> Get the name of a registry by its zero-based ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Name.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-ID           BINARY-LONG.
    01 LK-REGISTRY-NAME         PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-REGISTRY-ID LK-REGISTRY-NAME.
    MOVE REGISTRY-NAME(LK-REGISTRY-ID + 1) TO LK-REGISTRY-NAME
    GOBACK.

END PROGRAM Registries-Name.

*> --- Registries-EntryName ---
*> Get the name of an entry by its zero-based ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-EntryName.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-ID           BINARY-LONG.
    01 LK-ENTRY-ID              BINARY-LONG.
    01 LK-ENTRY-NAME            PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-REGISTRY-ID LK-ENTRY-ID LK-ENTRY-NAME.
    MOVE REGISTRY-ENTRY-NAME(LK-REGISTRY-ID + 1, LK-ENTRY-ID + 1) TO LK-ENTRY-NAME
    GOBACK.

END PROGRAM Registries-EntryName.

*> --- Registries-LookupRegistry ---
*> Get the zero-based ID of a registry by its name. Negative values indicate failure.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-LookupRegistry.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
    01 IDX                      BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-REGISTRY-NAME         PIC X ANY LENGTH.
    01 LK-REGISTRY-ID           BINARY-LONG.

PROCEDURE DIVISION USING LK-REGISTRY-NAME LK-REGISTRY-ID.
    PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > REGISTRY-COUNT
        IF LK-REGISTRY-NAME = REGISTRY-NAME(IDX)
            COMPUTE LK-REGISTRY-ID = IDX - 1
            GOBACK
        END-IF
    END-PERFORM
    MOVE -1 TO LK-REGISTRY-ID
    GOBACK.

END PROGRAM Registries-LookupRegistry.

*> --- Registries-Lookup ---
*> Get the zero-based ID of an entry by its registry's name and the entry's name. Negative values indicate failure.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Lookup.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
    01 IDX-REGISTRY             BINARY-LONG UNSIGNED.
    01 IDX-ENTRY                BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-REGISTRY-NAME         PIC X ANY LENGTH.
    01 LK-ENTRY-NAME            PIC X ANY LENGTH.
    01 LK-ENTRY-ID              BINARY-LONG.

PROCEDURE DIVISION USING LK-REGISTRY-NAME LK-ENTRY-NAME LK-ENTRY-ID.
    PERFORM VARYING IDX-REGISTRY FROM 1 BY 1 UNTIL IDX-REGISTRY > REGISTRY-COUNT
        IF LK-REGISTRY-NAME = REGISTRY-NAME(IDX-REGISTRY)
            PERFORM VARYING IDX-ENTRY FROM 1 BY 1 UNTIL IDX-ENTRY > REGISTRY-ENTRY-COUNT(IDX-REGISTRY)
                IF LK-ENTRY-NAME = REGISTRY-ENTRY-NAME(IDX-REGISTRY, IDX-ENTRY)
                    COMPUTE LK-ENTRY-ID = IDX-ENTRY - 1
                    GOBACK
                END-IF
            END-PERFORM
            EXIT PERFORM
        END-IF
    END-PERFORM
    MOVE -1 TO LK-ENTRY-ID
    GOBACK.

END PROGRAM Registries-Lookup.

*> --- Registries-RequiresPacket ---
*> Get whether a registry requires a packet by its ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-RequiresPacket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-ID           BINARY-LONG.
    01 LK-REQUIRES-PACKET       BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-REGISTRY-ID LK-REQUIRES-PACKET.
    MOVE REGISTRY-REQUIRES-PACKET(LK-REGISTRY-ID + 1) TO LK-REQUIRES-PACKET
    GOBACK.

END PROGRAM Registries-RequiresPacket.
