*> --- CG-LoadRegistries ---
*> Parse the registries.json file. Afterwards, the parsed contents are stored in the REGISTRIES shared data structure.
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-LoadRegistries.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGISTRIES.
    *> input
    01 JSONBUF                  PIC X(1000000).
    01 JSONLEN                  BINARY-LONG UNSIGNED.
    *> parsing
    01 FAILURE                  BINARY-CHAR UNSIGNED.
    01 JSONPOS                  BINARY-LONG UNSIGNED.
    01 EXIT-LOOP                BINARY-CHAR UNSIGNED.
    01 OBJECT-KEY               PIC X(64).
    01 JSONPOS-PROTOCOL-ID      BINARY-LONG UNSIGNED.
    01 CURRENT-REGISTRY-NAME    PIC X(64).
    01 CURRENT-REGISTRY-ID      BINARY-LONG.
    01 CURRENT-ENTRY-NAME       PIC X(64).
    01 CURRENT-ENTRY-ID         BINARY-LONG.
    *> validation
    01 REGISTRY-IDX             BINARY-LONG UNSIGNED.
    01 ENTRY-IDX                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    INITIALIZE REGISTRIES

    CALL "Codegen-ReadDataFile" USING "generated/reports/registries.json" JSONBUF JSONLEN
    MOVE 1 TO JSONPOS

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    *> Loop over each key in the root object, each representing a registry.
    PERFORM UNTIL EXIT
        PERFORM ParseRegistry
        CALL "JsonParse-Comma" USING JSONBUF JSONPOS EXIT-LOOP
        IF EXIT-LOOP NOT = 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    *> There should be no gaps in the registry IDs.
    PERFORM VARYING REGISTRY-IDX FROM 1 BY 1 UNTIL REGISTRY-IDX > REGISTRY-COUNT
        COPY ASSERT REPLACING COND BY ==REGISTRY-NAME(REGISTRY-IDX) NOT = SPACES==,
            MSG BY =="CG-LoadRegistries: Missing registry name at index " REGISTRY-IDX==.

        PERFORM VARYING ENTRY-IDX FROM 1 BY 1 UNTIL ENTRY-IDX > REGISTRY-ENTRY-COUNT(REGISTRY-IDX)
            COPY ASSERT REPLACING COND BY ==REGISTRY-ENTRY-NAME(REGISTRY-IDX, ENTRY-IDX) NOT = SPACES==,
                MSG BY =="CG-LoadRegistries: Missing entry name for registry " FUNCTION TRIM(REGISTRY-NAME(REGISTRY-IDX)) " at index " ENTRY-IDX==.
        END-PERFORM
    END-PERFORM

    GOBACK.

ParseRegistry.
    ADD 1 TO REGISTRY-COUNT

    CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE CURRENT-REGISTRY-NAME
    PERFORM AssertOk

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    *> Find the protocol_id first, as we use it to identify the registry.
    MOVE JSONPOS TO JSONPOS-PROTOCOL-ID
    CALL "JsonParse-FindValue" USING JSONBUF JSONPOS-PROTOCOL-ID "protocol_id" FAILURE
    PERFORM AssertOk
    CALL "JsonParse-Integer" USING JSONBUF JSONPOS-PROTOCOL-ID FAILURE CURRENT-REGISTRY-ID
    PERFORM AssertOk

    *> Now we have the ID, we can store the name.
    MOVE CURRENT-REGISTRY-NAME TO REGISTRY-NAME(CURRENT-REGISTRY-ID + 1)

    *> Parse the object to find the entries.
    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE OBJECT-KEY
        PERFORM AssertOk

        EVALUATE OBJECT-KEY
            WHEN "entries"
                PERFORM ParseRegistryEntries
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS EXIT-LOOP
        IF EXIT-LOOP NOT = 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    .

ParseRegistryEntries.
    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    *> Parse each entry.
    PERFORM UNTIL EXIT
        PERFORM ParseRegistryEntry
        CALL "JsonParse-Comma" USING JSONBUF JSONPOS EXIT-LOOP
        IF EXIT-LOOP NOT = 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    .

ParseRegistryEntry.
    ADD 1 TO REGISTRY-ENTRY-COUNT(CURRENT-REGISTRY-ID + 1)

    CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE CURRENT-ENTRY-NAME
    PERFORM AssertOk

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    MOVE -1 TO CURRENT-ENTRY-ID

    *> Loop over each key looking for "protocol_id", which is all we care about.
    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE OBJECT-KEY
        PERFORM AssertOk

        EVALUATE OBJECT-KEY
            WHEN "protocol_id"
                CALL "JsonParse-Integer" USING JSONBUF JSONPOS FAILURE CURRENT-ENTRY-ID
                PERFORM AssertOk
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS EXIT-LOOP
        IF EXIT-LOOP NOT = 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    COPY ASSERT REPLACING COND BY ==CURRENT-ENTRY-ID >= 0==, MSG BY =="CG-LoadRegistries: Missing entry protocol_id"==.

    *> Store the name
    MOVE CURRENT-ENTRY-NAME TO REGISTRY-ENTRY-NAME(CURRENT-REGISTRY-ID + 1, CURRENT-ENTRY-ID + 1)
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==, MSG BY =="CG-LoadRegistries: Failed to parse JSON"==.
    .

END PROGRAM CG-LoadRegistries.
